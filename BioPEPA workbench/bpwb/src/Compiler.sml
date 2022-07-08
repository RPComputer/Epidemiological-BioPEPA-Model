(* 
 File: Compiler.sml

 The compiler from PEPA nets to PEPA
*)
structure Compiler :> Compiler = 
struct
  local  open PepaNets  in

  type cellRecord = { token : Identifier, place : Identifier, marked : bool }
  val cells = ref [] : cellRecord list ref
  type tokenRecord = { token : Identifier, place : Identifier }
  val tokens = ref [] : tokenRecord list ref
  val statics = ref [] : Identifier list ref
  val activities = ref [] : Identifier list ref
  val transitionNames = ref [] : Identifier list ref
  val rates = ref [] : Identifier list ref
  val synchronisationSets = ref [] : Identifier list ref

  type environment = (Identifier * Component) list

  fun lookup I ([] : environment) = 
      Error.internal_error ("undeclared identifier: " ^ HashTable.unhash I)
    | lookup I ((Id, P) :: t) = if I = Id then P else (lookup I t)

  fun lookupArcs a I [] = []
    | lookupArcs a I ((P, (alpha, rate), Q) :: t) = 
      if I = P andalso a = alpha 
      then Q :: lookupArcs a I t
      else lookupArcs a I t

  fun member x [] = false
    | member x (h::t) = x = h orelse member x t

  fun intersect [] l = []
    | intersect (h::t) l = 
       let val remainder = intersect t l
        in if member h l then h :: remainder else remainder
       end

  fun product (h::t, l) = List.map (fn x => (h, x)) l @ product (t, l)
    | product _ = []

  fun sanitiseComponentId id = 
      let val id' = HashTable.unhash id
          fun primeToSuffix #"'" = "__prime"
            | primeToSuffix c = str c
          val id'' = String.translate primeToSuffix id'
          val id''' = HashTable.hash id''
       in id'''
      end

  fun specialize Id Place = HashTable.hash (specialize' Id Place)
  and specialize' Id Place = 
      let val Id' = HashTable.unhash (sanitiseComponentId Id) 
          val Place' = HashTable.unhash Place
       in Id' ^ "__at__" ^ Place'
      end
  and specializeToken { token, place } = specialize token place
  and specializeIndividual Id Place (VAR Continuation) = 
         HashTable.hash (specializeIndividual' Id Place Continuation)
    | specializeIndividual Id Place Continuation = 
         Error.internal_error ("compiling: specializing requires an identifier for" ^
			       Prettyprinter.print Continuation)
  and specializeIndividual' Id Place Continuation = 
      let val Id' = HashTable.unhash (sanitiseComponentId Id) 
          val Place' = HashTable.unhash Place
          val Continuation' = HashTable.unhash Continuation
       in Id' ^ "__at__" ^ Place' ^ "__becoming__" ^ Continuation'
      end

  fun componentNames [] = []
    | componentNames ((I, ARC _) :: t) = (* ignore arcs *) componentNames t
    | componentNames ((I, COOP _) :: t) = (* ignore places *) componentNames t
    | componentNames ((I, HIDING _) :: t) = (* ignore places *) componentNames t
    | componentNames ((I, _) :: t) = I :: componentNames t

  fun placeNames [] = []
    | placeNames ((VAR I) :: t) = I :: placeNames t
    | placeNames (_ :: t) = placeNames t

  fun union ([], l) = l
    | union (h::t, l) = insert (h, union (t, l))
  and insert (x, []) = [x]
    | insert (x, l) = if member (x, l) then l else x :: l
  and member (h, []) = false
    | member (h, h1::t1) = h = h1 orelse member (h, t1)
  and minus ([], _) = []
    | minus (h::t, l) = if member (h, l) then minus(t, l) else 
                                           h::minus(t, l)

  fun sanitiseRateId r = 
      let val r' = HashTable.unhash r
          fun starToTimes #"*" = "__times__"
            | starToTimes c = str c
          val r'' = String.translate starToTimes r'
          val r''' = HashTable.hash r''
       in r'''
      end
  fun sanitiseRate Top = Top
    | sanitiseRate (Rate r) = Rate (sanitiseRateId r) 
    | sanitiseRate (Product (i, r)) = Product (i, sanitiseRate r)
    | sanitiseRate (Literal_rate s) = Literal_rate s
    | sanitiseRate (Binary_rate_op (r1, s, r2)) = Binary_rate_op (sanitiseRate r1, s,
								  sanitiseRate r2)
    | sanitiseRate (Rate_fun_apply (s, args))   = Rate_fun_apply (s, List.map sanitiseRate args)

  fun ratePart Top = HashTable.hash "infty"
    | ratePart (Rate r) = r
    | ratePart (Product (i, r))   = HashTable.hash "rate___dummy"
    | ratePart (Literal_rate _)   = HashTable.hash "rate___dummy"
    | ratePart (Binary_rate_op _) = HashTable.hash "rate___dummy"
    | ratePart (Rate_fun_apply _) = HashTable.hash "rate___dummy"

  fun generateToken (Env: environment) Arcs ( token, place ) = 
        let
          val tokenTag = VAR (HashTable.hash "")
          val tokenBody = lookup token Env
          val tokenBody' = generateBody Env Arcs place tokenBody
        in
          CONSTS (specialize token place, tokenBody', tokenTag)
        end
  and generateBody Env Arcs Place (VAR I) = VAR (specialize I Place)
    | generateBody Env Arcs Place (PREFIX ((alpha, rate), P)) =
        (case (lookupArcs alpha Place Arcs) of 
           [] => (* a local transition, but is it shared or individual? *)
                 let val alpha' = 
			 if member (alpha, !synchronisationSets)
			 then specialize alpha Place (* shared, so don't rename *)
			 else specializeIndividual alpha Place P (* individual *)
                     val rate' = sanitiseRate rate
                 in
                     activities := insert(alpha', !activities);
		     rates := insert(ratePart rate', !rates);
                     PREFIX ((alpha', rate'), generateBody Env Arcs Place P)
		 end
         | Q =>  (* a global firing *) 
                 generateFiring Env Arcs (PREFIX ((alpha, rate), P)) Q
        )
    | generateBody Env Arcs Place (CHOICE (P, Q)) = 
         let
            val P' = generateBody Env Arcs Place P
            val Q' = generateBody Env Arcs Place Q
         in CHOICE (P', Q')
         end
    | generateBody Env Arcs Place P = 
         Error.internal_error ("Compiler: generating body for " ^ 
			       Prettyprinter.print P)
  and generateFiring Env Arcs (PREFIX ((alpha, rate), P)) [] = 
         Error.internal_error ("Compiler: generating firing")
    | generateFiring Env Arcs (PREFIX ((alpha, rate), P)) [Q] =
         let val alpha' = fireTo alpha Q P
             val rate' = sanitiseRate rate
          in
             activities := insert(alpha', !activities);
	     rates := insert(ratePart rate', !rates);
             PREFIX ((alpha', rate'), generateBody Env Arcs Q P)
         end
    | generateFiring Env Arcs P (h::t) = 
         CHOICE (generateFiring Env Arcs P [h],
                 generateFiring Env Arcs P t)
    | generateFiring Env Arcs P _ = 
         Error.internal_error ("Compiler: generating firing for " ^ 
			       Prettyprinter.print P)
  and fireTo Id Place (VAR Continuation) = HashTable.hash (fireTo' Id Place Continuation)
    | fireTo Id Place Continuation = 
         Error.internal_error ("compiling: should have an identifier for" ^
			       Prettyprinter.print Continuation)
  and fireTo' Id Place Continuation = 
         HashTable.unhash Id ^ 
              "__to__" ^ HashTable.unhash Place ^ 
              "__becoming__" ^ HashTable.unhash Continuation

  fun promote Env [] = Error.internal_error ("Compiler: generating promotion")
    | promote Env [P] = P
    | promote Env [P,Q] = 
         let val actP = Alphabets.alphabetOf Env P
             val actQ = Alphabets.alphabetOf Env Q
          in COOP([P, Q], intersect actP actQ)
         end
    | promote Env (P::t) = 
         let val actP = Alphabets.alphabetOf Env P
             val Q = promote Env t
             val actQ = Alphabets.alphabetOf Env Q
          in COOP([P, Q], intersect actP actQ)
         end

  local 
    val rateIndex = ref 0
  in
    fun fakeRate r = 
        let
          val rateTag = VAR (HashTable.hash "")
          val _ = rateIndex := (!rateIndex + 1) mod 1000
	  val rate = real (Primes.nth (!rateIndex))
          val fakeRateValue = VAR (HashTable.hash (Real.toString (rate)))
        in
          CONSTS (r, fakeRateValue, rateTag)
        end
    fun removeInfty [] = []
      | removeInfty (h::t) = if HashTable.unhash h = "infty" then t else h :: removeInfty t
  end

  val null = VAR (HashTable.hash "")
  val Environment = HashTable.hash "Environment__PEPANET"
  fun passivePrefix a = PREFIX ((a, Top), VAR Environment)
  fun mkEnvironment [] = Error.internal_error "Compiling: empty environment"
    | mkEnvironment [a] = CONSTS (Environment, passivePrefix a, null)
    | mkEnvironment (h::t) = 
         (case mkEnvironment t of
             CONSTS (_, continuation, _) =>
                 CONSTS (Environment, CHOICE (passivePrefix h, continuation), null)
         | _ => Error.internal_error "Compiling: making environment")

  fun getSynchronisationSets (CONSTS (I, P, L)) =
      (getSynchronisationSets P; getSynchronisationSets L)
    | getSynchronisationSets (COOP(P, L)) = 
      (map getSynchronisationSets P;
       synchronisationSets := union(!synchronisationSets, L))
    | getSynchronisationSets _ = ()

  fun getEnvironment (CONSTS (I, P, C)) = (I, P) :: getEnvironment C
    | getEnvironment _ = []

  fun getEnvironments [] = [] : PepaNets.Environment
    | getEnvironments (h::t) = getEnvironment h @ getEnvironments t

  fun list (banner, lstfn) = 
      if !Options.listing then 
          let val lst = lstfn()
	      fun nl s = "\t" ^ s ^ "\n"
          in Listings.shout ("Begin " ^ banner);
             map (Listings.report o nl) (Sort.quicksort lst);
	     Listings.shout ("End " ^ banner)
          end
      else ()

  fun deriveAlphabet Env (P as (VAR I)) = 
      ("alphabet of " ^ HashTable.unhash I, 
       fn () => map HashTable.unhash (Alphabets.alphabetOf Env P))
    | deriveAlphabet Env P = 
      ("missing alphabet of " ^ Prettyprinter.print P, fn () => [])

  fun compile P = 
      (cells := []; 
       tokens := []; 
       statics := []; 
       activities := []; 
       transitionNames := []; 
       rates := []; 
       getSynchronisationSets P;
       compileTop [] [] P)
  and compileTop (Env : environment) Arcs (MARKING L) = 
        let
           (* Places do not talk to other places so make a parallel composition *)
           val flatPepaMarking = COOP (map (compilePlace Env Arcs) L, []) 

           val _ = Listings.report(Prettyprinter.print (flatPepaMarking))
           val components : Identifier list = componentNames Env
           val places : Identifier list = placeNames L
	   val pairs : (Identifier * Identifier) list = product (rev components, rev places)
           val tokens = map (VAR o specializeToken) (!tokens)
           val staticComponents = map VAR (!statics)
           val bodies : Component list = 
                 map (generateToken Env Arcs) pairs (* side effect!! *)

           val newEnv : PepaNets.Environment = getEnvironments bodies
           val _ = map (list o deriveAlphabet newEnv) (map (VAR o #1) (newEnv))

           (* The following assumes at least one token and at least one static component  *)
           val rootPEPA = COOP ([promote newEnv tokens, flatPepaMarking], !transitionNames)
           (* The above assumes at least one token and at least one static component  *)

           val rateList = map fakeRate (removeInfty(!rates))
(*
           val individualActivities = minus(!activities, !transitionNames)
           val environment = 
	       if individualActivities <> [] 
                  then [mkEnvironment individualActivities]
                  else []
           val root = 
	       if individualActivities = [] 
		  then rootPEPA
		       else COOP([rootPEPA, VAR Environment], individualActivities)
*)

	   val _ = list ("activities", 
                         fn () => map HashTable.unhash (!activities))
	   val _ = list ("transition names", 
                         fn () => map HashTable.unhash (!transitionNames))
           val _ = map (list o deriveAlphabet Env) (map (VAR o #token) (!cells))
        in rateList
	       @ bodies
(*               @ environment  *)
	       @ [rootPEPA] 
        end
    | compileTop Env Arcs (CONSTS (I, ARC a, L)) = compileTop Env (a :: Arcs) L
    | compileTop Env Arcs (CONSTS (I, P, L))     = compileTop ((I, P) :: Env) Arcs L
    | compileTop _   _    P                      = Error.fatal_error "Compiling top level"
  and compilePlace Env Arcs (VAR P) = compilePlaceTerm Env Arcs P (lookup P Env)
    | compilePlace Env Arcs _       = Error.fatal_error "Compiling place in marking"
  and compilePlaceTerm Env Arcs Place (COOP ([P, Q], L)) = 
        let 
           val P' = compilePlaceTerm Env Arcs Place P
           val Q' = compilePlaceTerm Env Arcs Place Q
           val L' = compileList Place L
        in transitionNames := union(!transitionNames, L');
           COOP ([P', Q'], L')
        end
    | compilePlaceTerm Env Arcs Place (COOP (_, L)) = 
        Error.fatal_error "Compiling: aggregation"
    | compilePlaceTerm Env Arcs Place (CELL (P, NONE)) =
        (cells := { token=P, place=Place, marked=false } :: !cells;
	 VAR (stub P Place))
    | compilePlaceTerm Env Arcs Place (CELL (P, SOME P')) =
        (cells := { token=P, place=Place, marked=true } :: !cells;
	 tokens := { token=P, place=Place } :: !tokens;
	 VAR (stub P Place))
    | compilePlaceTerm Env Arcs Place (VAR I) = 
         let val static = specialize I Place
         in
	     statics := static :: !statics;
	     VAR static
	 end
    | compilePlaceTerm Env Arcs Place (HIDING (P, L)) = 
        let 
           val P' = compilePlaceTerm Env Arcs Place P
           val L' = compileList Place L
        in HIDING (P', L')
        end
    | compilePlaceTerm Env Arcs Place _ = 
        Error.fatal_error "Compiling: term at place contains non-top-level operators"
  and compileList Place [] = []
    | compileList Place (h::t) = 
        (specialize h Place) :: (compileList Place t)
  and stub Id Place = HashTable.hash (specialize' Id Place ^ "__Stub")

  fun compileFromFile filename = 
      let val file = Files.readFile filename
	  val tokens = Lexer.analyse file
	  val component = Parser.parse tokens
	  val output = ref ""
          val fileOutName = rename filename
	  val fileOut = TextIO.openOut(fileOutName)
       in Reporting.report ("Compiling PEPA net model from " ^ filename ^ "\n");
	  Prettyprinter.setMode Prettyprinter.verbose;
          output := String.concat (map Prettyprinter.print (compile component));
	  TextIO.output(fileOut, !output);
	  TextIO.closeOut(fileOut);
	  Reporting.report ("Writing PEPA model to " ^ fileOutName ^ "\n")	  
      end
  and rename filename = (removeSuffix o rev o explode) filename ^ ".pwb"
  and removeSuffix (#"a" :: #"p" :: #"e" :: #"p" :: #"." :: front) = implode (rev front)
    | removeSuffix t = implode (rev t)

  end (* local *)
end;
