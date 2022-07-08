(*
  File: Derivatives.sml

  Given an environment of definitions and a component, compute the one-step
  ((activity, rate), name) transitions leading to new components
*)
structure Derivatives :> Derivatives =
struct

  type Derivative = (BioPEPA.Identifier * BioPEPA.Rate) * BioPEPA.Name * BioPEPA.Component

  datatype Derivation = Firing of Derivative 
                      | Transition of Derivative

  type Derivatives = Derivation list

  local 
    fun member x [] =  false
      | member x (h::t) = (x=h) orelse (member x t)

    fun hide [] [] = [] 
      | hide x []  = x
      | hide [] l  = []
      | hide ((C, ((a, r), n, P))::t) l = 
	    (C, (if member a l 
	         then (HashTable.hide a, r)
	         else (a, r), n, P)) :: (hide t l)

    fun filter [] [] = [] 
      | filter x []  = x
      | filter [] l  = []
      | filter ((C, ((a, r), n, P))::t) l = 
	     if member a l 
	     then (C, ((a, r), n, P)) :: (filter t l)
	     else filter t l

    fun filterout [] [] = []
      | filterout x []  = x
      | filterout [] l  = []
      | filterout ((C, ((a, r), n, P))::t) l = 
	    if member a l 
	    then filterout t l
	    else (C, ((a, r), n, P)) :: (filterout t l)

    fun cellPref NONE NONE = NONE
      | cellPref NONE C    = C
      | cellPref C    NONE = C
      | cellPref C1   C2   = C1

    fun cooperations n [] dQ L = []
      | cooperations n dP [] L = []
      | cooperations n dP dQ [] = []
      | cooperations n (((C1, C1'), ((a, r1), _, P as BioPEPA.COOP (P', _))) :: t1) 
                       (((C2, C2'), ((b, r2), _, Q as BioPEPA.COOP (Q', _))) :: t2) L =
	  (if a = b andalso member a L 
	   then [((cellPref C1 C2,cellPref C1' C2'), ((a, BioPEPA.min(r1, r2)), n, BioPEPA.COOP (P' @ Q', L)))] 
	   else [])  @ 
	  (cooperations n [((C1, C1'), ((a, r1), n, P))]  t2                              L) @
	  (cooperations n t1                              [((C2, C2'), ((b, r2), n, Q))]  L) @
	  (cooperations n t1                              t2                              L)
      | cooperations _ _ _ _ = Error.internal_error "cooperations function error"

    fun selectOnPlace (P, []) = []
      | selectOnPlace (P, (P1, (a,r), P2) :: Arcs) = 
           if P = BioPEPA.VAR  P1 then
              ((a, r), P2) :: selectOnPlace (P, Arcs)
           else
              selectOnPlace (P, Arcs)

    fun selectOnPlaceAndActivity (P, a, []) = []
      | selectOnPlaceAndActivity (P, a, (P1, (a',r), P2) :: Arcs) = 
           if P = BioPEPA.VAR P1 andalso a = a' then
              P2 :: selectOnPlaceAndActivity (P, a, Arcs)
           else
              selectOnPlaceAndActivity (P, a, Arcs)

    fun outgoingArcs (P, L) = map #1 (selectOnPlace (P, L))
    fun outgoingActivities (P, L) = map #1 (outgoingArcs (P, L))

    val transitions = ref 0

    fun priority (cell, ((a, r), n, P)) = Priorities.getPriority a
    fun higherPriority (a,b) = higherPriority'(priority a, priority b)
    and higherPriority'(NONE, NONE) = false
      | higherPriority'(SOME p, NONE) = true
      | higherPriority'(NONE, SOME p) = false
      | higherPriority'(SOME p, SOME p') = p > p'
    fun actionsWithoutPriorities c = priority c = NONE

    (* We never want to insert firings with priority NONE *)
    fun insertOverride (c, []) = if priority c = NONE then [] else [c]
      | insertOverride (c, h::t) = if higherPriority(c, h) then [c]
                                   else if priority c = priority h then c :: h :: t
                                        else h :: t
    fun selectHighest [] = []
      | selectHighest (h::t) = insertOverride (h, selectHighest t)

    fun selectOnPriorities l = 
        if !Priorities.enabled 
        then (* This is the PEPA nets extension for priorities *)
             List.filter actionsWithoutPriorities l @ selectHighest l
        else (* This is the normal PEPA case *)
             l

    local 
      fun cderiv cell (E, Arcs) n (BioPEPA.PREFIX (a as (alpha, rate), P)) = [(cell, (a, n, P))]
	| cderiv cell (E, Arcs) n (BioPEPA.CHOICE (P, Q)) = 
                (cderiv cell (E, Arcs) n P) @ (cderiv cell (E, Arcs) n Q)
	| cderiv cell (E, Arcs) n (BioPEPA.COOP ([], L)) = []
	| cderiv cell (E, Arcs) n (BioPEPA.COOP ([P], L)) = 
                let  val dP = cderiv cell (E, Arcs) n P
                in
                     map (fn (C, (a, n, P')) => (C, (a, n, BioPEPA.COOP ([P'], L)))) dP
                end
	| cderiv cell (E, Arcs) n (BioPEPA.COOP (P :: Q, L)) = 
		let  val dP = cderiv cell (E, Arcs) (n) (BioPEPA.COOP ([P], L))
		     val dQ = cderiv cell (E, Arcs) (n + 1) (BioPEPA.COOP (Q, L))
		     val fP = filterout dP L
		     val fQ = filterout dQ L
		in
		     (map (fn (C, (a, n, BioPEPA.COOP (P', L))) => 
                              (C, (a, n, BioPEPA.COOP (P' @ Q, L)))
                            | _ => Error.internal_error "cderiv cooperation case (1)") fP)
		   @ (map (fn (C, (a, n, BioPEPA.COOP (Q', L))) => 
                              (C, (a, n, BioPEPA.COOP (P :: Q', L)))
                            | _ => Error.internal_error "cderiv cooperation case (2)") fQ)
		   @ cooperations n dP dQ L
		end
	| cderiv cell (E, Arcs) n (BioPEPA.HIDING (P, L)) = hide (cderiv cell (E, Arcs) n P) L
	| cderiv cell (E, Arcs) n (BioPEPA.CELL (P, NONE)) = []
	| cderiv _    (E, Arcs) n (C as BioPEPA.CELL (P, SOME P')) = 
		map (fn ((C, C'), (a, n, P')) => 
                        let val C' = BioPEPA.CELL (P, SOME P')
                        in ((C, SOME C'), (a, n, C'))
                        end) 
                (cderiv (SOME C, NONE) (E, Arcs) n P')
	| cderiv cell (E, Arcs) n (BioPEPA.VAR I) = cderiv cell (E, Arcs) n (Environment.lookup I)
        | cderiv cell (E, Arcs) n (BioPEPA.CONSTS (I, P, L)) = 
                Error.internal_error "Derivation not defined for component declarations"
        | cderiv cell (E, Arcs) n (BioPEPA.ARC _) = 
                Error.internal_error "Derivation not defined for arcs"
        | cderiv cell (E, Arcs) n (BioPEPA.RATE _) = 
                Error.internal_error "Derivation not defined for rates"
        | cderiv cell (E, Arcs) n (BioPEPA.PRIORITIES _) = 
                Error.internal_error "Derivation not defined for priorities"
        | cderiv cell (E, Arcs) n (BioPEPA.MARKING []) = []
        | cderiv cell (E, Arcs) n (BioPEPA.MARKING (x::xs)) = 
	        case (!Statespace.initialMarking) of
		   BioPEPA.MARKING (p::ps) =>
		       selectOnPriorities (cderiv' cell (E, Arcs) n ([] , (p, x), (ps, xs)))
	        | _ => Error.fatal_error ("Empty initial marking")
      and cderiv'  cell (E, Arcs) n (L, (p, x), ([], [])) = 
          cderiv'' cell (E, Arcs) n (L, (p, x), ([], []))
        | cderiv'  cell (E, Arcs) n (L, (p, x), (_::_, [])) =
              Error.internal_error "Initial marking/current marking mismatch"
        | cderiv'  cell (E, Arcs) n (L, (p, x), ([], _::_)) =
              Error.internal_error "Current marking/initial marking mismatch"
        | cderiv'  cell (E, Arcs) n (L, (p, x), (ps as h1::t1, xs as h2::t2)) =
          cderiv'' cell (E, Arcs) n (L, (p, x), (ps, xs)) @ 
          cderiv'  cell (E, Arcs) n (L @ [x], (h1, h2), (t1, t2))
      and cderiv'' cell (E, Arcs) n (L, (p, x), (ps, xs)) = 
          let 
	      val outgoing = outgoingActivities (p, Arcs)
              val  dx   = cderiv cell (E, Arcs) n x      (* All derivatives *)
	      val  dx'  = filterout dx outgoing          (* PEPA component derivatives *)
	      val  dx'' = filter dx outgoing             (* PEPA cell derivatives *)
	      fun destinations ((SOME (C as BioPEPA.CELL (I, _)), _), ((a, r), n, P)) = 
                  let 
                     val arcPossibles = selectOnPlaceAndActivity (p, a, Arcs)
                     val posnPossible = map (Markings.initialPosition o BioPEPA.VAR) arcPossibles
                     val currentPossibles = map (fn n => (n, List.nth (L @ [x] @ xs, n))) posnPossible
                  in List.filter (fn (n, t) => Cell.hasVacantCellOfType { term = t, ofType = BioPEPA.VAR I }) currentPossibles
                  end
                | destinations _ = []
              fun firing ((C  as SOME (BioPEPA.CELL (P, SOME P')), 
                           C' as SOME (BioPEPA.CELL (Q, SOME Q'))), ((a, r), n, P'')) = 
                  let
                     val (result, departures) = Cell.vacateCell (P', x)
                     fun product (h::t, l) = List.map (fn x => (h, x)) l @ product (t, l)
                       | product _ = []
                     val pairs = product (departures, (destinations ((C, C'), ((a, r), n, P''))))
                     fun moveComponent (from, (n, to)) = 
                         let 
			     val marking = L @ [from] @ xs
                             val allFillings = Cell.fillCell (Q', to)
                         in  map (fn c => ((C, C'), ((a, r), n, 
                                      BioPEPA.MARKING (Markings.replaceNth n (c, marking))))) allFillings
                         end
                  in if result then List.concat (List.map moveComponent pairs) else []
                  end
                | firing _ = []

	      fun rmDuplicates [] = [] 
		| rmDuplicates (h::t) = if member (h,t) then rmDuplicates t else h :: rmDuplicates t
	      and member (x,[]) = false | member (x,h::t) = x=h orelse member(x,t)

              fun countTransitions l = (transitions := !transitions + length l; l)

          in  (* The front of the list of derivatives contains local transitions *)
              countTransitions (map (fn (C, (a, n, P')) => 
                                        (C, (a, n, BioPEPA.MARKING (L @ [P'] @ xs)))) dx')
            @ (* The rear of the list of derivatives contains global firings *)
              rmDuplicates (List.concat (List.map firing dx''))
          end 
    in
      fun derivative (E, Arcs) n A = 
	  let val _ = transitions := 0
              val d = map #2 (cderiv (NONE, NONE) (E, Arcs) n A)
          in
	      (* Label the list of derivatives: 
                   to the front are transitions; 
                   to the rear are firings       *)
	      (map Transition (List.take (d, !transitions))) @ 
	      (map Firing (List.drop (d, !transitions)) )
	  end
    end

  in
 
    val derivative = fn (E, Arcs) => fn A =>
      case derivative (E, Arcs) 0 A of
         [] => (* no outgoing transitions, so this is a deadlock state *)
               (Prettyprinter.setMode Prettyprinter.verbose;
	        Error.warning ("deadlock found at\n\t" ^ Prettyprinter.print A); [])
        | v => (* not a deadlock, but need to check for absorbing states *)
               let 
                  fun detag (Firing (_, _, c)) = c
                    | detag (Transition (_, _, c)) = c
                  val l = map detag v
                  fun allEqual [] A = true
                    | allEqual (h::t) A = (h = A) andalso allEqual t A
               in if allEqual l A 
                  then (Prettyprinter.setMode Prettyprinter.verbose;
	                Error.warning ("absorbing state with no outgoing transitions found at\n\t" ^ 
                           Prettyprinter.print A))
                  else ();
                  if !Options.branching
                  then Branching.reportDerivatives(A, v)
                  else ();
                  v
               end
  end

end;
