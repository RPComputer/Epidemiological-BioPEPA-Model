(*
  File: Graph.sml

  Compile the derivation graph for a PEPA model
*)
structure Graph :> Graph =
struct

  val exceptionsRaised = ref 0

  local 
    fun lookup I [] = 
        Error.internal_error ("undeclared identifier: " ^ HashTable.unhash I)
      | lookup I ((Id, P) :: t) = if I = Id then P else (lookup I t)

    fun graph (E, Arcs) (A as (PepaNets.PREFIX ((alpha, rate), P)))
	    = usual (E, Arcs) A
      | graph (E, Arcs) (A as (PepaNets.CHOICE (P, Q)))
	    = usual (E, Arcs) A
      | graph (E, Arcs) (A as (PepaNets.COOP _))
	    = usual (E, Arcs) A
      | graph (E, Arcs) (A as (PepaNets.HIDING (P, L)))
	    = usual (E, Arcs) A
      | graph (E, Arcs) (A as (PepaNets.CELL (P, P')))
	    = usual (E, Arcs) A
      | graph (E, Arcs) (A as (PepaNets.ARC _))
	    = Error.fatal_error "Derivation graph not defined for PEPA arcs"
      | graph (E, Arcs) (A as (PepaNets.RATE _))
	    = Error.fatal_error "Derivation graph not defined for rates"
      | graph (E, Arcs) (A as (PepaNets.PRIORITIES _))
	    = Error.fatal_error "Derivation graph not defined for priorities"
      | graph (E, Arcs) (A as (PepaNets.MARKING l))
	    = usual (E, Arcs) (PepaNets.MARKING (map (expandParallel E) l))
      | graph (E, Arcs) (A as (PepaNets.VAR I))
	    = let val A' = lookup I E
	       in (graph (E, Arcs) A';  ())
              end
      | graph (E, Arcs) (A as (PepaNets.CONSTS (_, PepaNets.ARC (arc as (P1, (a, r), P2)), L)))
	    = graph (E, (arc :: Arcs)) L
      | graph (E, Arcs) (A as (PepaNets.CONSTS (I, P, L)))
	    = graph ((I, P) :: E, Arcs) L

    and expandParallel E (A as (PepaNets.VAR I)) =
        (case lookup I E of
	     (A' as PepaNets.COOP _) => A'
	 |   (A' as PepaNets.CELL _) => A'
         |   _ => A
	)
      | expandParallel E C = C

    and list (E, Arcs) A [] = ()
      | list (E, Arcs) A ((a, _, A')::t) = 
		    ((graph (E, Arcs) A'); 
		     (list (E, Arcs) A' t);
		     ())

    and usual (E, Arcs) A =
       case Table.addState A of
         Table.AlreadySeen _ => ()
       | Table.NotYetSeen stateCode => 
	   let  
	       val dA = Derivatives.derivative (E, Arcs) A
               val dA' = if !Options.aggregating 
                         then Aggregate.minimize dA
                         else dA

               fun countFirings [] = 0
                 | countFirings (Derivatives.Firing _ :: t) = 1 + countFirings t
                 | countFirings (Derivatives.Transition _ :: t) = countFirings t

               fun countTransitions [] = 0
                 | countTransitions (Derivatives.Firing _ :: t) = countTransitions t
                 | countTransitions (Derivatives.Transition _ :: t) = 1 + countTransitions t

               val removedFirings = countFirings dA - countFirings dA'
               val removedTransitions = countTransitions dA - countTransitions dA'

               fun removeLabels (Derivatives.Firing d) = (Statespace.countFiring(); d)
                 | removeLabels (Derivatives.Transition d) = (Statespace.countTransition(); d)

               val dA'' = map removeLabels dA'

	    in   
	       map (fn ((a,r), n, A') => 
		 (Statespace.addTransition (A, SOME ((a,r), n), A'))) dA'';
                  Table.markSeen stateCode;
		  list (E, Arcs) A dA'';
		  () 
	   end 

    fun environment (A as (PepaNets.CONSTS (_, PepaNets.ARC _, L)))
	    = environment L
      | environment (A as (PepaNets.CONSTS (I, P, L)))
	    = (I, P) :: environment L
      | environment _ = []

    fun priorities (A as (PepaNets.CONSTS (_, PepaNets.PRIORITIES p, L))) = p
      | priorities (A as (PepaNets.CONSTS (I, P, L))) = priorities L
      | priorities _ = []

  in
    fun compile C = 
        (Table.initialize();
         Statespace.initialize (); 
	 Environment.build (environment C);
	 if !Priorities.enabled then Priorities.setPriorities (priorities C) else ();
         graph ([], []) C; 
         Table.finalize();
	 Statespace.finalize())
  end  

end;
