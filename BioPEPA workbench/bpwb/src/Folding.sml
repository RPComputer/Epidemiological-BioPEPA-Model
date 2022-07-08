(* 
  File: Folding.sml
	
  Algorithm to fold component expressions of the form
    P <L> Q <L> R
  into
    [P, Q, R]_L
*)

structure Folding :> Folding =
struct

  local
    open PepaNets 

    local
      fun sort []                     = []
	| sort (h::t)                 = insert h (sort t)
      and insert (x: Identifier) []   = [x]
	| insert x (l as h::t)        = if x = h then l else 
					  if HashTable.le(x, h) 
					  then x::l else h :: insert x t
    in
      fun fold (COOP ([], L))       = (COOP ([], sort L))
	| fold (COOP (h::t, L))     = let
					val recCase = fold (COOP (t, L))
				      in
					case (recCase, h) of
					  (COOP (t', L'), COOP (t'', L'')) => 
					    if seteq (L, L'') 
					    then COOP (map fold t'' @ t', L')
					    else COOP (fold h :: t', L')
					| (COOP (t', L'), _) => COOP (h :: t', L')
					| _ => Error.fatal_error "folding implementation bug"
				      end
	| fold (HIDING (P, L))      = (HIDING (fold P, sort L))
	| fold (CONSTS (I, P, L))   = (CONSTS (I, fold P, (fold L)))
	| fold P                    = P
      and seteq (s1, s2)            = subset (s1, s2) andalso subset (s2, s1)
      and subset ([], _)            = true
	| subset (h::t, s)          = let
					fun member (x, []) = false
					  | member (x, h::t) = x=h orelse member (x, t)
				      in
					member (h, t) andalso subset (t, s)
				      end
    end
  in

    fun fixpointfold A = 
      let val A' = fold A 
       in if A = A'
	  then A
	  else fixpointfold A'
      end

    val fold : PepaNets.Component -> PepaNets.Component = 
	Aggregate.sort_component o fixpointfold o Aggregate.sort_component

  end
end;
