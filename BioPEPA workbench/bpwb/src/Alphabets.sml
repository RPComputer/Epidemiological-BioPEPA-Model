(*
   File: Alphabets.sig
 
   Calculate alphabets of components
*)
structure Alphabets :> Alphabets =
struct
  local open PepaNets in 

  fun member x [] = false
    | member x (h::t) = x = h orelse member x t

  fun allMembers [] _ = true
    | allMembers (h::t) l = member h l andalso allMembers t l

  fun union [] l = l
    | union (h::t) l = 
       let val remainder = union t l 
	in if member h l then remainder else h :: remainder
       end

  fun subtract [] l = []
    | subtract (h::t) l = 
       let val remainder = subtract t l
        in if member h l then remainder else h :: remainder
       end

  fun distributedunion [] = []
    | distributedunion (h::t) = union h (distributedunion t)

  fun lookup I [] = Error.fatal_error "Lookup in Alphabets"
    | lookup I ((I', P) :: t) =
	if I = I' then P else lookup I t

  fun firstActions E (VAR I) = firstActions E (lookup I E)
    | firstActions E (CHOICE(P, Q)) = union (firstActions E P)
					    (firstActions E Q)
    | firstActions E (PREFIX ((a, _), _)) = [a]
    | firstActions E _ = []

  fun leadingActivities E (VAR I) = leadingActivities E (lookup I E)
    | leadingActivities E (CHOICE(P, Q)) = union (leadingActivities E P)
					         (leadingActivities E Q)
    | leadingActivities E (PREFIX ((a, _), VAR _)) = [a]
    | leadingActivities E (PREFIX ((a, _), P)) = union [a] (leadingActivities E P)
    | leadingActivities E _ = []

  fun oneStepDerivatives E (VAR I) = oneStepDerivatives E (lookup I E)
    | oneStepDerivatives E (CHOICE(P, Q)) = union (oneStepDerivatives E P)
					    (oneStepDerivatives E Q)
    | oneStepDerivatives E (PREFIX (_, P)) = [P]
    | oneStepDerivatives E _ = []

  fun fix derivatives E =
    let val nextDerivatives = distributedunion (map (oneStepDerivatives E) derivatives)
    in if allMembers nextDerivatives derivatives
       then derivatives
       else fix (union derivatives nextDerivatives) E
    end

  fun derivativesOf E P = fix (oneStepDerivatives E P) E

  fun alphabetOf E (COOP(lst, _)) = distributedunion(map (alphabetOf E) lst)
    | alphabetOf E (HIDING(P, L)) = subtract (alphabetOf E P) L
    | alphabetOf E P =
       let val derivatives = derivativesOf E P
        in distributedunion (map (leadingActivities E) derivatives)
       end

  end
end;
