(* 
  File: Aggregate.sml

  Provide functions to aggregate PEPA net transitions based on 
  similarity of terms.
*)
structure Aggregate  :> Aggregate =
struct


local
  open PepaNets

  fun le (VAR I, VAR I') = HashTable.le(I, I')
    | le (VAR _, _) = true
    | le (COOP ([], L), COOP ([], L')) = le_st (L, L')
    | le (COOP ([], L), COOP (_,  L')) = true
    | le (COOP (_,  L), COOP ([], L')) = false
    | le (COOP (h1::t1, L), COOP (h2::t2, L')) =
      le (h1, h2) orelse (h1=h2 andalso le (COOP (t1, L), COOP (t2, L')))
    | le (COOP _, _) = true
    | le (CELL (I, C_opt), CELL (I', C_opt')) = 
      if I=I' then le_opt (C_opt, C_opt') else HashTable.le(I, I')
    | le (HIDING (P, L), HIDING (P', L')) = 
      le (P, P') orelse (P=P' andalso le_st (L, L'))
    | le (HIDING _, _) = true
    | le _ = true
  and le_opt (NONE, NONE) = true
    | le_opt (SOME _, NONE) = true
    | le_opt (NONE, SOME _) = false
    | le_opt (SOME P, SOME P') = le (P, P')
  and le_st ([], []) = true
    | le_st ([], _) = true
    | le_st (_, []) = false
    | le_st (h1::t1, h2::t2) = 
        if h1 = h2 then le_st (t1, t2) else HashTable.le(h1, h2)
  and le_pair (((a, r), _, P), ((a', r'), _, P')) =
      if a = a' 
      then if r = r' then le (P, P') else le_rate (r, r')
      else HashTable.le (a, a')
  and le_labelled_pair (Derivatives.Firing f, Derivatives.Firing f') = le_pair (f, f')
    | le_labelled_pair (Derivatives.Firing f, Derivatives.Transition t) = true
    | le_labelled_pair (Derivatives.Transition t, Derivatives.Firing f) = false
    | le_labelled_pair (Derivatives.Transition t, Derivatives.Transition t') = le_pair (t, t')
  and le_rate (PepaNets.Top, _) = true
    | le_rate (PepaNets.Rate r, PepaNets.Rate r') =  HashTable.le (r, r')
    | le_rate (PepaNets.Product (n, r), PepaNets.Product (n', r')) =
      if n < n' then true else
      if n = n' then le_rate (r, r') else false
    | le_rate _ = false

  fun sorta [] = []
    | sorta ((COOP (C, L)) :: t) = 
      let val h = COOP (sorta C, L)
      in insert (h, sorta t)
      end
    | sorta ((MARKING M) :: t) = 
      (* When sorting a marking we do not change the relative *)
      (* ordering of places in the marking.                   *)
      let val h = MARKING (map sort_sing M) 
      in insert (h, sorta t)
      end
    | sorta (h::t) = insert (h, sorta t)
  and insert (x, []) = [x]
    | insert (x, h::t) =
      if le (x, h) then x :: h :: t
      else h :: insert (x, t)
  and sort_sing (CONSTS (I, P, L)) = CONSTS (I, sort_sing P, sort_sing L)
    | sort_sing A = hd (sorta [A])

  (* Sorting destroys name information *)
  fun sortp [] = []
    | sortp (h as Derivatives.Firing (a, _, P) :: t) =
      insert (Derivatives.Firing (a, 0, sort_sing P), sortp t)
    | sortp (h as Derivatives.Transition (a, _, P) :: t) =
      insert (Derivatives.Transition (a, 0, sort_sing P), sortp t)
  and insert (x, []) = [x]
    | insert (x, h::t) =
      if le_labelled_pair (x, h) then x :: h :: t
      else h :: insert (x, t)
in
  val sort_component = sort_sing
  val sort_components = sorta 
  val sort_pairs = sortp
end;


local
  fun mktrio n [] = []
    | mktrio n [x] = [(n, x)]
    | mktrio n (h1::h2::t) = 
      if h1=h2
      then mktrio (n+1) (h2::t)
      else (n, h1) :: mktrio 1 (h2::t)


 (* Make PepaNets.Product of int * Rate (rather than int * Identifier)
    then everything else follows from this, ie we match against all rates
    rather than just Rate.
  *)
  fun minimize_trio (1, Derivatives.Firing ((a, r), _, P)) = Derivatives.Firing ((a, r), 0, P)
    | minimize_trio (1, Derivatives.Transition ((a, r), _, P)) = Derivatives.Transition ((a, r), 0, P)

    | minimize_trio (m, Derivatives.Firing ((a, PepaNets.Top), _, P)) = 
          Error.internal_error ("multiple passive firings: " ^ Int.toString m)

    | minimize_trio (m, Derivatives.Transition ((a, PepaNets.Top), _, P)) = 
          Error.internal_error ("multiple passive transitions: " ^ Int.toString m)

    | minimize_trio (m, Derivatives.Firing ((a, PepaNets.Product (n, r)), _, P)) = 
      Derivatives.Firing ((a, PepaNets.Product (m * n, r)), 0, P)

    | minimize_trio (m, Derivatives.Transition ((a, PepaNets.Product (n, r)), _, P)) = 
      Derivatives.Transition ((a, PepaNets.Product (m * n, r)), 0, P)


      (*
       At this point we assume that the rate argument of Firing is not PepaNets.Top
       or PepaNets.Product because we've matched against both of those.
       *)
    | minimize_trio (m, Derivatives.Firing ((a, rate), _, P)) = 
      if m <= 0 then
	  Error.internal_error ("miscounted occurrences: " ^ Int.toString m)
      else Derivatives.Firing ((a, PepaNets.Product (m, rate)), 0, P)

      (*
	Again the rate argument to Transition cannot be PepaNets.Top or PepaNets.Product
       *)
    | minimize_trio (m, Derivatives.Transition ((a, rate), _, P)) = 
      if m <= 0 then
	  Error.internal_error ("miscounted occurrences: " ^ Int.toString m)
      else Derivatives.Transition ((a, PepaNets.Product (m, rate)), 0, P)

in
  val minimize = (map minimize_trio) o (mktrio 1) o sort_pairs
end;


end;
