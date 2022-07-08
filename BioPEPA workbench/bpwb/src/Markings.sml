(*
  File: Markings.sml
 
  Functions for processing markings in a PEPA Net.
*)
structure Markings :> Markings =
struct

  fun initialPosition P = findPosition P (getList (!Statespace.initialMarking))
  and getList (PepaNets.MARKING M) = M
    | getList _ = Error.internal_error "invalid initial marking"
  and findPosition P [] = Error.internal_error "missing entry in initial marking"
    | findPosition P (h :: t) = if P = h then 0 else 1 + findPosition P t

  fun replace { old = P1, new = P2 } M =
      let
	  val n = initialPosition P1
      in
	  replaceNth n (P2, M)
      end
  and replaceNth 0 (P, _::t) = P :: t
    | replaceNth n (P, h::t) = h :: replaceNth (n - 1) (P, t)
    | replaceNth _ _ = Error.internal_error "misnumbering in marking"

end;
