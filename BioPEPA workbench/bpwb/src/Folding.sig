(* 
  File: Folding.sig
	
  Algorithm to fold component expressions of the form
    P <L> Q <L> R
  into
    [P, Q, R]_L
*)

signature Folding =
sig
    val fold : PepaNets.Component -> PepaNets.Component 
end;
