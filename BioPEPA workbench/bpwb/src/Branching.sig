(*
  File : Branching.sig

  Report the number of derivatives for each state
*)

signature Branching =
sig

  val reportDerivatives : PepaNets.State * 'a list -> unit

end;
