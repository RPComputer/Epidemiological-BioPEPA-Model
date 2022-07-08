(*
  File: Graph.sig

  Compile the derivation graph for a PEPA model
*)
signature Graph =
sig
  val exceptionsRaised : int ref
  val compile : PepaNets.Component -> unit
end;
