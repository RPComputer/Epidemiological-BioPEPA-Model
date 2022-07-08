(*
  File: Markings.sig

  Functions for processing markings in a PEPA Net.
*)
signature Markings = 
sig
  val initialPosition : PepaNets.Component -> int
  val replace : {old : PepaNets.Component, new : PepaNets.Component} -> 
        PepaNets.Component list -> PepaNets.Component list
  val replaceNth : int -> (PepaNets.Component * PepaNets.Component list)
        -> PepaNets.Component list
end;
