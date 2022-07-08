(* 
 File: StochKit.sig

 Structure which performs lexical analysis of BioPEPA rate expressions
 and component definitions with the intention of mapping to C/C++ for
 compilation with the StochKit libraries.

*)

signature StochKit = 
sig
  val compile : BioPEPA.Component -> unit
end;
