(* 
 File: Semantic.sig

 Signature for the structure which performs semantic analysis of
 BioPEPA models

*)

signature Semantic =
sig
  val analyse : BioPEPA.Component -> BioPEPA.Component
end;
