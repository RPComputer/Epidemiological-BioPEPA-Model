(* 
 File: Sugar.sig

 Signature for the structure which contains the BioPEPA process
 which removes syntactic sugar from models.  I.e. removes 
 keywords such as species and kineticLawOf.
*)

signature Sugar =
sig
  val desugar : Lexer.token list -> Lexer.token list 
end;
