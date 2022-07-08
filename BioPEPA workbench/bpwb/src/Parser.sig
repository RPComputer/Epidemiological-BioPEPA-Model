(* 
 File: Parser.sig

 Signature for the structure which contains the BioPEPA parser
*)

signature Parser =
sig
  val parse : Lexer.token list -> BioPEPA.Component
end;
