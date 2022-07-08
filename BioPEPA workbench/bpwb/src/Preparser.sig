(* 
 File: Preparser.sig

 Signature for the structure which contains the BioPEPA preparser
 which checks for simple syntax errors.
*)

signature Preparser =
sig
  val preparse : Lexer.token list -> Lexer.token list 
end;
