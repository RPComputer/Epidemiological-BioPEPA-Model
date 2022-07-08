(* 
 File: Lexer.sig

 Functions which perform lexical analysis of BioPEPA models.
*)

signature Lexer =
sig

  datatype token = 
      Ident of BioPEPA.Identifier
    | Float of string
    | Symbol of char

  val prettyprintToken : token -> string
  val analyse : char list -> token list

end;

