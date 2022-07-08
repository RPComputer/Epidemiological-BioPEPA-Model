(* 
 File: Prettyprinter.sig

 Signature for the structure which prettyprints BioPEPA models
*)

signature Prettyprinter =
sig
  datatype printMode = compressed | uncompressed | verbose

  val setMode : printMode -> unit

  val print : BioPEPA.Component -> string
  val printRate : BioPEPA.Rate -> string

end;
