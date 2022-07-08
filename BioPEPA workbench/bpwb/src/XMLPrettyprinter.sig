(* 
 File: XMLPrettyprinter.sig

 Signature for the structure which prettyprints BioPEPA models
 in XML format
*)

signature XMLPrettyprinter =
sig
  datatype printMode = verbose  (* :-) *)

  val setMode : printMode -> unit

  val print : BioPEPA.Component -> string
  val printRate : BioPEPA.Rate -> string

end;
