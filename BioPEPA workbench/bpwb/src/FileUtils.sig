(*
   File: FileUtils.sig

   Implement post-2005 revision of the Basis inputLine

*)
signature FileUtils =
sig
   val inputLine : TextIO.instream -> string
end;

