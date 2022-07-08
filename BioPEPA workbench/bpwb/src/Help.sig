(*
   File: Help.sig

   Display help messages on command-line usage of the Workbench
*)

signature Help =
sig
  (* Print help message and throw the exception given *)
  val help : exn -> 'a
end;
