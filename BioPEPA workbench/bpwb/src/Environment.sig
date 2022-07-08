(*
   File: Environment.sig

   Build a definition environment for BioPEPA models
*)

signature Environment =
sig

  (* Build an environment *)
  val build : BioPEPA.Environment -> unit

  (* Look up a component definition *)
  val lookup : BioPEPA.Identifier -> BioPEPA.Component

end;
