(*
   File: Environment.sml

   Build a definition environment for BioPEPA models
*)

structure Environment :> Environment =
struct

  (* A local data structure which stores (identifier, component) pairs
     in an updateable ordered binary tree.
  *)
  datatype tree = 
    empty 
  | node of tree ref * (BioPEPA.Identifier * BioPEPA.Component) * tree ref

  (* The environment *)
  val env = ref empty

  fun ins (def as (id, comp), env as ref empty) = 
      env := node (ref empty, def, ref empty)
    | ins (def as (id, comp), env as ref (node (left, (id', comp'), right))) = 
      if id = id' then
	  Error.internal_error ("duplicate definition for " ^ id)
      else if (id <= id') then
	  ins (def, left) 
      else
	  ins (def, right)

  fun insert def = ins (def, env)

  (* Build an environment *)
  fun build e = List.app insert e

  (* Lookup an identifier *)
  fun look (id, ref empty) = Error.internal_error ("no definition for " ^ id)
    | look (id, env as ref (node (left, (id', comp'), right))) =
      if id = id' then comp'
      else if (id <= id') then
	  look (id, left)
      else
	  look (id, right)

  fun lookup id = look (id, env)

end;
