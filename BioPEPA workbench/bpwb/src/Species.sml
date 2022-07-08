(* 
 File: Species.sml

 Species handling for the BioPEPA Workbench
*)

structure Species :> Species =
struct

  val species = ref [] : (string * int) list ref

  fun set (s, n) = species := (s, n) :: !species

  fun look s [] = Error.fatal_error("Species " ^ s ^ " not found in lookup")
    | look s ((s1, n)::t) = if s = s1 then n else look s t
  fun lookup s = look s (!species)

end;
