(*
   File: MyTextIO.sml
*)
structure MyTextIO :> MyTextIO =
struct
    fun inputLine is =
       let val c = ref NONE
           and ln = ref [] : char list ref
           fun chVal (ref NONE) = #" "
             | chVal (ref (SOME c)) = c
        in
           while (c := TextIO.input1 is; !c <> SOME #"\n") do
             ln := chVal c :: !ln;
           implode (rev (!ln))
       end
end;
