(*
   File: Priorities.sml

   Priorities for PEPA net firings
*)

structure Priorities :> Priorities =
struct
  
   (* Priorities are enabled: default false *)
   val enabled = ref false

   (* A local data structure which stores (identifier, priority) pairs
      in an updateable ordered binary tree.
   *)
   datatype tree = 
     empty 
   | node of tree ref * (PepaNets.Identifier * int) * tree ref

   (* The environment for priority information *)
   val priorities = ref empty

   fun ins (pri as (id, p), priorities as ref empty) = 
       priorities := node (ref empty, pri, ref empty)
     | ins (pri as (id, p), priorities as ref (node (left, (id', p'), right))) = 
       if id = id' then
	   Error.fatal_error ("more than one priority found for ``" ^ HashTable.unhash id ^ "''")
       else if HashTable.le (id, id') then
	   ins (pri, left) 
       else
	   ins (pri, right)

   fun insert pri = ins (pri, priorities)

   (* Set the priorities of firings *)
   fun setPriorities p =
       setPriority (length p) p
   and setPriority n [] = ()
     | setPriority n (h::t) = (set n h; setPriority (n - 1) t)
   and set n p = List.app (fn x => insert(x, n)) p

   (* Enable priorities *)
   fun setEnabled () = enabled := true

   (* Priorities: higher numbers take priority over lower ones *)
   (* Lookup an identifier *)
   fun look (id, ref empty) = NONE
     | look (id, priorities as ref (node (left, (id', p'), right))) =
       if id = id' then SOME p'
       else if HashTable.le (id, id') then
	   look (id, left)
       else
	   look (id, right)

   (* Get the priority of a firing, either NONE or SOME int *)
   fun getPriority i = look (i, priorities)

end;
