(* 
   File: pepa.sml

   This is the root file for the MLj compilation process and 
   refers the compiler to the PWB Standard ML structure.

   This version of the PEPA compiler compiles with MLj 0.1,
   but not 0.2
*)

structure jpwb = struct 

_public _classtype T 
{

  _public _static _final _method "main" (env : Java.String option Java.array option) =
      case env of 
	NONE => 
	   PWB.main []
      | SOME env' => 
	let
	  val array = Java.toArray env'
	in
	  if Array.length array = 0 
	  then PWB.main []
	  else
	    case Array.sub(array, 0) of
	      NONE => PWB.main []
	    | SOME jstr =>
	      PWB.main [Java.toString jstr]
	end
}


end;
