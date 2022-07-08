(* 
  File: Version.sml

  Version information for this release of the BioPEPA Workbench
*)

structure Version :> Version  =
struct 

  val version = "1.0 \"Charlie Mingus\"";
  val compiled = "22nd-April-2009";
  val banner = "Bio-PEPA Workbench Version "^ version ^ " [" ^ compiled ^ "]"

end;
