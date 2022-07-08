(*
 File: Bridge.sig
 
 Makes a bridge between PEPA and Dizzy
*)
 
signature Bridge =
sig
    val bridgeToDizzy : PepaNets.Component -> string
    val bridgeToDizzyFromFile : string -> unit
end;

