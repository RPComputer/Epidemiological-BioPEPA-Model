(* 
 File: BioPEPA.sig

 Interface to the internals of the BioPEPA Workbench.
*)

signature BioPEPA =
sig
  type Identifier = string

  type Name = int
  type Rate = string

  datatype Component =  
	  INCREASES of (Identifier * Identifier) * Component
	| DECREASES of (Identifier * Identifier) * Component
	| CHOICE    of Component * Component
	| COOP      of Component * Component * Identifier list
	| HIDING    of Component * Identifier list
	| VAR       of Identifier
	| RATE      of Identifier
	| CONSTS    of Identifier * Component * Component

  (* States and components are isomorphic.  The difference
     is that components are specified by the modeller and
      states are found by the Workbench.
  *)
  type State = Component

  type Environment = (Identifier * Component) list

  type Transition = Component * 
       ((Identifier * Identifier) * Name) option * Component

  val repaintIdentifier : string -> string 

end;
