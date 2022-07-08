(* 
 File: BioPEPA.sml

 Datatypes of the BioPEPA Workbench.
*)

structure BioPEPA :> BioPEPA =
struct
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

  type State = Component

  type Environment = (Identifier * Component) list

  type Transition = Component * 
       ((Identifier * Identifier) * Name) option * Component

  fun repaintIdentifier i = 
    let fun toCId #":" = "_colon_"
          | toCId #"-" = "_dash_"
          | toCId #"@" = "_at_"
          | toCId #"'" = "_prime_"
          | toCId c = str c
     in String.translate toCId i 
    end


(* 
   Identifiers and Names 
   =====================
   An identifier is an alphanumeric descriptor assigned by the PEPA modeller
   whereas a name is a numeric stamp computed by the PEPA Workbench to allow
   the workbench to distinguish between repeated occurrences of a component.
   Names are assigned by the `derivative' function on traversing the tree
   of the abstract syntax of the PEPA model.  Names are assigned thus:

                               0
                              / \
                             1   2
                            / \ / \
                           3  4 5  6
   
   doubling as we go down the tree and adding 1 if we go left and 2 if we go 
   right.  This will make a distinction between P <> P <> P <> P, numbering the
   copies 1, 5, 13 and 14 and a bushier tree, (P <> P) <> (P <> P), numbering
   these copies 3, 4, 5, 6.  In either case the effect is the same: to have
   unique numbers for the leaves of the tree (the sequential components) and 
   the nodes (the cooperations).
*)

end;
