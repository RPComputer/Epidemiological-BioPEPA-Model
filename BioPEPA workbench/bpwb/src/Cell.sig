(*
  File: Cell.sig

  Functions for processing cells.    
*)
signature Cell = 
sig
  val allCells : PepaNets.Component -> PepaNets.Component list

  val equivNotEqual : 
      PepaNets.Component -> PepaNets.Component -> bool

  val fillCell : 
      PepaNets.Component * PepaNets.Component
                 -> PepaNets.Component list

  val vacateCell : 
      PepaNets.Component * PepaNets.Component
                   -> bool * PepaNets.Component list     

  val hasVacantCellOfType : 
      { term : PepaNets.Component, ofType : PepaNets.Component } -> bool 
end;
