(*
  File: Cell.sml
 
  Functions for processing cells.
*)
structure Cell :> Cell =
struct

    fun allCells (PepaNets.CELL (_, SOME C)) = [C]
      | allCells (PepaNets.CELL (_, NONE)) = []
      | allCells (PepaNets.PREFIX (_, P)) = allCells P
      | allCells (PepaNets.CHOICE (P, Q)) = allCells P @ allCells Q
      | allCells (PepaNets.COOP (P, _)) = List.concat (map allCells P)
      | allCells (PepaNets.HIDING (P, _)) = allCells P
      | allCells _ = []

    local
      fun eq (PepaNets.CELL(P1, SOME C1)) (PepaNets.CELL(P2, SOME C2))  =  P1 = P2 andalso C1 <> C2 
	| eq (PepaNets.CELL (P1, NONE))   (PepaNets.CELL (P2, NONE))    =  P1 = P2
	| eq (PepaNets.PREFIX (a1, P1))   (PepaNets.PREFIX (a2, P2))    =  a1 = a2 andalso eq P1 P2
	| eq (PepaNets.CHOICE (P1, Q1))   (PepaNets.CHOICE (P2, Q2))    =  eq P1 P2 andalso eq Q1 Q2
	| eq (PepaNets.COOP (P1, L1))     (PepaNets.COOP (P2, L2))      =  allEq P1 P2 andalso L1 = L2
	| eq (PepaNets.HIDING (P1, L1))   (PepaNets.HIDING (P2, L2))    =  eq P1 P2 andalso L1 = L2
	| eq (PepaNets.VAR I1)            (PepaNets.VAR I2)             =  I1 = I2
	| eq _                            _                             =  false
      and allEq [] [] = true
        | allEq (h1::t1) (h2::t2) = eq h1 h2 andalso allEq t1 t2
        | allEq _ _ = false
    in
      val equivNotEqual = eq
    end

    fun fillCell (C, PepaNets.CELL (P, NONE)) = 
          if DerivativeSets.in_ds (C, P) 
          then [PepaNets.CELL (P, SOME C)]
          else []
      | fillCell (C, PepaNets.CELL (P, SOME _)) = []
      | fillCell (C, PepaNets.PREFIX (a as (alpha, rate), P)) = 
          map (fn P' => PepaNets.PREFIX (a, P')) (fillCell (C, P))
      | fillCell (C, PepaNets.CHOICE (P, Q)) = 
          let val P' = fillCell (C, P)
              val Q' = fillCell (C, Q)
           in map (fn P' => PepaNets.CHOICE (P', Q)) P'
            @ map (fn Q' => PepaNets.CHOICE (P, Q')) Q'
          end
      | fillCell (C, Comp as (PepaNets.COOP (P, L))) = fillCellList (C, [], Comp)
      | fillCell (C, PepaNets.HIDING (P, L)) = 
          map (fn P' => PepaNets.HIDING (P', L)) (fillCell (C, P))
      | fillCell _ = []          
    and fillCellList (C, seen, PepaNets.COOP ([], L)) = []
      | fillCellList (C, seen, PepaNets.COOP (P::t, L)) =
          let val P' = fillCell (C, P)
           in map (fn P' => PepaNets.COOP (seen @ [P'] @ t, L)) P'
          end @ fillCellList (C, seen @ [P], PepaNets.COOP (t, L))
      | fillCellList _ = Error.fatal_error "implementation bug: filling cell lists"

    fun vacateCell (C, T as PepaNets.CELL (P, SOME P')) = 
            if C = P'
            then (true, [PepaNets.CELL (P, NONE)])
            else (false, [T])
      | vacateCell (C, T as PepaNets.CELL (P, NONE)) = (false, [T])
      | vacateCell (C, PepaNets.PREFIX (a as (alpha, rate), P)) = 
            let val (result, P') = vacateCell (C, P)
             in (result, map (fn P' => PepaNets.PREFIX (a, P')) P')
            end
      | vacateCell (C, T as PepaNets.CHOICE (P, Q)) = 
          let val (resultP, P') = vacateCell (C, P)
              val (resultQ, Q') = vacateCell (C, Q)
              val left  = map (fn P' => PepaNets.CHOICE (P', Q)) P'
              val right = map (fn Q' => PepaNets.CHOICE (P, Q')) Q'
           in if resultP
              then if resultQ
                   then (true, left @ right)
                   else (true, left)
              else if resultQ
                   then (true, right)
                   else (false, [T])
          end
      | vacateCell (C, PepaNets.HIDING (P, L)) = 
            let val (result, P') = vacateCell (C, P)
             in (result, map (fn P' => PepaNets.HIDING (P', L)) P')
            end
      | vacateCell (C, T as PepaNets.COOP (P, L)) = 
          let val results = vacateCellList (C, [], T)
              val successes = List.filter (fn (b, _) => b) results
           in if successes <> [] 
              then (true, List.concat (map #2 successes))
              else (false, [T])
          end
      | vacateCell _ = (false, [])
    and vacateCellList (C, seen, T as PepaNets.COOP (P :: t, L)) = 
          let
            val (resultP, P') = vacateCell (C, P)
            val front = map (fn P' => PepaNets.COOP (seen @ (P' :: t), L)) P'
          in 
            if resultP
            then (true, front)
            else (false, [T])
          end :: vacateCellList (C, seen @ [P], PepaNets.COOP (t, L))
      | vacateCellList _ = []

    fun hasVacantCellOfType { ofType = C, term = (T as PepaNets.CELL (P, SOME P')) } = false
      | hasVacantCellOfType { ofType = C, term = (T as PepaNets.CELL (P, NONE)) } = DerivativeSets.in_ds (C, P)
      | hasVacantCellOfType { ofType = C, term = (PepaNets.PREFIX (a as (alpha, rate), P)) } = 
        hasVacantCellOfType { ofType = C, term = P }
      | hasVacantCellOfType { ofType = C, term = (PepaNets.CHOICE (P, Q)) } = 
        hasVacantCellOfType { ofType = C, term = P } orelse hasVacantCellOfType { ofType = C, term = Q }
      | hasVacantCellOfType { ofType = C, term = (PepaNets.COOP (P, L)) } = 
        List.exists (fn p => hasVacantCellOfType { ofType = C, term = p }) P
      | hasVacantCellOfType { ofType = C, term = (PepaNets.HIDING (P, L)) } = 
        hasVacantCellOfType { ofType = C, term = P }
      | hasVacantCellOfType _ = false

end;
