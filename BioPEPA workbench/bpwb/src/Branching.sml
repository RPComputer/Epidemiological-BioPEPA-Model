(*
  File : Branching.sml

  Report the number of derivatives for each state
*)

structure Branching :> Branching =
struct

  fun reportDerivatives (P, derivatives) =
    TextIO.output(TextIO.stdOut, 
      "State " ^ Int.toString(Table.getCode P) ^ " has " ^ number derivatives)
  and number [_] = "1 derivative\n"
    | number d = Int.toString(List.length d) ^ " derivatives\n"

end;
