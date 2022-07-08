(* 
 File: Model.sml

 Model component handling for the Bio-PEPA Workbench.
*)

structure Model :> Model
= struct

  fun repaint (Lexer.Ident s) = "$"^Int.toString(Species.lookup s + 2)
    | repaint (Lexer.Float s) = s
    | repaint (Lexer.Symbol c) = str c

  fun sqToRound #"[" = "("
    | sqToRound #"]" = ")"
    | sqToRound c = str c

  fun toGnuplotString def = 
     let 
         val def = String.translate sqToRound def
         val tokens = Lexer.analyse (explode def)
         val result = String.concat (map repaint tokens)
      in result
     end

end;
