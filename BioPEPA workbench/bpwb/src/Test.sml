structure Test =
struct

 val go = 
  (*    Prettyprinter.print Prettyprinter.uncompressed o  *)
    Semantic.analyse o
    Parser.parse o
    Lexer.analyse o
    Files.readFile

end;
 
