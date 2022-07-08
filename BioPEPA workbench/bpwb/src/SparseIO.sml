load "Int";
load "Real";
load "Error";

structure SparseIO =
struct

   val reference = {
      author =       "W.H. Press and S.A. Teukolsky and W.T. Vetterling and B.F. Flannery",
      title =        "Numerical Recipes in C: The Art of Scientific Computing",
      chapter =      "2.  Solution of Linear Algebraic Equations",
      section =      "2.7.  Sparse Linear Systems",
      subsection =   "Indexed Storage of Sparse Matrices",
      publisher =    "Cambridge University Press",
      year =         1992,
      edition =      "Second",
      url =          "http://www.nr.com/"
   }

   (* This function reads an output file written by the PEPA Workbench
      and builds the equivalent row-indexed sparse storage mode vectors
      sa and ija.  
   *)

   val nnz = ref 0

   fun readFile n (m as nmax) s = 
   let 
       val is = TextIO.openIn s
       val line = ref [] : string list ref
       val sa = Array.array(m + 1, 0.0)
       val ija = Array.array(m + 1, 0)
       val data = Array.array(n + 1, [])
       val k = ref 0
       val prev_i = ref 1
   in
       nnz := 0;
       while not (TextIO.endOfStream is) do (
          line := readTokens is;
          Array.update(data, row line, 
            insert (column line, rate line) (Array.sub (data, row line)))
       );
       ++nnz; (* Location N + 1 of sa is not used *)
       print (Int.toString (!nnz));

       Array.appi (fn (i, l) =>
         Array.update(sa, i, find i l)
       ) (data, 1, SOME n);

       TextIO.closeIn is;
       Array.update(ija, 1, n+2);
       k := n + 1;

       Array.appi (fn (i, l) => (
         List.app (fn (j, r) => (
              if (i <> j) then (
                 if (++k > nmax) 
                 then Error.internal_error "sparse I/O, nmax too small"
                 else ();
                 Array.update(sa, !k, r);
                 Array.update(ija, !k, j)
              ) else ()
            )) l;
         Array.update(ija, i+1, !k+1)
       )) (data, 1, SOME n);

       (sa, ija)
   end
   and readTokens is = String.tokens Char.isPunct (TextIO.inputLine is)
   and diag line = row line
   and item n line = valOf (Int.fromString (List.nth (!line, n)))
   and row line = item 1 line
   and column line = item 2 line
   and rate line = valOf (Real.fromString (List.nth (!line, 6)))
   and ++ r = (r := !r + 1; !r)
   and sort [] = []
     | sort (h::t) = insert h (sort t)
   and insert (i1, r1) [] = (++nnz ; [(i1, r1)])
     | insert (i1, r1) ((i2, r2) :: t) =
       if i1 = i2 
       then (i1, r1 + r2) :: t
       else if i1 < i2 
            then (++nnz ; (i1, r1) :: (i2, r2) :: t)
            else (i2, r2) :: insert (i1, r1) t
   and find i [] = (Error.warning "missing diagonal element"; 0.0)
     | find i ((i2, r) :: t) = if i = i2 then r else find i t
   and test n m s = 
       let
          val (sa, ija) = readFile n m s
          fun prList [] = ""
            | prList [p] = pr p
            | prList (h::t) = pr h ^ ", " ^ prList t
          and pr (n, s) = "(" ^ Int.toString n ^ ", " ^ Real.toString s ^ ")"
       in Array.appi (fn (i, r) =>
             TextIO.output (TextIO.stdOut, "sa[" ^ Int.toString i ^ "] = " ^ Real.toString r ^ "\n")
          ) (sa, 1, SOME m);
          Array.appi (fn (i, n) =>
             TextIO.output (TextIO.stdOut, "ija[" ^ Int.toString i ^ "] = " ^ Int.toString n ^ "\n")
          ) (ija, 1, SOME m)
       end

end;
