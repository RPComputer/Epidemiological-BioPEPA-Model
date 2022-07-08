(*
   File: Alphabets.sig
 
   Calculate alphabets of components
*)
signature Alphabets =
sig
  val alphabetOf : PepaNets.Environment -> 
                        PepaNets.Component -> 
                             PepaNets.Identifier list
end;
