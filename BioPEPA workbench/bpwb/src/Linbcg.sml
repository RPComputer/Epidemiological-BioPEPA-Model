(**
 * Steady state solution through the use of the Linear Biconjugate Gradient 
 * Method.  Solves Ax = b for x[1..n], given b[1..n], by the iterative 
 * biconjugate gradient method.  
 *)

structure Linbcg =
struct

   val reference = {
      author =       "W.H. Press and S.A. Teukolsky and W.T. Vetterling and B.F. Flannery",
      title =        "Numerical Recipes in C: The Art of Scientific Computing",
      chapter =      "2.  Solution of Linear Algebraic Equations",
      section =      "2.7.  Sparse Linear Systems",
      publisher =    "Cambridge University Press",
      year =         1992,
      edition =      "Second",
      url =          "http://www.nr.com/"
   }

   open Array;
   val == = Real.== 
   val != = Real.!= 
   infix == != 

   fun ++ r = r := !r + 1;
   fun += (r, x: real) = r := !r + x;
   fun -= (r, x: real) = r := !r - x;
   fun /= (r, x: real) = r := !r / x;
   infix += -= /= 

   (* Epsilon: a small number *)
   val EPS = 1.0E~14


   fun dsprsax(sa: real array, ija: int array, x: real array, b: real array, n: int) = (

      if (sub(ija,1) <> n + 2) then
         Error.internal_error ("Mismatch between vector and matrix in dsprsax")
      else ();

      appi (fn (i, _) => 
      (
         update(b, i, sub (sa, i) * sub (x, i));
         appi (fn (k, _) =>
         (
            update (b, i, sub(b, i) + sub(sa, k) * sub(x, sub (ija, k)))
         )) (b, sub(ija, i), SOME (sub(ija, i + 1) - 1))
      )) (b, 1, SOME n) 
   )


   fun dsprstx(sa: real array, ija: int array, x: real array, b: real array, n: int) = (

      if (sub (ija, 1) <> n + 2) then
         Error.internal_error ("Mismatch between vector and matrix in dsprstx")
      else ();

      modifyi (fn (i, _) => sub(sa, i) * sub (x, i)) (b, 1, SOME n);
      appi (fn (i, _) => 
         appi (fn (k, _) =>
         (
            let val j = sub (ija, k)
            in update (b, j, sub(b,j) + sub(sa,k) * sub(x,i))
            end
         )) (b, sub (ija, i), SOME (sub (ija, i + 1) - 1)) 
      ) (b, 1, SOME n)
   )



   (**
    * Compute one of two norms for a vector sx[1..n], as signaled by itol
    *)
   fun snrm(n: int, sx: real array, itol: int) =
      if (itol <= 3) then
      (
         (* Vector magnitude norm *)
         let val ans = ref 0.0
         in appi (fn (i, _) => 
                ans += sub(sx,i) * sub(sx,i)
            ) (sx, 1, SOME n);
            Math.sqrt(!ans)
         end
      ) 
      else 
      (
         (* Largest component norm *)
         let val isamax = ref 1
         in
            appi (fn (i, _) => 
               if (Real.abs(sub(sx,i)) > Real.abs(sub(sx,!isamax))) 
               then isamax := i 
               else ()
            ) (sx, 1, SOME n);
            Real.abs(sub(sx, !isamax))
         end
      )

   
  (**
   * Steady state solution through the use of the Linear Biconjugate Gradient
   * Method.  Solves Ax = b for x[1..n], given b[1..n], by the iterative
   * biconjugate gradient method.  
   *
   *  ija   : index of the values in sa for sparse format of matrix
   *  sa    : values within the matrix without the zero elements
   *
   *  n     : The matrix dimension
   *  b     : The vector which is the answer for A*x
   *  x     : On input x[1..n] should be set to an initial guess of the solution
   *          (or all zeros); on output the correct answer should be given back
   *  itol  :  itol is 1,2,3, or 4 depending on which convergence test is specified
   *  tol   : the desired convergence tolerance
   *  itmax : the maximum number of allowed iterations
   *
   *  iter  : the number of iterations actually taken (returned from the function)
   *  err   : the estimated error (returned from the function)
   *)

   fun linbcg (sa: real array, ija: int array)
              (n: int, b: real array, x: real array, itol: int, tol: real, 
               itmax: int, iter: int ref, err: real ref) =
   let
     (**
      * Computes the product of either A or its transpose on a vector
      *) 
      fun atimes(n: int, x: real array, r: real array, itrnsp: bool) = 
	 if (itrnsp) 
         then dsprstx(sa, ija, x, r, n) 
         else dsprsax(sa, ija, x, r, n);


     (**
      * solves ~Ax=b or ~A^T x=b for some preconditioner matrix ~A (possibly 
      * the trivial diagonal part of A) 
      *
      * The matrix ~A is the diagonal part of A, stored in the first n elements 
      * of sa.  Since the transpose matrix has the same diagonal, the flag itrnsp 
      * is not used.
      *)
      fun asolve(n: int, b: real array, x: real array, itrnsp: bool) = 
	 modifyi (fn (i, _) => if sub(sa,i) != 0.0 
			       then sub(b,i) / sub(sa, i) 
			       else sub(b, i)) (x, 1, SOME n);

      exception break
      exception continue

      val p  = Array.array(n + 1, 0.0)
      val pp = Array.array(n + 1, 0.0)
      val r  = Array.array(n + 1, 0.0)
      val rr = Array.array(n + 1, 0.0)
      val z  = Array.array(n + 1, 0.0)
      val zz = Array.array(n + 1, 0.0)

      val znrm = ref 0.0;
      val bnrm = ref 0.0;
      val zm1nrm = ref 0.0;
      val ak = ref 0.0;
      val akden = ref 0.0;
      val bk = ref 0.0;
      val bkden = ref 0.0;
      val bknum = ref 0.0;
      val xnrm = ref 0.0;
      val dxnrm = ref 0.0;
   in
      iter := 0;

      (* Input to atimes is x[1..n], output is r[1..n] *)
      atimes(n, x, r, false);

      appi (fn (j, _) => (
         update (r, j, sub(b,j) - sub(r,j));
         update (rr, j, sub(r,j))
      )) (r, 1, SOME n);
      znrm := 1.0;
      if (itol = 1) then
      (
         bnrm := snrm(n, b, itol);
         asolve(n, r, z, false)
      ) 
      else if (itol = 2) then
      (
         asolve(n, b, z, false);
         bnrm := snrm(n, z, itol);
         asolve(n, r, z, false)
      ) 
      else if (itol = 3 orelse itol = 4) then
      (
         asolve(n, b, z, false);
         bnrm := snrm(n, z, itol);
         asolve(n, r, z, false);
         znrm := snrm(n, z, itol)
      ) 
      else Error.internal_error ("Illegal itol Value");
      
      (* Main while loop *)
      (while (!iter <= itmax) do (
         ++ iter;
         zm1nrm := !znrm;
         asolve(n, rr, zz, true);
         
         bknum := 0.0;
         appi (fn (j, _) =>
            bknum += sub(z,j) * sub(rr,j)
         ) (z, 1, SOME n);

         (* Calculate coefficient bk and direction vectors p and pp *)
         if (!iter = 1) then
         (
            appi (fn (j, _) => (
               update (p, j, sub (z,j));
               update (pp, j, sub (zz,j))
            )) (p, 1, SOME n)
         ) 
         else 
         (
            bk := !bknum / !bkden;
            appi (fn (j, _) => (
               update (p, j, !bk * sub(p,j) + sub(z,j));
               update (pp, j, !bk * sub(pp,j) + sub(zz,j))
            )) (p, 1, SOME n)
         );
         bkden := !bknum;
         (* Calculate coefficient ak, new iterate x and new residuals *)
         atimes(n, p, z, false);
         
         akden := 0.0;
         appi (fn (j, _) => (
             akden += sub(z,j) * sub(pp,j)
         )) (z, 1, SOME n);
         ak := !bknum / !akden;

         atimes(n, pp, zz, true);

         appi (fn (j, _) => (
            update (x, j, sub(x,j) + !ak * sub(p,j));
            update (r, j, sub(r,j) - !ak * sub(z,j));
            update (rr, j, sub(rr,j) - !ak * sub(zz,j))
         )) (x, 1, SOME n);

         asolve(n, r, z, false);
         if (itol = 1) then
         (
            znrm := 1.0;
            err := snrm(n, r, itol) / !bnrm
         ) 
         else if (itol = 2) then
         (
            err := snrm(n, z, itol) / !bnrm
         ) 
         else if (itol = 3 orelse itol = 4) then
         (
            zm1nrm := !znrm;
            znrm := snrm(n, z, itol);
            if (Real.abs(!zm1nrm - !znrm) > EPS * !znrm) then
            (
               dxnrm := Real.abs(!ak) * snrm(n, p, itol);
               err := !znrm / Real.abs(!zm1nrm - !znrm) * !dxnrm
            ) 
            else 
            (
               err := !znrm / !bnrm;
               raise continue
            )
            xnrm := snrm(n, x, itol);
            if (!err <= 0.5 * !xnrm) then err /= !xnrm else 
            (
               err := !znrm / !bnrm;
               raise continue
            )
         ) else ();
         if (!err <= tol) then raise break else ()
       ) handle continue => ()
     ) handle break => ()

  end;


end;
