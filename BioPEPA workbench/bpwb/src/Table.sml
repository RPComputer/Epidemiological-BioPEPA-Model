(* 
 File: Table.sml

 Structure for the table which stores states of a BioPEPA model. 
*)

structure Table :> Table =
struct
  (* A local data structure which stores (number, state) pairs
     in an updateable ordered binary tree.
  *)
  datatype tree = 
    empty 
  | node of tree ref * (int * string * bool ref) * tree ref

  val table as (size, data) = 
      (ref 0, ref empty)

  val tableOut = ref TextIO.stdOut

  (* Write XML headers *)
  fun XMLinitialise () =
      let val timestamp = Date.toString (Date.fromTimeLocal(LocalTime.now()))
      in
          TextIO.output(!tableOut, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
          TextIO.output(!tableOut, "<!-- Generated: " ^ timestamp ^ " -->\n");
          TextIO.output(!tableOut, "<!-- Generator: " ^ Version.banner ^ " -->\n");
          TextIO.output(!tableOut, "<!-- Input was: " ^ 
                                         Files.jobName() ^ 
                                         JobName.showExt() ^" -->\n");
          TextIO.output(!tableOut, "<PEPA_Workbench_State_Table>\n")
      end

  (* Write XML footer *)
  fun XMLfinalise () =
      let val timestamp = Date.toString (Date.fromTimeLocal(LocalTime.now()))
      in
          TextIO.output(!tableOut, "</PEPA_Workbench_State_Table>\n");
          TextIO.output(!tableOut, "<!-- Closed: " ^ timestamp ^ " -->\n")
      end


  fun initialize () = 
      (size := 0; data := empty; 
       if (!Options.solver <> Options.XMLoutput)
       then (tableOut := TextIO.openOut (Files.jobName() ^ ".table"))
       else (tableOut := TextIO.openOut (Files.jobName() ^ ".xmltable");
	     XMLinitialise())
      )

  fun finalize () = 
      let val xml = !Options.solver = Options.XMLoutput
      in if xml then XMLfinalise() else ();
         TextIO.closeOut (!tableOut)
      end

  fun showtree (data) = 
      (case !data of
        empty => []
      | (node (l, (n, d, seen), r)) => showtree l @ [(n, d)] @ showtree r)
  fun show () = showtree (data)
  fun show_size () = !size

  fun extend_table() = 
      (size := !size + 1;
       if !size mod 1000 = 0 then
	   (Reporting.report ("Generated " ^ Int.toString (!size) ^ " states\n");
	    TextIO.flushOut(TextIO.stdOut))
       else ())

  fun printState (n, P) = 
      if (!Options.solver <> Options.XMLoutput) then 
	  let 
	      val _ = Prettyprinter.setMode Prettyprinter.uncompressed
	      val st = Prettyprinter.print P 
	  in 
	      TextIO.output (!tableOut, Int.toString n ^ "\t" ^ st ^ "\n");
	      TextIO.flushOut (!tableOut)
	  end
      else
	  let
	      val st = XMLPrettyprinter.print P
	  in
	      TextIO.output (!tableOut, "<State number=\"" ^ Int.toString n ^ "\">\n");
	      TextIO.output (!tableOut, st);
	      TextIO.output (!tableOut, "</State>\n");
	      TextIO.flushOut (!tableOut)
	  end

  (* abstract code for referring to states *)
  type stateCode = int * string
  datatype stateResult = 
    NotYetSeen of stateCode
  | AlreadySeen of stateCode

  fun addStateResult (P, Pcompressed, data) =
      case !data of
	empty => 
           (extend_table(); 
            data := node(ref empty, (!size, Pcompressed, ref false), ref empty);
            printState (!size, P);
            NotYetSeen (!size, Pcompressed))
      | node (l, (key, Q, ref seen), r) => 
	    if Pcompressed < Q 
	    then addStateResult (P, Pcompressed, l)
	    else if Q < Pcompressed
		 then addStateResult (P, Pcompressed, r)
		 else if seen 
                      then AlreadySeen (key, Pcompressed)
                      else NotYetSeen (key, Pcompressed)

  fun addState P = 
      (Prettyprinter.setMode Prettyprinter.compressed;
       addStateResult (P, Prettyprinter.print P, data))

  fun markAsSeen (P, data) =
      case !data of
	empty => 
           (Error.internal_error "invalid state code forged")
      | node (l, (key, Q, seen), r) => 
	    if P < Q 
	    then markAsSeen (P, l)
	    else if Q < P 
		 then markAsSeen (P, r)
		 else seen := true

  fun markSeen (_, P) = markAsSeen (P, data)

  fun addSt (P, data) =
      case !data of
	empty => 
           (extend_table(); 
            data := node(ref empty, (!size, P, ref false), ref empty))
      | node (l, (_, Q, _), r) => 
	    if P < Q 
	    then addSt (P, l)
	    else if Q < P 
		 then addSt (P, r)
		 else ()

  fun addifmissing P = addSt (P, data)

  fun getCode P = 
     case addState P of
	 AlreadySeen (key, _) => key
       | NotYetSeen (key, _) => key

  fun getc x (empty)    = (0, false)
    | getc x (node (l, (m, h, _), r)) = 
      if x < h then getc x (!l) else 
      if x > h then getc x (!r) else (m, true)

  fun getcode x = getc x (!data)
end;
