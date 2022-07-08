(*
  File: DerivativeSets.sml
 
  Compute the DerivativeSets for components.
*)
structure DerivativeSets :> DerivativeSets =
struct

    val derivativeSets = ref [] : (BioPEPA.Identifier * BioPEPA.Component list) list ref

    (* Compute the declared component identifiers *)
    fun identifiers (BioPEPA.CONSTS (I, _, P)) =
        I :: identifiers P
      | identifiers _ = []

    (* Compute the declaration environment *)
    fun environment (BioPEPA.CONSTS (I, C, P)) =
        (I, C) :: environment P
      | environment _ = []

    fun lookup (I, []) = Error.fatal_error ("Could not find definition of " ^ I)
      | lookup (I, (I', C) :: t) = if I = I' then C else lookup (I, t)

    fun member (I, []) = false
      | member (I, h::t) = I = h orelse member (I, t)

    (* Compute the derivatives of *sequential* component I *)
    fun derivatives M I =
        let val E = environment M
            val Idefn = lookup (I, E)
         in deriv E [BioPEPA.VAR I] Idefn
        end
    and deriv E seen (C as BioPEPA.INCREASES ((a,r), P)) =
           if member (C, seen) then seen
           else deriv E (C :: seen) P
      | deriv E seen (C as BioPEPA.DECREASES ((a,r), P)) =
           if member (C, seen) then seen
           else deriv E (C :: seen) P
      | deriv E seen (C as BioPEPA.CHOICE (P, Q)) =
           if member (C, seen) then seen
           else let val seenP = deriv E (C :: seen) P
                 in deriv E seenP Q
                end
      | deriv E seen (C as BioPEPA.VAR I) =
           if member (C, seen) then seen
           else deriv E (C :: seen) (lookup (I, E))
      | deriv E seen _ = seen

    fun zip (h1::t1, h2::t2) = (h1, h2) :: zip (t1, t2)
      | zip _ = []

    (* Compute the identifiers of components and then
       compute a list of derivatives for each component.
       Then store the result *)
    fun compute M = 
        let val id = identifiers M
         in derivativeSets := zip (id, map (derivatives M) id)
        end

    fun show () =
        let fun componentToString C = 
	        (Prettyprinter.setMode Prettyprinter.uncompressed;
		 "\t" ^ Prettyprinter.print C ^ "\n")
            fun toString (I, C) = I ^ " --> " ^
                String.concat (map componentToString C)
            fun pr s = TextIO.output(TextIO.stdOut, s ^ "\n")
         in List.app (pr o toString) (!derivativeSets)
        end

    fun ds (BioPEPA.VAR I) = lookup (I, !derivativeSets)
      | ds _ = Error.fatal_error "Cannot compute derivative set of non-identifiers"

    fun in_ds (C, I) = member (C, ds (BioPEPA.VAR I))

end;
