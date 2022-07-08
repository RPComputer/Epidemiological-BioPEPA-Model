(*
  File: Aggregate.sig

  Provide functions to aggregate PEPA net transitions based on 
  similarity of terms.
*)

signature Aggregate =
sig

    val sort_component : PepaNets.Component -> PepaNets.Component
    val sort_components : PepaNets.Component list -> PepaNets.Component list
    val sort_pairs : Derivatives.Derivatives -> Derivatives.Derivatives

    val minimize : Derivatives.Derivatives -> Derivatives.Derivatives

end;

