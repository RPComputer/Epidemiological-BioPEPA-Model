(*
  File: Derivatives.sig

  Given an environment of definitions and a component, compute the one-step
  ((activity, rate), name) transitions leading to new components
*)
signature Derivatives = 
sig

  type Derivative = (BioPEPA.Identifier * BioPEPA.Rate) * BioPEPA.Name * BioPEPA.Component

  datatype Derivation = Firing of Derivative 
                      | Transition of Derivative

  type Derivatives = Derivation list

  val derivative : 
    (BioPEPA.Environment *
       (BioPEPA.Identifier * (BioPEPA.Identifier * BioPEPA.Rate) * BioPEPA.Identifier) list)
       -> BioPEPA.Component
          -> Derivatives

end;
