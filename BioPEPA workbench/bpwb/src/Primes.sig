(*
  File: Primes.sig

  Prime numbers used to generate well-separated rates.
*)
signature Primes =
sig
  val primes : int list
  val nth : int -> int
end;
