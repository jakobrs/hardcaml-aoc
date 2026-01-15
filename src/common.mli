open! Hardcaml

val num_bits : int

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; direction : 'a
    ; hundreds : 'a [@bits 16]
    ; amount : 'a [@bits num_bits] (* Should be < 100 *)
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      count : 'a With_valid.t [@bits 16]
    }
  [@@deriving hardcaml]
end

(* module type D1_sol = sig
  module I = I
  module O = O

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end *)
