open! Core
open! Hardcaml
open! Signal

(* Number of bits for all state _except_ amount *)
val num_bits : int
val num_count_bits : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; direction : 'a
    ; hundreds : 'a
    ; amount : 'a
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { count : 'a With_valid.t [@bits num_count_bits] } [@@deriving hardcaml]
end

module type Part = sig
  module I = I
  module O = O

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Part1 : Part
module Part2 : Part
