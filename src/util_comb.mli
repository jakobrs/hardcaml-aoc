open! Core
open! Hardcaml

val widen : w:int -> Signal.t -> Signal.t
val mul : Signal.t -> ?shift:int -> int -> Signal.t
