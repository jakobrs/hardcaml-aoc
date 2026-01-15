open! Core
open! Hardcaml

module type S = sig
  module I : Interface.S
  module O : Interface.S

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

val generate_rtl : (module S) -> name:string -> string
