open! Core
open! Hardcaml

module type S = sig
  module I : Interface.S
  module O : Interface.S

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

let generate_rtl (module Comp : S) ~name =
  let module C = Circuit.With_interface (Comp.I) (Comp.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name (Comp.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  rtl
;;
