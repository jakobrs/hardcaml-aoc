open! Core
open! Hardcaml
open! Hardcaml_demo_project

module type S = sig
  module I : Interface.S
  module O : Interface.S

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

let generate_rtl (module Comp : S) ~name () =
  let module C = Circuit.With_interface (Comp.I) (Comp.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name (Comp.hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let rtl_command (module Comp : S) ~name =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> generate_rtl (module Comp) ~name ()]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "d1p1-logic", rtl_command (module D1_logic.Part1) ~name:"d1p1_logic_top"
       ; "d1p2-logic", rtl_command (module D1_logic.Part2) ~name:"d1p2_logic_top"
       ; "d1-byte", rtl_command (module D1_byte) ~name:"d1_byte_top"
       ])
;;
