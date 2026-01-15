open! Core
open! Hardcaml
open! Hardcaml_demo_project

module type S = Rtl_gen.S

let rtl_command (module Comp : S) ~name =
  Command.basic
    ~summary:""
    [%map_open.Command
      let () = return () in
      fun () -> print_endline @@ Rtl_gen.generate_rtl (module Comp) ~name]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:""
       [ "d1p1-logic", rtl_command (module D1_logic.Part1) ~name:"d1p1_logic_top"
       ; "d1p2-logic", rtl_command (module D1_logic.Part2) ~name:"d1p2_logic_top"
       ; ( "d1p1-byte"
         , rtl_command (module D1_byte.Make (D1_logic.Part1)) ~name:"d1_byte_top" )
       ; ( "d1p2-byte"
         , rtl_command (module D1_byte.Make (D1_logic.Part2)) ~name:"d1_byte_top" )
       ])
;;
