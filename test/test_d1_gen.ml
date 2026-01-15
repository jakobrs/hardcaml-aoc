open! Core
open! Hardcaml
open! Hardcaml_demo_project

let%test_unit "Test generation part 1 logic version" =
  ignore @@ Rtl_gen.generate_rtl (module D1_logic.Part1) ~name:"d1p1_logic"
;;

let%test_unit "Test generation part 2 logic version" =
  ignore @@ Rtl_gen.generate_rtl (module D1_logic.Part2) ~name:"d1p2_logic"
;;

let%test_unit "Test generation part 1 ascii version" =
  ignore @@ Rtl_gen.generate_rtl (module D1_byte.Make (D1_logic.Part1)) ~name:"d1p2_logic"
;;

let%test_unit "Test generation part 2 ascii version" =
  ignore @@ Rtl_gen.generate_rtl (module D1_byte.Make (D1_logic.Part2)) ~name:"d1p2_logic"
;;
