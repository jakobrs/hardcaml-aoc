open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness

let ( <--. ) = Bits.( <--. )

type dir =
  [ `L
  | `R
  ]

let samples : (dir * int) list list =
  [ [ `L, 68; `L, 30; `R, 48; `L, 5; `R, 60; `L, 55; `L, 1; `L, 99; `R, 14; `L, 82 ] ]
;;

module Make (M : Hardcaml_demo_project.Sig.Component) = struct
  module Harness = Cyclesim_harness.Make (M.I) (M.O)

  let run_sample sample (sim : Harness.Sim.t) =
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let cycle ?n () = Cyclesim.cycle ?n sim in
    (* Helper function for inputting one value *)
    let feed_input dir amt =
      inputs.direction <--. if Poly.( = ) dir `L then 1 else 0;
      inputs.amount <--. amt;
      inputs.data_in_valid := Bits.vdd;
      cycle ();
      inputs.data_in_valid := Bits.gnd;
      cycle ()
    in
    (* Reset the design *)
    inputs.clear := Bits.vdd;
    cycle ();
    inputs.clear := Bits.gnd;
    cycle ();
    (* Pulse the start signal *)
    inputs.start := Bits.vdd;
    cycle ();
    inputs.start := Bits.gnd;
    (* Input some data *)
    List.iter sample ~f:(fun (dir, amt) -> feed_input dir amt);
    inputs.finish := Bits.vdd;
    cycle ();
    inputs.finish := Bits.gnd;
    cycle ();
    (* Wait for result to become valid *)
    while not (Bits.to_bool !(outputs.count.valid)) do
      cycle ()
    done;
    let count = Bits.to_unsigned_int !(outputs.count.value) in
    print_s [%message "Result" (count : int)];
    (* Show in the waveform that [valid] stays high. *)
    cycle ~n:2 ()
  ;;

  (* The [waves_config] argument to [Harness.run] determines where and how to save waveforms
      for viewing later with a waveform viewer. The commented examples below show how to save
      a waveterm file or a VCD file. *)
  let waves_config = Waves_config.no_waves

  let test_simple () =
    Harness.run_advanced
      ~waves_config
      ~create:M.hierarchical
      (run_sample (List.nth_exn samples 0))
  ;;

  let test_waves () =
    (* For simple tests, we can print the waveforms directly in an expect-test (and use the
        command [dune promote] to update it after the tests run). This is useful for quickly
        visualizing or documenting a simple circuit, but limits the amount of data that can
        be shown. *)
    let display_rules =
      [ Display_rule.port_name_matches
          ~wave_format:(Bit_or Unsigned_int)
          (Re.compile (Re.Glob.glob "d1*"))
      ]
    in
    Harness.run_advanced
      ~create:M.hierarchical
      ~trace:`All_named
      ~print_waves_after_test:(fun waves ->
        Waveform.print
          ~display_rules
            (* [display_rules] is optional, if not specified, it will print all named
                signals in the design. *)
          ~signals_width:30
          ~display_width:130
          ~wave_width:1
          (* [wave_width] configures how many chars wide each clock cycle is *)
          waves)
      (run_sample (List.nth_exn samples 0))
  ;;
end

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(* |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(* |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

module D1p1_logic = Hardcaml_demo_project.D1_logic
module Mp1 = Make (D1p1_logic)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Mp1.test_simple ();
  [%expect {| (Result (count 3)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  Mp1.test_waves ();
  [%expect
    {|
      (Result (count 3))
      ┌Signals─────────────────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
      │                            ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────────│
      │d1_logic$cur                ││ 0          │50 │82     │52     │0      │95     │55     │0      │99     │0      │14     │32       │
      │                            ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────────│
      │                            ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────────┬───┬───────────┬───┬───┬───┬─────────│
      │d1_logic$cur_temp           ││ 0          │494│0  │52 │0  │100│0  │507│0  │155│0          │511│0          │14 │0  │444│0        │
      │                            ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────────┴───┴───────────┴───┴───┴───┴─────────│
      │                            ││────────────┬───┬───┬───┬───────────┬───┬───┬───┬───────────┬───┬───────────┬───┬───┬───┬─────────│
      │d1_logic$cur_temp_post_mod  ││ 0          │82 │0  │52 │0          │95 │0  │55 │0          │99 │0          │14 │0  │32 │0        │
      │                            ││────────────┴───┴───┴───┴───────────┴───┴───┴───┴───────────┴───┴───────────┴───┴───┴───┴─────────│
      │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────────────│
      │d1_logic$i$amount           ││ 0          │68     │30     │48     │5      │60     │55     │1      │99     │14     │82           │
      │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────────────│
      │d1_logic$i$clear            ││────┐                                                                                             │
      │                            ││    └─────────────────────────────────────────────────────────────────────────────────────────────│
      │d1_logic$i$clock            ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
      │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
      │d1_logic$i$data_in_valid    ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐         │
      │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────────│
      │d1_logic$i$direction        ││            ┌───────────────┐       ┌───────┐       ┌───────────────────────┐       ┌─────────────│
      │                            ││────────────┘               └───────┘       └───────┘                       └───────┘             │
      │d1_logic$i$finish           ││                                                                                            ┌───┐ │
      │                            ││────────────────────────────────────────────────────────────────────────────────────────────┘   └─│
      │d1_logic$i$start            ││        ┌───┐                                                                                     │
      │                            ││────────┘   └─────────────────────────────────────────────────────────────────────────────────────│
      │d1_logic$o$count$valid      ││                                                                                                ┌─│
      │                            ││────────────────────────────────────────────────────────────────────────────────────────────────┘ │
      │                            ││────────────────────────────────┬───────────────────────┬───────────────┬─────────────────────────│
      │d1_logic$o$count$value      ││ 0                              │1                      │2              │3                        │
      │                            ││────────────────────────────────┴───────────────────────┴───────────────┴─────────────────────────│
      └────────────────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
      |}]
;;

module D1p2_logic = Hardcaml_demo_project.D1p2_logic
module Mp2 = Make (D1p2_logic)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Mp2.test_simple ();
  [%expect {| (Result (count 3)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  Mp2.test_waves ();
  [%expect
    {|
      (Result (count 3))
      ┌Signals─────────────────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
      │                            ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────────│
      │d1_logic$cur                ││ 0          │50 │82     │52     │0      │95     │55     │0      │99     │0      │14     │32       │
      │                            ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────────│
      │                            ││────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────────┬───┬───────────┬───┬───┬───┬─────────│
      │d1_logic$cur_temp           ││ 0          │494│0  │52 │0  │100│0  │507│0  │155│0          │511│0          │14 │0  │444│0        │
      │                            ││────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────────┴───┴───────────┴───┴───┴───┴─────────│
      │                            ││────────────┬───┬───┬───┬───────────┬───┬───┬───┬───────────┬───┬───────────┬───┬───┬───┬─────────│
      │d1_logic$cur_temp_post_mod  ││ 0          │82 │0  │52 │0          │95 │0  │55 │0          │99 │0          │14 │0  │32 │0        │
      │                            ││────────────┴───┴───┴───┴───────────┴───┴───┴───┴───────────┴───┴───────────┴───┴───┴───┴─────────│
      │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────────────│
      │d1_logic$i$amount           ││ 0          │68     │30     │48     │5      │60     │55     │1      │99     │14     │82           │
      │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────────────│
      │d1_logic$i$clear            ││────┐                                                                                             │
      │                            ││    └─────────────────────────────────────────────────────────────────────────────────────────────│
      │d1_logic$i$clock            ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
      │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
      │d1_logic$i$data_in_valid    ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐         │
      │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────────│
      │d1_logic$i$direction        ││            ┌───────────────┐       ┌───────┐       ┌───────────────────────┐       ┌─────────────│
      │                            ││────────────┘               └───────┘       └───────┘                       └───────┘             │
      │d1_logic$i$finish           ││                                                                                            ┌───┐ │
      │                            ││────────────────────────────────────────────────────────────────────────────────────────────┘   └─│
      │d1_logic$i$start            ││        ┌───┐                                                                                     │
      │                            ││────────┘   └─────────────────────────────────────────────────────────────────────────────────────│
      │d1_logic$o$count$valid      ││                                                                                                ┌─│
      │                            ││────────────────────────────────────────────────────────────────────────────────────────────────┘ │
      │                            ││────────────────────────────────┬───────────────────────┬───────────────┬─────────────────────────│
      │d1_logic$o$count$value      ││ 0                              │1                      │2              │3                        │
      │                            ││────────────────────────────────┴───────────────────────┴───────────────┴─────────────────────────│
      └────────────────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
      |}]
;;
