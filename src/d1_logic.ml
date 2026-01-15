(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal
open Util_comb

(* Number of bits for all state _except_ amount *)
let num_bits = 9
let num_count_bits = 16

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; direction : 'a
    ; hundreds : 'a [@bits 5]
    ; amount : 'a [@bits num_bits] (* Should be < 100 *)
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { count : 'a With_valid.t [@bits num_count_bits] } [@@deriving hardcaml]
end

module type Part = sig
  module I = I
  module O = O

  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module Part1 = struct
  module I = I
  module O = O

  module States = struct
    type t =
      | Idle
      | Accepting_inputs
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create
    scope
    ({ clock; clear; start; finish; direction; amount; hundreds = _; data_in_valid } :
      _ I.t)
    : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm =
      (* Note that the state machine defaults to initializing to the first state *)
      State_machine.create (module States) spec
    in
    (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
      will show up in waveforms. The [_var] version is used when working with the Always
      DSL. *)
    let%hw_var cur = Variable.reg spec ~width:num_bits in
    let%hw_var cur_temp = Variable.wire ~default:(zero num_bits) () in
    let%hw_var cur_temp_post_mod = Variable.wire ~default:(zero num_bits) () in
    (* We don't need to name the range here since it's immediately used in the module
      output, which is automatically named when instantiating with [hierarchical] *)
    let count = Variable.reg spec ~width:16 in
    let count_valid = Variable.wire ~default:gnd () in
    compile
      [ sm.switch
          [ ( Idle
            , [ when_
                  start
                  [ cur <-- of_signed_int ~width:num_bits 50
                  ; sm.set_next Accepting_inputs
                  ]
              ] )
          ; ( Accepting_inputs
            , [ when_
                  data_in_valid
                  [ if_
                      direction
                      [ cur_temp <-- cur.value -: amount ]
                      [ cur_temp <-- cur.value +: amount ]
                  ; cur_temp_post_mod
                    <-- priority_select_with_default
                          ~default:cur_temp.value
                          [ { valid = cur_temp.value <+. 0
                            ; value = cur_temp.value +:. 100
                            }
                          ; { valid = cur_temp.value >=+. 100
                            ; value = cur_temp.value -:. 100
                            }
                          ]
                  ; when_ (cur_temp_post_mod.value ==:. 0) [ count <-- count.value +:. 1 ]
                  ; cur <-- cur_temp_post_mod.value
                  ]
              ; when_ finish [ sm.set_next Done ]
              ] )
          ; Done, [ count_valid <-- vdd; when_ finish [ sm.set_next Idle ] ]
          ]
      ];
    (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
    { count = { value = count.value; valid = count_valid.value } }
  ;;

  (* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
    waveforms and (optionally) the generated RTL. *)
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"d1_logic" create
  ;;
end

module Part2 = struct
  module I = I
  module O = O

  module States = struct
    type t =
      | Idle
      | Accepting_inputs
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create
    scope
    ({ clock; clear; start; finish; direction; hundreds; amount; data_in_valid } : _ I.t)
    : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let sm =
      (* Note that the state machine defaults to initializing to the first state *)
      State_machine.create (module States) spec
    in
    (* let%hw[_var] is a shorthand that automatically applies a name to the signal, which
       will show up in waveforms. The [_var] version is used when working with the Always
       DSL. *)
    let%hw_var cur = Variable.reg spec ~width:num_bits in
    let%hw_var cur_temp = Variable.wire ~default:(zero num_bits) () in
    let%hw_var cur_temp_post_mod = Variable.wire ~default:(zero num_bits) () in
    (* We don't need to name the range here since it's immediately used in the module
       output, which is automatically named when instantiating with [hierarchical] *)
    let count = Variable.reg spec ~width:16 in
    let count_temp = Variable.wire ~default:count.value () in
    let count_valid = Variable.wire ~default:gnd () in
    compile
      [ sm.switch
          [ ( Idle
            , [ when_
                  start
                  [ cur <-- of_signed_int ~width:num_bits 50
                  ; sm.set_next Accepting_inputs
                  ]
              ] )
          ; ( Accepting_inputs
            , [ when_
                  data_in_valid
                  [ if_
                      direction
                      [ cur_temp <-- cur.value -: amount
                      ; cur_temp_post_mod <-- cur_temp.value
                      ; when_
                          (cur_temp.value <+. 0)
                          [ count_temp <-- count.value +:. 1
                          ; cur_temp_post_mod <-- cur_temp.value +:. 100
                          ]
                      ; when_ (cur_temp.value ==:. 0) [ count_temp <-- count.value +:. 1 ]
                      ]
                      [ cur_temp <-- cur.value +: amount
                      ; cur_temp_post_mod <-- cur_temp.value
                      ; when_
                          (cur_temp.value >=+. 100)
                          [ count_temp <-- count.value +:. 1
                          ; cur_temp_post_mod <-- cur_temp.value -:. 100
                          ]
                      ]
                  ; when_ (cur.value ==:. 0) [ count_temp <-- count.value ]
                  ; count <-- count_temp.value +: widen ~w:num_count_bits hundreds
                  ; cur <-- cur_temp_post_mod.value
                  ]
              ; when_ finish [ sm.set_next Done ]
              ] )
          ; Done, [ count_valid <-- vdd; when_ finish [ sm.set_next Idle ] ]
          ]
      ];
    (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
    { count = { value = count.value; valid = count_valid.value } }
  ;;

  (* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
     waveforms and (optionally) the generated RTL. *)
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"d1_logic" create
  ;;
end
