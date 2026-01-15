(* An example design that takes a series of input values and calculates the range between
   the largest and smallest one. *)

(* We generally open Core and Hardcaml in any source file in a hardware project. For
   design source files specifically, we also open Signal. *)
open! Core
open! Hardcaml
open! Signal
open Util_comb

let num_bits = D1_logic.num_bits
let num_count_bits = D1_logic.num_count_bits

(* Every hardcaml module should have an I and an O record, which define the module
   interface. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits 8]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* With_valid.t is an Interface type that contains a [valid] and a [value] field. *)
      count : 'a With_valid.t [@bits num_count_bits]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_direction
    | Accepting_amount
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module Make (Part : D1_logic.Part) = struct
  module I = I
  module O = O

  let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t)
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
    let%hw_var direction = Variable.reg spec ~width:1 in
    (* This is a kind of ring buffer thing, however, just doing it manually seems fine for now *)
    let%hw_var hundreds = Variable.reg spec ~width:5 in
    let%hw_var tens = Variable.reg spec ~width:5 in
    let%hw_var ones = Variable.reg spec ~width:5 in
    let%hw_var amount = Variable.wire ~default:(zero num_bits) () in
    let inner_data_is_valid = Variable.wire ~default:gnd () in
    (* We don't need to name the range here since it's immediately used in the module
     output, which is automatically named when instantiating with [hierarchical] *)
    let { D1_logic.O.count } =
      Part.hierarchical
        scope
        { D1_logic.I.clock
        ; clear
        ; start
        ; direction = direction.value
        ; hundreds = hundreds.value
        ; amount = amount.value
        ; finish
        ; data_in_valid = inner_data_is_valid.value
        }
    in
    compile
      [ sm.switch
          [ Idle, [ when_ start [ sm.set_next Accepting_direction ] ]
          ; ( Accepting_direction
            , [ when_
                  data_in_valid
                  [ direction <-- (data_in ==:. Char.to_int 'L')
                  ; hundreds <--. 0
                  ; tens <--. 0
                  ; ones <--. 0
                  ; sm.set_next Accepting_amount
                  ]
              ; when_ finish [ sm.set_next Done ]
              ] )
          ; ( Accepting_amount
            , [ when_
                  data_in_valid
                  [ if_
                      (data_in ==:. Char.to_int '\n')
                      [ amount
                        <-- mul (widen ~w:num_bits tens.value) 10
                            +: widen ~w:num_bits ones.value
                      ; inner_data_is_valid <-- vdd
                      ; sm.set_next Accepting_direction
                      ]
                      [ hundreds <-- tens.value
                      ; tens <-- ones.value
                      ; ones <-- sel_bottom (data_in -:. Char.to_int '0') ~width:5
                      ]
                  ]
              ] )
          ; Done, [ when_ finish [ sm.set_next Idle ] ]
          ]
      ];
    (* [.value] is used to get the underlying Signal.t from a Variable.t in the Always DSL. *)
    { count }
  ;;

  (* The [hierarchical] wrapper is used to maintain module hierarchy in the generated
   waveforms and (optionally) the generated RTL. *)
  let hierarchical scope =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"d1_byte" create
  ;;
end
