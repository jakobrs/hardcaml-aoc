open! Core
open! Hardcaml
open! Signal

let widen ~w a = zero (w - width a) @: a

let rec mul a ?(shift = 0) = function
  | 0 -> zero (width a)
  | 1 -> sll a ~by:shift
  | by when by mod 2 = 0 -> mul a ~shift:(shift + 1) (by / 2)
  | by -> sll a ~by:shift +: mul a ~shift:(shift + 1) (by / 2)
;;
