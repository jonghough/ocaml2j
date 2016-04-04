open Core.Std
open Int64

(* Extracts an 8-byte floating point from the            *)
(* higher and lower 4-bytes, represented as 4-byte ints. *)
(* This is a convoluted way to get the float, but it     *)
(* is the only method I could get to work.               *)
let extract_float (pointer1 : int32) (pointer2 : int32) =
	let high = Int64.of_int32 pointer2 in
	let bits : int = 32 in
	let shifted : int64 = Int64.shift_left high bits in
	let low : int64 = Int64.of_int32 pointer1 in
	let sum : int64 = shifted + low in
	Int64.float_of_bits sum

