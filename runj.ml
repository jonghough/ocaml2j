open Core.Std
open Callc

(* prints bool *)
let print_bool_element = function
	| JBool i -> printf "%B\n" i;
	()
	
	
(* prints int *)
let print_int_element = function
	| JInt i -> printf "%i\n" i;
	()	
	
	
(* prints float *)	
let print_float_element = function
	| JFloat f -> printf "%f\n" f;
	()	
	
let () = 
	let pt = Callc.setup () in
	Callc.doit pt "isPrime =: 1 p: >: i. 100 ";
	let result = Callc.getit pt "isPrime" in

	match result with
	| Callc.JError e ->
		printf "error %s\n" e
	| Callc.JResult (_,_,_,jr) ->
		match jr with
		| `JBoolArray arr ->
			Array.iter ~f: print_bool_element arr
		| `JIntArray arr ->
			Array.iter ~f: print_int_element arr
		| `JFloatArray arr ->
			Array.iter ~f: print_float_element arr
		| `JString s ->
			Printf.printf "%s\n" s
		| _ -> 
			printf "Unknown datatype.";
	let ret2 = freeit pt in
	print_int ret2;
	