open Ctypes
open Foreign
open Core.Std
open Floating


(* 
  8 NB. main definitions:
  9 NB. jclear   clear J session
 10 NB. jcmd     send sentence, return result
 11 NB. jdo      send sentence to be executed
 12 NB. jget     retrieve value of J noun
 13 NB. jset     assign value to J noun
 14 NB.
 15 NB. utilities:
 16 NB. jinit    initialize J instance
 17 NB. jfree    free J instance
*) 

type jInt = JInt of int
type jInt32 = JInt32 of int32
type jBool = JBool of bool
type jFloat = JFloat of float
type jBox = JBox of int (* int will be a pointer *)


type jArray =
	[ `JIntArray of jInt Array.t
	| `JBoolArray of jBool Array.t
	| `JFloatArray of jFloat Array.t
	| `JBoxArray of jFloat Array.t
	| `JString of string]
	
type jResult = 
	| JResult of jInt * jInt  * jArray * jArray (* datatype, rank, shape, data *)
	| JError of string


(* =============== J.dll function calls ======================= *)
let jinit =
  foreign "JInit" (void @-> returning int32_t)
	
let jfree =
	foreign "JFree" (int32_t @-> returning int)
	
let jdo =
	foreign "JDo" (int32_t @-> string @-> returning int)
	
let jget =
	foreign "JGetM" (int32_t @-> string @-> ptr int @-> ptr int @-> ptr int @-> ptr int @-> returning int)
(* ============================================================ *)


let setup () =
	let a = jinit () in
	a
	
	
let doit (jinstance : int32) command =
	jdo jinstance command


(* Retrieve string data from the jget function's final pointer reference. *)
(* Length parameter is calculated from rank and shape data, which were    *)
(* recovered from pointers two and three.                                 *)
let retrieve_string_data pointer length =
	let data = String.make length ('0') in
	for i = 0 to (length - 1) do
		let ptr = from_voidp char (ptr_of_raw_address(Nativeint.of_int((!@pointer) + (1*i)))) in
		String.set data i ((!@ptr));
	done;
	`JString data
		
	
(* Retrieve integer data from the jget function's final pointer reference. *)
(* Length parameter is calculated from rank and shape data, which were     *)
(* recovered from pointers two and three.                                  *)	
let retrieve_integer_data pointer length =
	let data : jArray = `JIntArray (Array.init length (fun d -> JInt d)) in
	for i = 0 to (length - 1) do
		let ptr = from_voidp int (ptr_of_raw_address(Nativeint.of_int((!@pointer) + (4*i)))) in
		match data with
		| `JIntArray arr -> arr.(i) <- JInt (!@ptr);
		| _ -> ();
	done;
	data
	
	
(* Retrieve boolean data from jget function's final pointer reference. *)
let retrieve_bool_data pointer length =
	let data : jArray = `JBoolArray (Array.init length (fun d -> JBool true)) in
	for i = 0 to (length - 1) do
		let ptr = from_voidp bool (ptr_of_raw_address(Nativeint.of_int((!@pointer) + (i)))) in
		match data with
		| `JBoolArray arr -> arr.(i) <- JBool (!@ptr);
		| _ -> ();
	done;
	data
	
	
(* Retrieve the shape data from the pointer.               *)
(* Shape data is referenced by the third pointer parameter *)
(* of jget function. The rank, which gives the length      *)
(* parameter, is referenced by the second pointer.         *)
let retrieve_shape pointer (length : int) =
	retrieve_integer_data pointer length


(* Retrieve float data from the jget function's final pointer reference.   *)
(* Length parameter is calculated from rank and shape data, which were     *)
(* recovered from pointers two and three.                                  *)	
let retrieve_float_data pointer (length : int)=
	let data = `JFloatArray (Array.init length (fun d -> JFloat 0.0)) in
	for i = 0 to (length - 1) do
		let ptr1 = from_voidp int32_t (
			ptr_of_raw_address @@ Nativeint.of_int((!@pointer) + (8 * i))) in
		let ptr2 = from_voidp int32_t (
			ptr_of_raw_address @@ Nativeint.of_int((!@pointer) + 4 + (8 * i))) in
		let fval = Floating.extract_float (!@ptr1) (!@ptr2) in
		
		match data with
		| `JFloatArray arr -> arr.(i) <- JFloat fval
		| _ -> ()

	done;
	data
	
	
(* multiply two jInts *)
let mul_jint x y =
	match x,y with
	| JInt a, JInt b -> JInt (a * b)


(* Gets the data represented in J by value, if it exists. *)	
let getit (jinstance : int32) value =
	let p1 = allocate int 0 in
	let p2 = allocate int 0 in
	let p3 = allocate int 0 in
	let p4 = allocate int 0 in
	let res = jget jinstance value p1 p2 p3 p4 in
	match res with
	| 0 ->
		let datatype : int = !@p1 in
		let rank : int = !@p2 in
		let shape = retrieve_shape p3 rank in
		let ravel = 
			match shape with
			| `JIntArray arr ->
				(let ravel_len = List.fold ~init:(JInt 1) ~f:mul_jint (Array.to_list arr) in
					match ravel_len with
					| JInt rl -> rl )
			| _ -> 0 in
			
		(match datatype with
		| 1 ->
			let data = retrieve_bool_data p4 ravel in
			JResult (JInt datatype, JInt rank, shape, data)
		| 2 ->
			let data = retrieve_string_data p4 ravel in
			JResult (JInt datatype, JInt rank, shape, data)
		| 4 ->
			let data = retrieve_integer_data p4 ravel in
			JResult (JInt datatype, JInt rank, shape, data)
		| 8 ->
			let data = retrieve_float_data p4 ravel in
			JResult (JInt datatype, JInt rank, shape, data)
		| 64 ->
			Printf.printf "Extended integer type not implemented.";
			JError "Extended integer type not implemented."
		| _ -> 
			JError "Unknown error. Maybe variable does not exist.")
	| _ -> JError "JGetM function call failed."

let freeit pointer =
	jfree pointer

	
(* example function *)	
(*let () = 
	let pt = setup () in
	doit pt "isPrime =: 1 p: >: i. 100 ";
	let result = getit pt "isPrime" in

	match result with
	| JError e ->
		printf "error %s\n" e
	| JResult (_,_,_,jr) ->
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
	let ret2 = jfree pt in
	print_int ret2;*)
	

	