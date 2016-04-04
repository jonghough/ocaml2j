open Core.Std

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

 
val setup : unit -> int32
val freeit : int32 -> int
val doit : int32 -> string -> int
val getit : int32 -> string -> jResult
