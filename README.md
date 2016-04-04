# ocaml2j
Example programm calling j.dll (libj.so) from Ocaml.

## Functions
j.dll functions 

`JInit`, `JDo`, `JFree`, `JGetM` are mapped to Ocaml functions:

```
jinit, jfree, jdo, jget
```

and **J** types `Boolean`, `Integer`, `Floating`, `Literal` are mapped to 
```

type jBool = JBool of bool
type jInt = JInt of int
type jFloat = JFloat of float
```
Other types are not supported (yet).

## Building and running

to build -> `./build.sh`

to run -> `./callc.byte`

## Tested platforms

This has been tested on 32-bit Linux only. For other OSes, obviously the correct library file is needed. Definitely not tested on 64-bit yet.
