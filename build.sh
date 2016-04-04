OPTIONS="-g -cclib -Xlinker,--no-as-needed -linkpkg -thread"
LIBS="-package core,ctypes,ctypes.foreign"
FILES="floating.mli floating.ml callc.mli callc.ml runj.ml"
DLLPATH=`pwd`
DLLLIB="-dllib -lj"
INCLUDE=`pwd`
OUTPUT="-o callc.byte"

ocamlfind ocamlc $OPTIONS $LIBS $FILES -dllpath $DLLPATH $DLLLIB $OUTPUT 


# -dllpath "/home/jon/workspace/ocaml2j/"

