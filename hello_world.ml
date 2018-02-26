(*This is a comment in OCAML (same as SML)*)
(* This
*is multiline *)

let avg a b = 
    (a +. b) /. 2.0;;
    
let ptest () = print_endline("hello");;

ptest ();;

let avgint a b = (a + b) / 2;;

let rec range a b = 
    if a > b
    then []
    else a :: (range (a + 1) b);;


print_float(avg 5.0 2.0);;

print_int(avgint 5 2);;




