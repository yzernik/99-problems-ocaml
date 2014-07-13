(*
 * Find the k'th element of a list. 
 *)
let rec at k lst =
  match lst with
    [] -> None
  | h::t ->
     if (k < 1) then
       None 
     else if (k = 1) then
       Some h
     else
       at (k-1) t
;;

assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c") ;;
assert (at 3 [ "a" ] = None) ;;
