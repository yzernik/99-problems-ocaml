(*
 * return the last two elements in a list if they exist.
 *)

let rec last_two lst =
  match lst with
    [] -> None
  | [l] -> None
  | p::l::[] -> Some (p,l)
  | h::t -> last_two t
;;
  
assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d")) ;;
assert (last_two [ "a" ] = None) ;;
