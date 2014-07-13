(*
 * Find the number of elements in a list.
 *)
let rec length lst =
  match lst with
    [] -> 0
  | h::t -> 1 + length t
;;
  
assert (length [ "a" ; "b"; "c"] = 3) ;;
assert (length [] = 0) ;;
