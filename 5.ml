(*
 * Reverse a list.
 *)
let rec rev lst =
  match lst with
    [] -> []
  | h::t -> rev t @ [h]
;;

assert (rev [ "a" ; "b"; "c"] = [ "c" ; "b"; "a"]) ;;

