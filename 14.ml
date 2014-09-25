(*
 * Duplicate the elements of a list.
 *)

let rec duplicate xs =
  match xs with
    [] -> []
  | h::t -> h::h::(duplicate t)
		
;;

assert (duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]) ;;
