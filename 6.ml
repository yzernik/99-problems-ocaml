(*
 * Find out whether a list is a palindrome.
 *)
let rec is_palindrome lst =
  let rec rev lst =
    match lst with
      [] -> []
    | h::t -> rev t @ [h]
  in
  match lst with
    [] -> true
  | h::t -> 
     match rev t with
       [] -> true
     | rh::rt ->
	(rh = h) &&
	  is_palindrome rt
;;

assert (is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true) ;;
assert (is_palindrome [ "a" ; "b" ] = false) ;;
