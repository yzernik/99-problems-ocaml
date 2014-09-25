(*
 * Replicate the elements of a list a given number of times.
 *)

let rec replicate xs n =
  let rec copy c m =
    if (m > 0) then
      c::copy c (m-1)
    else
      []
  in
  match xs with
    [] -> []
  | h::t -> (copy h n) @ (replicate t n)
		
;;

assert (replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]) ;;
