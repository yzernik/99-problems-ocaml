(*
 * Rotate a list N places to the left.
 *)

let rec rotate xs n =
  if (n = 0) then
    xs
  else
    match xs with
      [] -> []
    | h::t -> rotate (t @ [h]) (n-1)



;;

assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]) ;;
