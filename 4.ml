(*
 * Find the number of elements in a list.
 *)
let length lst =
  let rec length_helper acc ls =
    match ls with
      [] -> acc
    | h::t -> length_helper (acc + 1) t
  in
  length_helper 0 lst
;;
  
assert (length [ "a" ; "b"; "c"] = 3) ;;
assert (length [] = 0) ;;
