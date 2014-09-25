(*
 * Split a list into two parts; the length of the first part is given.
 *)

let split xs n =
  let rec split_helper acc hxs m =
    if (m = 0) then
      (acc,hxs)
    else
      match hxs with
      [] -> (acc,[])
    | h::t ->
       split_helper (acc @ [h]) t (m-1)
  in
  split_helper [] xs n


;;

assert (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])) ;;
assert (split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], [])) ;;
