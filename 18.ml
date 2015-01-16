(*
 * Extract a slice from a list.
 *)

let slice xs n m =
  let rec slice_helper acc hxs hn hm =
    if (hm < 0) then
      acc
    else
      match hxs with
	[] -> acc
      | h::t ->
	 let newAcc = 
	   if (hn <= 0) then
	     acc @ [h]
	   else
	     acc
	 in
	 slice_helper newAcc t (hn-1) (hm-1)
  in
  slice_helper [] xs n m
;;

assert (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]) ;;
