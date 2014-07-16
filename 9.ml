(*
 * Pack consecutive duplicates of list elements into sublists.
 *)
let pack lst =
  let rec pack_helper acc hlst =
    match hlst with
      [] ->
      (match acc with
	 [] -> []
       | hh::ht -> [hh::ht])
    | h::t ->
       (match acc with
	  [] -> pack_helper [h] t
	| hh::ht -> 
	   if (h = hh) then
	     pack_helper (acc @ [h]) t
	   else
	     acc::pack_helper [h] t)
  in
  pack_helper [] lst
	      
;;
  

assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]) ;;
