(*
 * Eliminate consecutive duplicates of list elements.
 *)
let compress lst =
  let rec compress_helper acc hlst =
    match hlst with
      [] -> []
    | h::t ->
       let rest =
	 compress_helper (Some h) t
       in
       match acc with
	 None -> h::rest
       | Some(x) ->
	  if (x = h) then
	    rest else
	    h::rest
  in
  compress_helper None lst
;;


assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]) ;;
