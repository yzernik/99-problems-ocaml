(*
 * Run-length encoding of a list.
 *)
let encode lst =
  let rec encode_helper acc hlst =
    match hlst with
      [] ->
      (match acc with
	 None -> []
       | Some(n,c) -> [(n,c)])
    | h::t ->
       (match acc with
	  None -> encode_helper (Some(1,h)) t
	| Some(n,c) -> 
	   if (h = c) then
	     encode_helper (Some(n+1,h)) t
	   else
	     (n,c)::encode_helper (Some(1,h)) t)
  in
  encode_helper None lst
	      
;;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]) ;;
