(*
 * Modified Run-length encoding of a list.
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;


let encode lst =
  let rec encode_helper acc hlst =
    match hlst with
      [] ->
      (match acc with
	 None -> []
       | Some(x) -> [x])
    | h::t ->
       (match acc with
	  None -> encode_helper (Some(One(h))) t
	| Some(e) -> 
	   (match e with
	      One(c) -> if (h = c) then 
			  encode_helper (Some(Many(2,h))) t
			else
			  One(c)::encode_helper (Some(One(h))) t
	    | Many(n,c) -> if (h = c) then 
			     encode_helper (Some(Many(n+1,h))) t
			   else
			     Many(n,c)::encode_helper (Some(One(h))) t))
  in
  encode_helper None lst
		
;;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) ;;
