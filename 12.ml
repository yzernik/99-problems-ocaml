(*
 * Decode a modified Run-length encoding of a list.
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;


let decode xs =
  let rec decode_helper acc hxs =
    match hxs with
      [] -> acc
     | h::t -> match h with
		 One(c) -> c::decode_helper acc t
	       | Many(n,c) ->
		  if (n > 1) then
		    c::decode_helper acc (Many(n-1,c)::t)
		  else 
		    c::decode_helper acc t
  in
  decode_helper [] xs
		
;;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) ;;
