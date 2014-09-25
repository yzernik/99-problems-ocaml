(*
 * Drop every N'th element from a list.
 *)

let drop xs n =
  let rec drop_helper hxs n m =
    match hxs with
      [] -> []
    | h::t ->
       if (m = 0) then
	 drop_helper t n (n-1)
       else
	 h::drop_helper t n (m-1)
  in
  drop_helper xs n (n-1)

		
;;

assert (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]) ;;
