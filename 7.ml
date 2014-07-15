(* There is no nested list type in OCaml, so we need to define one
 *     first. A node of a nested list is either an element, or a list of
 *    nodes. 
 *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

				      
(*
 * Flatten a nested list structure.
 *)
let rec flatten lst =
  match lst with
    [] -> []
  | One(x)::t -> 
     x::flatten t
  | Many(xs)::t ->
     flatten xs @ flatten t
;;

assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]) ;;
