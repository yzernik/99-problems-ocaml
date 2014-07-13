(* 
 * returns the last element of a list
 * 
 *)
let rec last ls =
  match ls with
    h::[] -> Some h
  | h::t -> last t
  | [] -> None
;;		 
