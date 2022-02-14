open G_tree;;

let breadth_first_t arbol =
	let rec aux arbol l = match arbol with
	    Gt(x,[]) -> List.rev (x::l)
	  | Gt(x,(Gt(y,t2))::t1) -> aux (Gt(y,List.rev_append (List.rev t1) t2)) (x::l)
	in aux arbol [];;	
		 

let t = let rec make t e =
    if e <= 0 then t
    else make (Gt(e, [t])) (e-1)
in make (Gt(500000,[])) 500000;;


