(*let rec to0from n =
	if n < 0 then []
	else n :: to0from (n-1);;*)

let to0from n =
	let rec aux l i =
		if i > n then l
		else aux (i::l) (i+1)
	in aux [] 0;;
	
(*let rec fromto m n =
	if m > n then []
	else m :: fromto (m+1) n;;*)

let fromto m n =
	let rec aux l i =
		if i < m then l
		else aux (i::l) (i-1)
	in aux [] n;;
	
(*let rec from1to n =
	if n < 1 then []
	else from1to (n-1) @ [n];;*)

let from1to n =
	let rec aux l i =
		if i < 1 then l
		else aux (i::l) (i-1)
	in aux [] n;;

(*let map =
	List.map;;*)

let map f l =
	let rec aux f l acc = match l with
		[] -> List.rev acc
		| h::t -> aux f t ((f h)::acc)		
	in aux f l [];;

(*let power x y =
	let rec innerpower x y =
		if y = 0 then 1
		else x * innerpower x (y-1)
	in
		if y >= 0 then innerpower x y
		else invalid_arg "power";;*)

let power x y =
	let rec innerpower x y acc =
		if y=0 then acc
		else innerpower x (y-1) (x*acc)
	in
	if y >= 0 then innerpower x y 1
	else invalid_arg "power";;

(*let incseg l =
	List.fold_right (fun x t -> x::List.map ((+) x) t) l [];;*)

let incseg l = 
	let rec aux l acc l2 = match l with
		[] -> []
		| [h] -> List.rev ((h+acc)::l2)
		| h::t -> aux t (h+acc) ((h+acc)::l2)
	in aux l 0 [];;
	
	
(*let rec remove x = function
	[] -> []
	| h::t -> if x = h then t
		  else h :: remove x t;;*)

let rec remove x l = 
	let rec aux a = function
		[] -> l
		|hd::tl -> if x = hd
		then List.rev_append a tl
		else aux (hd::a)tl
		in aux [] l;;	
		
     
let divide l = 
	let rec aux l l1 l2 = match l with
		[]->(List.rev l1, List.rev l2)
		|(x::y::t)-> aux t (x::l1) (y::l2)
		|(x::[])-> (List.rev (x::l1), List.rev l2)
	in aux l [] [];;


	
(*let rec compress = function
| h1::h2::t -> if h1 = h2 then compress (h2::t)
else h1 :: compress (h2::t)
| l -> l;;*)	

let rec append l1 l2 = match (l1,l2) with
	([],l2) -> l2
	|(h::t,l2) -> h::(append t l2);;	
	
let compress l = 
    let rec aux l acum = match l with
        [] -> acum
       |h1::h2::t -> if h1=h2 
                     then aux(h2::t) acum
                     else aux (h2::t) (h1::acum)
       | _ -> append l acum
    in aux (List.rev l) [];;
    
    		
