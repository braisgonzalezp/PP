type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;


let sum arbol =
	let auxsum a b c = a+b+c in
	    fold_tree auxsum 0 arbol;;


let prod arbol = fold_tree (fun a b c -> a *. b *. c) 1.0 arbol;;


let size arbol = 
    let auxsum a b c = 1+b+c in
        fold_tree auxsum 0 arbol ;;


let inorder arbol = fold_tree (fun a b c -> b@[a]@c) [] arbol;;


let mirror arbol = fold_tree (fun a i d -> Node(a,d,i)) Empty arbol ;;

