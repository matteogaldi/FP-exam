type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let t = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(6, Empty, Empty)), Tr(3, Tr(5, Empty, Empty), Tr(7, Empty, Empty)))

let rec size = function
		Empty -> 0
	| Tr(r, tleft, tright) -> 1 + size tleft + size tright


let rec tree_exists p = function
		Empty -> false
	| Tr(r, tleft, tright) -> 
			p r || tree_exists p tleft || tree_exists p tright


let rec raccogli = function
		Empty -> []
	| Tr(r, tleft, tright) -> r::((raccogli tleft) @ (raccogli tright))


let rec preorder = function
		Empty -> []
	| Tr(r, tleft, tright) -> r::(preorder tleft @ preorder tright)


let rec postorder = function
		Empty -> []
	| Tr(r, tleft, tright) -> (postorder tleft) @ (postorder tright) @ [r]


let rec inorder = function
		Empty -> []
	| Tr(r, tleft, tright) -> (inorder tleft) @ (r::inorder tright)

(*-------------------- n-Trees --------------------*)
type 'a ntree = Ntree of 'a * 'a ntree list
let nt = Ntree(2, [Ntree(1, [Ntree(7,[]);Ntree(9,[]);Ntree(8,[])]); Ntree(3, [Ntree(2,[])]); Ntree(5, [Ntree(7,[]); Ntree(6,[])])]);;

let rec size_n (Ntree(_, tlist)) = 
	1 + sum_sizes tlist
and sum_sizes = function
		[] -> 0
	| x::rest -> (size_n x) + sum_sizes rest


let rec tree_exists_n p (Ntree(x, tlist)) = 
		p x && exists_in_list p tlist
and exists_in_list p = function
		[] -> false
	| x::rest -> tree_exists_n p x && exists_in_list p rest


let rec raccogli_n (Ntree(x, tlist)) = 
	x::raccogli_list_n tlist
and raccogli_list_n = function
		[] -> []
	| x::rest -> raccogli_n x @ raccogli_list_n rest


let rec preorder_n (Ntree(x, tlist)) = 
	x::preorder_list tlist
and preorder_list = function
		[] -> []
	| x::rest -> preorder_n x @ preorder_list rest


let rec postorder_n (Ntree(x, tlist)) = 
	postorder_list tlist @ [x]
and postorder_list = function
		[] -> []
	| x::rest -> postorder_n x @ postorder_list rest
(*
#use "alberi.ml";;
*)