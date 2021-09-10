exception NotFound
type 'a ntree = Tr of 'a * 'a ntree list

let t = Tr(1, [Tr(2, [Tr(5, []); Tr(8, [Tr(9, [Tr(15,[])]); Tr(10, [])])]); Tr(3, [Tr(6, []); Tr(7, []); 
	Tr(18, [Tr(29,[]); Tr(4, [])])]); Tr(14, [Tr(19, []); Tr(12,[]); Tr(29, [Tr(13,[])])])]);;

let depth x t = 
	let rec from_node (Tr(r, tlist)) = 
		if r=x then 0
		else 1 + from_list tlist
	and from_list = function
			[] -> raise NotFound
		| x::rest -> 
				try from_node x
				with NotFound -> from_list rest
	in from_node t


let parentela t x y = 
	let rec from_node (Tr(r, tlist)) = 
		if r=x then depth y (Tr(r, tlist))
		else if r=y then depth x (Tr(r, tlist))
		else 
			try parentela_list tlist
			with NotFound -> depth x (Tr(r, tlist)) + depth y (Tr(r, tlist)) 
	and parentela_list = function
			[] -> raise NotFound
		| r::rest ->
				try from_node r
				with NotFound -> parentela_list rest
	in from_node t 


(*
#use "jul2018.ml";;
*)