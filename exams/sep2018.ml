let rec is_sorted = function
		[] | _::[] -> true
	| x::y::rest -> x<=y && is_sorted (y::rest)

exception NotFound
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let t = Tr(3,
           Tr(4,Tr(3,Empty,Empty),Tr(16,Tr(15,Empty,Empty),Empty)),
             Tr(6,Tr(9,Tr(13,Tr(15,Empty,Empty),Empty),Tr(-19,Empty,Empty)),Empty))

let sorted_branch t x =
	let rec aux path = function
			Empty -> raise NotFound
		| Tr(r, Empty, Empty) -> 
				if r=x && is_sorted (path@[r]) then [r]
				else raise NotFound
		| Tr(r, tleft, tright) ->
				if is_sorted (path@[r])
				then r::(
					try aux (path@[r]) tleft
					with NotFound -> aux (path@[r]) tright
				)
				else raise NotFound
	in aux [] t

(*
#use "sep2018.ml";;
*)