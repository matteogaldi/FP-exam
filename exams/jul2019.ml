type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let t = Tr(5, Tr(7, Tr(3, Empty, Empty), Tr(4, Empty, Empty)), Tr(1, Tr(2, Empty, Empty), Tr(9, Empty, Empty)));;

type 'a ntree = Ntree of 'a * 'a ntree list
let nt = Ntree(2, [Ntree(1, [Ntree(7,[]);Ntree(9,[]);Ntree(8,[])]); Ntree(3, [Ntree(2,[])]); Ntree(5, [Ntree(7,[]); Ntree(6,[])])]);;

let n_ramo_bin n t = 
	let rec aux sum = function
			Empty -> failwith "n_ramo_bin"
		| Tr(r, Empty, Empty) -> 
				if r+sum=n then [r]
				else failwith "n_ramo_bin"
		| Tr(r, tleft, tright) -> 
				let new_sum = sum+r in
					if new_sum>n then failwith "n_ramo_bin"
						else
							try r::aux new_sum tleft
							with _ -> r::aux new_sum tright
	in aux 0 t

exception NotFound
let n_ramo n tree = 
	let rec from_node sum = function
			Ntree(r, []) -> if (sum+r)=n then [r] else raise NotFound
		| Ntree(r, tlist) -> 
				if r+sum>n then raise NotFound
				else r::from_list (sum+r) tlist
	and from_list sum = function
			[] -> raise NotFound
		| x::rest -> 
				try from_node sum x
				with NotFound -> from_list sum rest
	in from_node 0 tree
 (*
#use "jul2019.ml";;
 *)
