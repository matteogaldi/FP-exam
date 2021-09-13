type 'a ntree = Ntree of 'a * 'a ntree list
exception NotFound

let assoclist = [(2,4);(3,6);(5,10);(8,16);(11,22)]
let lst = [4;5;7;11]
let rec apply assoclist = function
	| [] -> []
	| x::rest -> 
		let y = 
			try List.assoc x assoclist
			with Not_found -> x
		in y::apply assoclist rest


let assoc x assoclist = 
	try List.assoc x assoclist
	with Not_found -> x

let apply_ assoclist elle = 
	List.map (fun x -> assoc x assoclist) elle

let guida = [(1,3);(2,2);(3,1);(10,1);(16,2);(11,2)]
let guida_sbagliata = [(1,3);(2,2);(3,1);(16,2);(11,2)]
let t = Ntree(1, [
			Ntree(2, [
				Ntree(3, [
					Ntree(4, []); Ntree(5, [])]); 
				Ntree(6, [
					Ntree(7, [])]); Ntree(8, [])]); 
			Ntree(9, []);
			Ntree(10, [
				Ntree(11,[
					Ntree(12, []);
					Ntree(13, []);
					Ntree(14, [])
				]);
				Ntree(15, []);
				Ntree(16, [
					Ntree(17, []);
					Ntree(18, [
						Ntree(19, []);
						Ntree(20, []);
					]);
				]);
			]);
		])

let cerca_foglia guida t = 
	let rec from_node = function
		| Ntree(r, []) -> r
		| Ntree(r, tlist) ->
			let next_node_pointer = 
				try List.assoc r guida
				with Not_found -> raise NotFound
			in from_list next_node_pointer tlist
	and from_list next_node_pointer = function
		| [] -> raise NotFound
		| _ as lst -> 
			let next_node = 
				try List.nth lst (next_node_pointer-1)
				with _ -> raise NotFound
			in from_node next_node
	in from_node t;; 
(*
#use "sep2021.ml";;
*)