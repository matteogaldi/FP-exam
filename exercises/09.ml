type 'a ntree = Ntree of 'a * ('a ntree list)
exception NotFound

let leaf x = Ntree(x, [])

let t = Ntree(1,[Ntree(2,[Ntree(3,[leaf 4;
                          leaf 5]);
                    Ntree(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Ntree(10,[Ntree(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Ntree(16,[leaf 17;
                            Ntree(18,[leaf 19;
                                   leaf 20])])])])

let leaves = [4;5;7;8;9;12;13;14;15;17;19;20]
(*
postorder: 'a ntree -> 'a list
*)
let rec postorder = function
	Ntree(x, tlist) -> (List.flatten(List.map postorder tlist)) @ [x]


let rec postorder_alt = function
	Ntree(x, tlist) -> postlist x tlist
	and postlist x = function
		[] -> [x]
	| t::rest -> (postorder_alt t) @ (postlist x rest)


let rec inorder = function
	Ntree(x, []) -> [x]
| Ntree(x, t::rest) -> 
	(inorder t) @ x::(List.flatten(List.map inorder rest))


let rec foglie_in_lista lst = function
	Ntree(x, []) -> List.mem x lst
| Ntree(_, tlist) -> List.for_all (foglie_in_lista lst) tlist


let make_list n = List.init n (fun x -> x)

let rec sumof = function
		[] -> 0
	| x::rest -> x+sumof rest

let rec sumof_tr lst = 
	let rec aux n lst = 
		match lst with
		| [] -> n
		| x::rest -> aux (n+x) rest 
	in aux 0 lst


let rec num_foglie = function
		Ntree(x, []) -> 1
	| Ntree(_, tlist) -> 
			sumof (List.map num_foglie tlist)


let get_nodo i = function
		[] -> raise NotFound
	| lst -> List.nth lst i

let rec lista_guida lst Ntree(x, tlist) = 
	match lst with
	| [] -> x
	| x::rest -> 
		try
			lista_guida rest (List.nth tlist x)
		with
		| _ -> failwith "Unknown"
(*
#use "09.ml";;
*)