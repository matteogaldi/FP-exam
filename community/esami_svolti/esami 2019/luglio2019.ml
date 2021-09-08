let n_ramo_bin n tree = 
	(*se l'albero è vuoto oppure se arrivati alla foglia non si è arrivati alla somma giusta fallisce,
	altrimenti incrementa la somma e controlla i restanti sottoalberi
	aux: int -> int -> int tree -> int list*)
	let rec aux n somma = function
		Empty -> failwith "n_ramo"
		|Tr(x,Empty,Empty) -> if (somma + x) = n then [x]
			else failwith "n_ramo"
		|Tr(x, tleft, tright) -> let new_somma = somma + x in
			if new_somma > n then failwith "n_ramo"
			else try x::aux n new_somma tleft with _-> 
				x::aux n new_somma tright
	in aux_tree n 0 tree;;

let tree = Tr(5, Tr(7, Tr(3, Empty, Empty), Tr(4, Empty, Empty)), Tr(1, Tr(2, Empty, Empty), Tr(9, Empty, Empty)));;

let n_ramo n tree =
	(*controlla il sottoalbero corrente, se siamo arrivati alla foglia controlla
	che la somma del ramo corrente sia = n, altrimenti fallisce
	Se non siamo arrivati alla foglia chiama aux_list e controlla i sottoalberi
	aux_tree: int -> int -> int ntree -> int list*)
	let rec aux_tree n somma = function
		Tr(r, []) -> if somma + r = n then [r]
			else failwith "n_ramo"
		|Tr(r, tlist) -> let new_somma = somma + r in
			if new_somma > n then failwith "n_ramo"
			else r::aux_list n new_somma tlist
			(*aux_list: int -> int -> 'a ntree -> 'a list
			scorre i figli di un nodo*)
			and aux_list n somma = function
				[] -> failwith "n_ramo"
				|x::rst -> try aux_tree n somma x with _ ->
					aux_list n somma rst
in aux n 0 tree;;

let tree = Tr(2, [Tr(1, [Tr(7,[]);Tr(9,[]);Tr(8,[])]); Tr(3, [Tr(2,[])]); Tr(5, [Tr(7,[]); Tr(6,[])])]);;
