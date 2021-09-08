type 'a ntree = Tr of 'a * 'a ntree list;;

exception NotFound;;

(*se la radice corrente è uguale a x allora ritorna 0 altrimenti aggiunge 1 e chiama auxlist
depth: 'a -> 'a ntree -> int*)
let rec depth x (Tr(r, tlist)) =
	if r = x then 0
	else 1 + auxlist x tlist
	(*auxlist scorre la lista di sottoalberi, se vuota fallisce, altrimenti chiama depth sul primo
	sottoalbero e in seguito se necessario va avanti
	auxlist: 'a -> 'a ntree list -> int*)
	and auxlist x = function 
		[] -> raise NotFound
		|y::rst -> try depth x y with NotFound ->
			auxlist x rst;;

let rec parentela x y (Tr(r, tlist)) =
	if r = x then depth y (Tr(r, tlist)) 
	else if r = y then depth x (Tr(r, tlist)) 
	else try parentela_list x y tlist with _ ->
		depth x (Tr(r, tlist)) + depth y (Tr(r, tlist)) 
	(*scorre la lista di sottoalberi e chiama parentela sul primo e in caso di fallimento sui successivi
	parentela_lista: 'a -> 'a -> 'a ntree list -> int*)
	and parentela_list x y = function
		[] -> raise NotFound
		|r::rst -> try parentela x y r with _ ->
			parentela_list x y rst;;

let albero = Tr(1, [Tr(2, [Tr(5, []); Tr(8, [Tr(9, [Tr(15,[])]); Tr(10, [])])]); Tr(3, [Tr(6, []); Tr(7, []); 
	Tr(18, [Tr(29,[]); Tr(4, [])])]); Tr(14, [Tr(19, []); Tr(12,[]); Tr(29, [Tr(13,[])])])]);;
