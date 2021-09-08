(*rimuove una sola occorrenza di x dalla lista
togli_un: 'a -> 'a list -> 'a list*)
let rec togli_un x = function
	[] -> []
	|y::rst -> if x = y then rst 
		else y::togli_un x rst;;

let rec complemento superset = function
	[] -> superset
	|y::rst as set-> if List.mem y superset then complemento (togli_un y superset) (togli_un y set)
		else failwith "complemento";;

let tree = Tr(1, [Tr(2,[Tr(3,[]); Tr(4, [Tr(5,[]); Tr(6,[])]); Tr(4, [Tr(7, [])])]); 
	Tr(3,[Tr(8, [Tr(9,[]); Tr(10,[])]); Tr(2, [Tr(4, [Tr(3,[])]); Tr(11,[]); Tr(12,[])])])]);;

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let tree = Tr(5, Tr(2, Tr(3, Empty, Empty), Tr(4, Empty, Empty)), Tr(2, Tr(2, Empty, Empty), Tr(9, Empty, Empty)));;

let rec labels = function
	Tr(x, Empty, Empty) -> [x]
	|Empty -> []
	|Tr(x, tleft, tright) -> x::(labels tleft)@(labels tright);;

let rec discendenti x = function
	Empty -> []
	|Tr(r, tleft, tright) as tr -> if r = x then labels tr else
		(discendenti x tleft) @ (discendenti x tright);;
		


(**HO FATTO ER PANICO, STA ROBA E' TUTTA PER ALBERI N-ARI**)
(*labels: 'a ntree -> 'a list*)
let rec labels = function
	Tr(x,[]) -> [x]
	|Tr(x, tlist) -> x::aux_label tlist
		(*scorre la lista del sottoalbero e poi lo ripassa alla funzione principale
		aux_label: 'a list -> 'a list*)
		and aux_label = function
			[] -> []
			|y::rst -> (labels y)@(aux_label rst)
	|_ -> [];;

let rec discendenti x = function
	Tr(y, []) -> if y = x then labels (Tr(y,[]))
		else failwith "discendenti"
	|Tr(y, tlist) -> if y = x then labels (Tr(y, tlist))
		else scorri x tlist 
		(*scorri scorre la lista dei sottoalberi
		scorri: 'a -> 'a ntree list -> 'a list*)
		and scorri x = function
			[] -> []
			|y::rst ->  discendenti x y @ scorri x rst;;
