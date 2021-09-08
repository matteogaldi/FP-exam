let rec is_sorted = function
	[] -> failwith "sorted"
	|[x] -> true
	|x::y::rst -> x <= y && is_sorted (y::rst);;

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;
type 'a graph = ('a * 'a) list;;

let rec sorted_branch t x = match t with
	Empty -> failwith "sorted"
	|Tr(r, Empty, Empty) -> if r = x then [r] else failwith "sorted"
	|Tr(r, tleft, tright) -> try r::check r x tleft with _ -> 
		r::check r x tright
		(*check controlla che il genitore del sottoalbero che sto considerando sia minore della radice del sottoalbero
		e in caso richiama sorted_branch, altrimenti fallsice
		check: 'a -> 'a -> 'a tree -> 'a list*)
		and check r x = function
			Empty -> failwith "sorted"
			|Tr(y, Empty, Empty) -> if y >= r then sorted_branch x (Tr(y, Empty, Empty))
				else failwith "sorted"
			|Tr(y, tleft, tright) -> if y >= r then try y::sorted_branch x tleft with _ ->
				y::sorted_branch x tright
				else failwith "sorted";;

(*riporta la lista dei successori di n in un grafo orientato g dove i successori siano >= n
successori: 'a -> 'a graph -> 'a list*)
let successori_ordinati n (g: 'a graph) =
	List.map snd (List.filter (function (x, y) -> x = n && y >= n) g);;

let sorted_path g start goal =
	(*esamina il nodo corrente, se è stato già visitato fallisce altrimenti chiama from_list
	from_node: 'a -> 'a list -> 'a list*)
	let rec from_node n visited =
		if List.mem n visited then failwith "sorted_path"
		else if n = goal then [n]
		else n::from_list (n::visited) (successori_ordinati n g)
		(*esamina la lista di nodi, se vuota fallisce altrimenti prova from_node
		from_list: 'a list -> 'a list -> 'a list*)
		and from_list visited = function
			[] -> failwith "sorted_path"
			|x::rst -> try from_node x visited with _ ->
				from_list visited rst
	in from_node start [];; 

let grafo = [(1,2); (2,3); (3,2); (3,5); (5,1); (5,6); (6,4)];;

let albero_bin_non_ordinato = Tr(5, Tr(4, Tr(2, Empty, Empty), Empty), Tr(3, Empty, Empty));;
let albero_bin_ordinato = Tr(1, Tr(2, Tr(1, Empty, Empty), Empty), Tr(3, Tr(2, Empty, Empty), Tr(4,Empty, Empty)));;
