type 'a graph = ('a * 'a) list
exception NotFound
(*
_vicini: 'a -> 'a graph -> 'a list
_vicini nodo g = riporta tutti i vicini di nodo nel grafo g
*)
let rec _vicini nodo = function
		[] -> []
	| (x,y)::rest -> 
			if x=nodo then y::(vicini nodo rest)
		  else if y=nodo then x::(vicini nodo rest)
		  else vicini nodo rest


(*
vicini: 'a -> 'a graph -> 'a list
vicini nodo g = riporta tutti i vicini di nodo nel grafo g
*)
let vicini nodo g = 
	List.filter ((<>)nodo) (List.flatten(List.map (fun (x,y) -> [x;y]) (List.filter (fun (x,y) -> nodo=x || nodo=y) g)))


let successori nodo g = 
	List.map snd (List.filter (fun (x,y) -> x=nodo) g)


let test_connessi grafo start goal =
	let rec aux visited = function
			[] -> false
		| n::rest -> 
				if List.mem n visited
				then aux visited rest
				else n=goal || aux (n::visited) ((successori n grafo) @ rest)
	in aux [] [start]



(*
#use "10.ml";;
*)