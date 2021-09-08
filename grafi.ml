type 'a graph = ('a * 'a) list

exception NotFound

let my_graph = [(1,2);(1,3);(1,4);(2,6);(3,5);(4,6);(6,5);(6,7);(5,4)]

let rec succ nodo = function
	  [] -> []
	| (x,y)::rest -> 
		if nodo=x then y::succ nodo rest
		else succ nodo rest

let successori nodo grafo = 
	List.map snd (List.filter (function (x,_) -> x=nodo) grafo)


let depth_first_collect graph start = 
	let rec search visited = function
		| [] -> visited
		| n::rest -> 
			if List.mem n visited then search visited rest
			else search (n::visited) ((successori n graph) @ rest)
	in search [] [start] 

let breadth_first_collect graph start = 
	let rec search visited = function
		| [] -> visited
		| n::rest -> 
			if List.mem n visited then search visited rest
			else search (n::visited) (rest @ (successori n graph))
	in search [] [start]


(*
search_node 'a graph -> 'a -> ('a -> bool) -> 'a
search_node g x p = nodo che soddisfa p e raggiungibile da x in g

search 'a list -> 'a list -> 'a
search visited pending = nodo che soddisfa p raggiungibile da uno dei nodi
in pending senza pasare per visited
*)

let search_node graph start p = 
	let rec search visited = function
		| [] -> raise NotFound
		| n::rest ->
			if List.mem n visited then search visited rest
			else if p n then n
				else search (n::visited) ((successori n graph) @ rest)
	in search [] [start]