type 'a action_graph = ('a * string * 'a) list

let grafo = [(1,"a",2); (1,"b",3); (1,"c",4); (2,"a",6); (3,"b",5);
(3,"c",5); (4,"b",1); (4,"c",6); (5,"c",4); (5,"a",5);
(5,"b",5); (6,"b",5)];;

(*riporta la  lista nodo * action dove ogni nodo è successore del nodo n nel grafo g
successori: 'a -> 'a action graph -> ('a * 'b) list *)
let successori n g =
	List.map (function (x,y,z) -> (y,z)) (List.filter (function (x,y,z) -> x = n) g);;

(*come successori ma ritorna solo il label*)
let succ_uscenti n g = 
	List.map (function (x,y,z) -> y) (List.filter (function (x,y,z) -> x = n) g);;

(*come successori ma ritorna solo in nodo*)
let succ_nodi n g =
	List.map (function (x,y,z) -> z) (List.filter (function (x,y,z) -> x = n) g);;

let nodes g =
	(*salva in una lista di appoggio i nodi estremi degli archi visitati e per ogni nodo controlla
		che non sia stato già salvato (per evitare ripetizioni)
	aux: 'a list -> 'a action_graph ->  'a list*)
	let rec aux temp = function
		[] -> temp
		|(x,y,z)::rst -> if List.mem x temp && List.mem z temp then aux temp rst
			else if List.mem x temp then aux (z::temp) rst
			else if List.mem z temp then aux (x::temp) rst
			else aux (x::z::temp) rst
	in aux [] g;; 

(*controlla che in una lista non vi sia nessuna ripetizione
check_usc: 'a list -> bool*)
let rec check_usc = function
	[] -> true
	|[x] -> true
	|x::y::rst -> not (List.mem x (y::rst)) && check_usc (y::rst);;

(*controlla che un nodo non abbia archi uscenti con lo stesso nome
check: 'a action_graph -> bool*)
let rec check g = match g with
	[] -> true
	|(x,y,z)::rst -> check_usc (succ_uscenti x g) && check rst;;

(*dato un nodo e il suo precedente ritorna il nome dell'arco
get_label: 'a -> 'a -> ('a *'b *'a) list -> 'a*)
let rec get_label n nprec = function
	(x,y,z)::rst -> if x = nprec && z = n then y
		else get_label n nprec rst
	|_ -> failwith "label";;

(*soluzione del ricevimento*)
let move g start goal = 
	if start = goal then []
	else
	(*controlla il nodo corrente, se non è stato visitato e se è goal restituisce l'azione tra n 
	e il suo precedente, altrimenti chiama from_list aggiungendo l 'azione tra n e nprec alla lista
	from_node: 'a -> 'a -> 'a list -> string list*)
	let rec from_node n nprec visited =
		if List.mem n visited then failwith "move"
		else 
			if n = goal then [get_label n nprec g]
			else (get_label n nprec g)::from_list n (n::visited)  (succ_nodi n g)
	(*scorre la lista dei nodi, fallisce se è vuota
	from_list: 'a -> 'a list -> 'a list -> string list*)
	and from_list nprec visited = function
		[] -> failwith "move"
		|x::rst -> try from_node x nprec visited 
				   with _ -> from_list x visited rst
	in
	(*if start = goal then [] else*) 
	from_list start [start] (succ_nodi start g);;

(*soluzione trovata seguendo il suggerimento del testo*)
let move (g: 'a action_graph) start goal =
	(*considera solo il nodo corrente
	from_node: 'a -> 'a list -> string list*)
	let rec from_node n visited = 
		if List.mem n visited then failwith "move"
		else from_list (n::visited) (successori n g)
	(*restituisce la lista di actions
	from_list: 'a list -> 'a list -> string list*)
	and from_list visited = function
		[] -> failwith "move"
		|[(x,y)] -> if y = goal then [x] else failwith "move"
		|(x,y)::rst -> try x::from_node y visited 
					   with _ -> from_list visited rst
	in from_list [start] (successori start g);;
