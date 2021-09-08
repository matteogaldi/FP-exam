 type color = Rosso | Verde | Neutro;;
 type col_assoc = ('a * color) list;;
 type 'a graph = ('a * 'a ) list;;

(*rimuove un'unica occorrenza di y dalla lista
rimuovi: 'a -> 'a list -> 'a list*)
let rec rimuovi  y = function
	[] -> []
	|x::rst -> if x = y then rst else
		x::rimuovi y rst;;

(*rimuove dalla prima lista gli elementi della seconda (che sono presenti)
subst: 'a list -> 'a list -> 'a list*)
let subst list1 list2 = match list1 with
    [] -> []
    |x::rst -> if List.mem x list2 then subst rst (rimuovi x list2)
    	else x::(subst rst list2);;

[(2,Rosso); (3,Verde); (4,Verde); (6,Verde); (7,Rosso)]

[1;2;3;4;5;6;7;8;9;10]

(*riporta una lista contente tutte le chiavi associate a un dato colore col presenti nella lista associativa
trova_colore : ('a * 'b) list -> 'b -> 'a list -> 'a list*)
let rec trova_colore col_list col num = match col_list with
	[] -> []
  	|(x,y)::rst -> if (y=col && List.mem x num)
		then x::trova_colore rst col num
		else trova_colore rst col num;;

(*associa a ogni colore il proprio numero di occorrenza nella lista, associando il colore neutro agli elementi 
non presente nella lista dei colori
conta_colori: ('a * color) list -> 'a list -> (color * int) list*)
let conta_colori lista_colori lista_numeri =
	[(Verde, List.length (trova_colore lista_colori Verde lista_numeri)); 
	(Rosso, List.length (trova_colore lista_colori Rosso lista_numeri));
	(Neutro, List.length(subst lista_numeri ((trova_colore lista_colori Verde lista_numeri)@(trova_colore lista_colori Rosso lista_numeri))))];;

(*riporta una lista dei successori di un nodo in un grafo non ordinato
successori: 'a -> 'a graph -> 'a list*)
let rec successori n = function
	[] -> []
	|(x,y)::rst -> if x = n then y::successori n rst
		else if y = n then x::successori n rst
		else successori n rst;; 

(*riporta il colore associato a un nodo, se non presente nella lista associativa riporta neutro
get_node_color: ('a * color) list -> 'a -> color*)
let get_node_color col_assoc n = 
	try List.assoc n col_assoc 
	with _ -> Neutro;;

let path g colors lst start = 
	(*controlla che il nodo corrente non sia già stato visitato, poi cerca il colore del nodo e controlla la lista,
	se è vuota o contiene solo il nodo corrente allora riporta il nodo; alla fine viene aggiornata la lista dei colori, 
	se necessario, ovvero se il primo nodo dell lista è quello in cui ci troviamo al momento
	from_node: 'a -> 'a list -> 'a list -> 'a list*)
	let rec from_node n visited lst =
		if List.mem n visited then failwith "path"
	    else let node_color = get_node_color colors n in 
	    	if lst = [] || lst = [node_color] 
	    		then [n]
	    	else let new_list = if List.hd lst = node_color then rimuovi node_color lst
	    		else lst in
	    n::from_list (n::visited) new_list (successori n g) 
	    (*cammino non ciclico da uno dei nodi fino ad esaurimento lista
	    from_list: 'a list -> 'a list -> 'a list -> 'a list*)
	    and from_list visited lst = function
	    	[] -> failwith "path"
	    	|x::rst -> try from_node x visited lst with _ ->
	    		from_list visited lst rst 
	in from_node start [] lst;;