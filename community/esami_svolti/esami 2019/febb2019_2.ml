type color = Rosso | Verde | Neutro;;
type col_assoc = ('a * color) list;;
type 'a graph = ('a * 'a) list;;

(*restituisce il colore associato ad x nella lista, se non è presente restituisce neutro
get_colore: 'a col_assoc -> 'a -> color *)
let get_colore (cl: 'a col_assoc) x =
	try List.assoc x cl 
	with _ -> Neutro;;

let conta_colori col_list l =
	[(Neutro, List.length (List.filter (function x -> x = Neutro) (List.map (function x -> get_colore col_list x) l))); 
	(Rosso, List.length (List.filter (function x -> x = Rosso) (List.map (function x -> get_colore col_list x) l))); 
	(Verde, List.length (List.filter (function x -> x = Verde) (List.map (function x -> get_colore col_list x) l)))];;

let successori n = function
	[] -> []
	|(x,y)::rst -> if x = n then y::successori n rst
		else if y = n then x::successori n rst
		else successori n rst;;

let path g cl lst start goal =
	(*controlla se il nodo corrente è stato già visitato e se il colore associato
	è nella giusta posizione della lista e poi chiama from_list, altrimenti fallisce
	from_node: 'a -> 'a list -> 'a col_assoc -> 'a list -> 'a list*)
	let rec from_node n visited cl lst =
		if List.mem n visited then failwith "path"
		else let new_list = if List.hd lst = get_colore cl n then List.tl lst
			else lst in
				if n = goal && (lst = [] || lst = [get_colore cl n]) then [n]
				else n::from_list (n::visited) cl new_list (successori n g) 
		(*esamina la lista di nodi e poi prova from_node
		from_list: 'a list -> 'a col_assoc -> 'a list -> 'a list - 'a list*)
		and from_list visited cl lst = function
			[] -> failwith "path"
			|x::rst -> try from_node x visited cl lst with _ ->
				from_list visited cl lst rst
	in from_node start [] cl lst;; 

let cl = [(2,Rosso); (3,Verde); (4,Verde);(6,Verde); (7,Rosso)];;
let grafo = [(1,2); (3,2); (3,1); (3,4); (4,5); (3,5); (2,5); (5,6); (6,7); (7,8)];;