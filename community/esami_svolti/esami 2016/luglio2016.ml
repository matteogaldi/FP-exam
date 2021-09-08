type ('a,'b) pseudo = ('a list * 'b) list;;

let rec dim x = function
	[] -> failwith "dim"
	|(y, b)::rst -> if List.mem x y then b else 
		dim x rst;;

type 'a graph = ('a * 'a) list;;

(*controlla se un elemento è contenuto nella lista delle dimensioni
check_dim: 'a -> ('a list * 'b) list -> bool*)
let rec check_dim x = function
	[] -> false
	|(y,b)::rst -> List.mem x y || dim x rst;;

(*ritorna la lista di successori del nodo n nel grafo g
successori: 'a -> ('a *'b) list -> 'b list' *)
let successori n g =
	List.map snd (List.filter (function (x,y) -> x = n) g);;  

let path plist dimlist g start goal = 
	(*visita il nodo, controlla se è gia stato visitato oppure se la sua dimensione non è corretta, in caso fallisce
	from_node: 'a -> 'a list -> 'a list*)
	let rec from_node n visited =
		if List.mem n visited || not (List.mem (dim n plist) dimlist) then failwith "path"
		else if n = goal then [n]
		else n::from_list (n::visited) (successori n g)
		(*scorre la lista di nodi
		from_list: 'a list -> 'a list -> 'a list*)
		and from_list visited = function
			[] -> failwith "path"
			|x::rst -> try from_node x visited with _ ->
				from_list visited rst
	in from_node start [];; 

let plist = [([1;2;3;4],"piccolo");([5;6;7;8],"medio");([9;10;11;12],"grande")];;
let dimlist = ["grande";"piccolo"];;
let grafo = [(1,100);(1,5);(1,3);(3,1);(100,10);(3,10);(5,10)];;