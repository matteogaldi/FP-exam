type metro = (int * int * string) list;;

let m = [(1,2,"A"); (2,3,"A");(3,1,"A"); (2,4,"B"); (4,5,"B"); (4,6,"C"); (6,3,"C"); (5,7,"D");
(6,7,"D")];;

let line ln (mt: metro) =
	(*riporta la lista di tutte le stazioni collegate dalla linea ln
	appoggiandosi a una lista ausiliaria
	aux: int list -> string -> metro -> int list*)
	let rec aux temp ln = function
		[] -> temp
		|(x,y,z)::rst -> if z = ln then
							if (List.mem x temp && List.mem y temp) then aux temp ln rst
							else 
								if List.mem x temp then aux (y::temp) ln rst
								else 
									if List.mem y temp then aux (x::temp) ln rst
									else aux (x::y::temp) ln rst
						 else aux temp ln rst
	in aux [] ln mt;; 

let rec vicini stz (m: metro) = match m with
	[] -> []
	|(x,y,z)::rst -> if stz = x then (y,z)::vicini stz rst
					 else
					 	if stz = y then (x,z)::vicini stz rst
					 	else vicini stz rst;; 

let raggiungi (m: metro) maxc start goal =
	(*controlla il nodo corrente, se non è stato già visitato o non ho superato il
	numero massimo di cambi lo aggiunge alla lista, altrimenti fallisce
	from_node: int -> int list -> int -> string -> int list*)
	let rec from_node n visited cambi linea =
		if List.mem n visited || cambi >= maxc then failwith "cambi"
		else 
			if n = goal then [n] 
			else n::from_list (n::visited) cambi linea (vicini n m)
	(*scorre la lista di nodi e aggiorno il numero di cambi se la linea è cambiata 
	rispetto al precedente
	from_list: int list -> int -> string -> int list*)
	and from_list visited cambi linea = function
		[] -> failwith "cambi"
		|(st,ln)::rst -> let new_cambi = if ln != linea then cambi + 1
										 else cambi in
			try from_node st visited new_cambi ln 
			with _ -> from_list visited cambi ln rst
	in from_node start [] (-1) "";; 
