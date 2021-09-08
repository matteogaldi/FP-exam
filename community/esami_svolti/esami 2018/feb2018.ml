 Assumiamo che gli interi di una lista [d1;...;dn] rappresentino le durate di una sequenza
temporale di eventi e1, . . . , en: per ogni i, di è la durata dell'evento ei
, espressa in unità di
tempo (u.t). Ad esempio, la lista [25;30;45] descrive la sequenza di eventi e1, e2, e3, dove
e1 dura 25 u.t., la durata di e2 è di 30 u.t. e quella di e3 di 45 u.t.
Se si assume che il primo evento inizi al tempo 0, e che gli altri inizino esattamente quando
finisce il precedente, dalla lista delle durate degli eventi si può ricavare la lista degli istanti
di tempo in cui termina ciascun evento. Scrivere una funzione:
durations_to_end_times: int list → int list, che, applicata a una lista di durate
di eventi, riporti la lista degli istanti temporali in cui terminano gli eventi della sequenza.
Ad esempio, si avrà che durations_to_end_times [25;30;45] = [25;55;100]. Infatti, se
e1, e2, e3 è la sequenza di eventi con durate, rispettivamente, di 25, 30 e 45 u.t., l'evento e1,
che inizia al tempo 0 e dura 25 u.t., nirà al tempo 25; l'evento e2, che inizia quando unisce
e1 (al tempo 25) e dura 30 u.t., unirà al tempo 55; e l'evento e3 che inizia al tempo 55 e dura
45 u.t., unisce al tempo 100

let rec sumof = function
	[] -> 0
	|x::rst -> x + (sumof rst);;

let duration_to_end_times lst =
	let rec aux temp = function
		[] -> temp
		|x::rst as lst -> aux ((sumof (lst))::temp) rst
	in aux [] (List.rev lst);;

(*riporta la lista dei nodi vicini ad n nel grafo
successori: 'a -> ('a * 'a) list -> 'a list*)
let rec successori n = function
	[] -> []
	|(x,y)::rst -> if x = n then y::successori n rst
		else if y = n then x::successori n rst
		else successori n rst;;

let safe_path (g: 'a graph) (wallet: 'a money) start goal init =
	(*a partire da un nodo cerca la quantità di soldi corrispondente nel wallet e aggiorna
	i soldi disponibili al momento, poi controlla se il nodo è già stato visitato oppure se 
	i soldi disponibili al momento sono < 0 e in caso fallisce
	from_node 'a -> 'a list -> 'a money -> int*)
	let rec from_node n visited wallie soldi =
		let new_wallet = soldi + try List.assoc n wallie 
				with _ -> 0 
			in
			if (new_wallet < 0) || List.mem n visited then failwith "safe_path"
			else if n = goal then [n]
			else n::from_list (n::visited) wallie new_wallet (successori n g) 
	(*scorre la lista di nodi
	from_list: 'a list -> 'a money -> int -> 'a list*)
				and from_list visited wallie soldi = function
					[] -> failwith "safe_path"
					|x::rst -> try from_node x visited wallie soldi with _ ->
						from_list visited wallie soldi rst
	in from_node start [] wallet init;;

let grafo = [('A', 'B'); ('A', 'C'); ('A', 'D'); ('B', 'E'); ('D', 'F'); ('C', 'F'); ('C', 'E'); ('E', 'G'); ('F', 'G')];;
let wallet = [('C',-7);('D',-15);('F',3);('G',-5)];;
