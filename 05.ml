(* BACKTRACKING *)
exception NotFound

(* Criterio per scartare una soluzione parziale: la somma degli elementi della soluzione è maggiore di N*)
let rec sumof = function
	[] -> 0
	| x::rest -> x + (sumof rest)


(* PROBLEMA: Dato un insieme S di numeri interi positivi e un intero
N, determinare un sottoinsieme Y di S tale che la somma degli elementi
di Y sia uguale a N. *)

let subset_search set n = 
	let rec aux soluzione altri = 
		let somma = sumof soluzione in
		if somma>n then raise NotFound
		else if somma=n then soluzione
		else match altri with
		| [] -> raise NotFound
		| x::rest -> try
			aux (x::soluzione) rest
		with
		| NotFound -> aux soluzione rest
	in aux [] set


(* Problema: attraversamento di un labirinto "quadrato", da una casella
   di entrata a una casella di uscita, senza passare per caselle
   che contengono un mostro. 
   Ci si puo' spostare in orizzontale e in diagonale, ma solo verso
   destra. La casella di ingresso e' nella colonna piu' a sinistra. *)
(**  in modo da non avere necessita' di loop-checking **)

(*        0  1  2  3  4
       0        M
       1     M     M
       2           M   
       3  M        
       4        M   
*)

let mostri = [(0,2);(1,1);(1,3);(2,3);(3,0);(4,2)]

(* Un caso di fallimento: siamo fuori matrice *)
(* sottoproblema: dato l'indice massimo della matrice e una casella 
    (riga,colonna), determinare se la casella e` nella matrice *)
(*  in_labirinto : int -> int * int -> bool *)
let in_labirinto dim (r,c) = 
	r >= 0 && c >= 0 && r < dim && c < dim

let path1 dim mostri ingresso uscita = 
	let rec cerca_da ((r,c) as casella) = 
		if not (in_labirinto dim casella) 
		|| List.mem casella mostri then raise NotFound
		else if casella=uscita then [casella]
		else casella::
			try cerca_da (r, c+1)
			with NotFound -> 
				try cerca_da (r+1, c+1)
				with NotFound -> cerca_da (r-1, c+1)
	in cerca_da ingresso 

let rec filter_vicini dim = function
	[] -> []
	| casella::rest -> 
		if (in_labirinto dim casella)
		then casella::filter_vicini dim rest
		else filter_vicini dim rest


let rec power x = function
	  0 -> 1
	| k -> x*(power x (k-1))
