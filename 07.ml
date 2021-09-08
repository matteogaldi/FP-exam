type direzione = Su | Giu | Destra | Sinistra
type azione =
    Gira
  | Avanti of int;;
let gira = function
    Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su
let avanti (x,y,dir) n =
  match dir with
    Su -> (x,y+n,dir)
  | Giu -> (x,y-n,dir)
  | Destra -> (x+n,y,dir)
  | Sinistra -> (x-n,y,dir)
let sposta (x,y,dir) act =
  match act with
    Gira -> (x,y,gira dir)
  | Avanti n -> avanti (x,y,dir) n


let rec esegui pos = function
| [] -> sposta
| x::rest -> 
	esegui (sposta pos x) rest


type nat = Zero | Succ of nat

(* somma : nat -> nat -> nat *)
(*let rec somma n m =
	match n with
		| Zero -> m
		| Succ k -> Succ(somma k m)


let rec mul m n = 
	match m with
		| Zero -> Zero
		| Succ k -> somma m (prodotto k m)*)


type chiave = Aperta | Chiusa
type cassaforte = chiave list


exception Fail
let gira = function
| Aperta -> Chiusa
| Chiusa -> Aperta


let giraPrima = function
| x::rest -> (gira x)::rest
| _ -> raise Fail


let rec giraDopoChiusa = function
| Chiusa::x::rest -> Chiusa::(gira x)::rest
| Aperta::rest -> Aperta::giraDopoChiusa rest
| _ -> raise Fail


type obj = Miss | Cann | Barca
type situazione = obj list * obj list
let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [])

type azione =
	| From_left of obj list
	| From_right of obj list


let conta n lst = 
	List.length (List.filter ((=) n) lst)


let safe (left, right) = 
	let rec aux riva = 
		let miss = conta Miss riva
		in miss=0 || miss >= conta Cann riva
	in aux left && aux right


type 'a pattern = Jolly | Val of 'a

let conta_jolly lst = 
	List.length (List.filter ((=) Jolly) lst)


let conforme x y =
	y = Val(x) || y = Jolly

let most_general_match lst1 lst2 = 
	