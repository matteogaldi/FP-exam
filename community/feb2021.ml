
type 'a ntree = Tr of 'a * 'a ntree list

(*  aggiungi : int -> int list -> int list
    aggiungi x [y1;...;yn] = [x+y1;...;x+yn|] *)
let rec aggiungi x = function
    [] -> []
  | y::rest -> (x+y) :: aggiungi x rest

(*  auxlist : int ntree list -> int list 
    auxlist tlist = lista con tutti i pesi dei rami di ciascun albero in tlist *)
let rec pesi (Tr(x,tlist)) =
  match tlist with
    [] -> [x]
  | _ -> aggiungi x (auxlist tlist)
and auxlist  = function
    [] -> []
  | t::rest -> pesi t @ auxlist rest

(* piccola variante, che non usa aggiungi *)
let rec pesi (Tr(x,tlist)) =
  match tlist with
    [] -> [x]
  | _ -> List.map ((+) x) (auxlist tlist)
and auxlist  = function
    [] -> []
  | t::rest -> pesi t @ auxlist rest

(* versione senza mutua ricorsione *)
let rec pesi (Tr(x,tlist)) =
  match tlist with
    [] -> [x]
  | _ ->
      List.map ((+) x) 
	(List.concat (List.map pesi tlist))


