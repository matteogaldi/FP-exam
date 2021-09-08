
type color = Rosso | Verde | Neutro

let cols = [(2,Rosso); (3,Verde); (4,Verde); (6,Verde); (7,Rosso)];;

let lst = [1;2;3;4;5;6;7;8;9;10];;


(*aggiungi: color ->(color * 'a)list->(color * 'a)list *)
(*aggiungi: aggiorna lstona aggiungendo +1 al secondo elemento 
  della coppia con colore = col *)
let rec aggiungi col = function
    []->[]]
  |(c,x)::rest-> 
     if c=col 
     then (c,x+1)::rest
     else (c,x)::aggiungi col rest  

(*aux: (color * 'a)list-> 'a list ->(color * 'a)list*)
(*aux lstona list aggiorna lstona tramite aggiungi sccorrendo list *)
let rec contacolori clist list =
  let rec aux lstona = function
      []->lstona
    |x::rest ->
      try  let temp = List.assoc x clist
       in aux(aggiungi temp lstona) rest
      with Not_found -> aux(aggiungi Neutro lstona) rest

   in aux ([(Rosso,0);(Verde,0);(Neutro,0)]) list

(*vicini: 'a -> ('a*'a)list -> 'a list*)
(* vicini n g restituisce la lista dei nodi vicini a n del grafo g  *)
let rec vicini nodo = function
    []->[]
  |(x,y)::rest -> 
     if x = nodo then y:: vicini nodo rest
     else  if y = nodo then x:: vicini nodo rest
     else vicini nodo rest


let grafo_no = [(1,2);(1,3);(2,3);(2,5);(3,4);(3,5);(4,5);(5,6);(5,7);(6,7);(7,8)]

let cols= [(1,Neutro);(5,Neutro);(8,Neutro);(2,Rosso);(7,Rosso);(3,Verde);
           (4,Verde);(6,Verde)]

let lst = [Rosso;Verde;Neutro]


let assoc n cols = try
    List.assoc n cols
with
| Not_found -> raise Error

(*funzione ausiliaria che elimina il primo elemento della lista lst se il primo
 * elemento è uguale ad n e ne restituisce una lista, altrimenti restituisce la lista data)

(remove_first: 'a -> 'a list -> 'a list*)

let remove_first n lst = if ((List.hd lst) = n) then List.tl lst else lst

(*path: 'a graph -> ('a * color) list -> color list -> 'a -> 'a list*)

let path grp cols lst start =
    let rec from_node visited lst n =
        if (List.mem n visited) then
            raise Error else
                if (lst = [(assoc n cols)]) then [n]
                    else n::from_list (n::visited) (remove_first (assoc n cols) lst) (vicini n grp)
    and from_list visited lst = function
        []->raise Error
        |x::rest -> try
            from_node visited lst x
        with
        | Error -> from_list visited lst rest
    in from_node [] lst start

