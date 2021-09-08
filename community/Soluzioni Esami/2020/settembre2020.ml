

let rec remove x = function
    []->[]
  |y::rest -> 
     if x=y then remove x rest
     else y::remove x rest

let remove_ite x list=
  let aux temp = function
      []-> temp
  |y::rest -> 
     if x=y then aux (y::temp) rest
     else aux (y::temp) rest
  in aux [] list

type 'a graph  = ('a *'a) list


let rec successivo nodo = function
    []->[]
  |(x,y)::rest-> 
     if (x=nodo) then y:: successivo nodo rest
     else successivo nodo rest 

let percorso g start tappa target  =
  let rec from_node visitati n=
      if List.mem n visitati
      then raise NotFound
      else if n = target && (List.mem tappa visitati || n=tappa)
      then [n]
      else  n:: from_list (n::visitati) (successivo n g)
  and from_list visitati = function
      []-> raise NotFound
    |x::rest -> 
       try  from_node visitati x
       with NotFound ->from_list visitati rest

  in from_node [] start

let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6); (5,4); (6,5); (6,7)]
