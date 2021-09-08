

type metro = (int * int * string) list

let m = [(1,2,"A"); (2,3,"A");
(3,1,"A");   (2,4,"B"); (4,5,"B"); (4,6,"C"); (6,3,"C"); (5,7,"D");
(6,7,"D")];;



let line m s =
  let rec aux visitati = function
      
      []->[]
    |(a,b,nome)::rest -> 
       if s != nome then aux visitati rest
       else if (List.mem a visitati) && (List.mem b visitati)
       then aux visitati rest
       else if (List.mem a visitati) && (not(List.mem b visitati))
       then b:: aux (b::visitati) rest
       else if (not(List.mem a visitati)) && (List.mem b visitati)
       then a::aux  (a::visitati) rest
       else a::b::aux (a::b::visitati) rest

in aux [] m



let rec vicini  stazione = function
    []->[]
  |(a,b,nome)::rest -> 
     if stazione = a then (b,nome):: vicini stazione rest
     else if stazione = b then (a,nome)::vicini stazione rest
     else vicini stazione rest

let raggiungi m  maxc start goal =
  let rec from_node linea cont visitati n =
    if List.mem n visitati 
    then raise NotFound
    else if n = goal
    then [n]
    else n:: from_list linea cont (n::visitati) (vicini n m)
  and from_list linea cont visitati = function
      []-> raise NotFound
    |(x,s)::rest ->
       try 
           if s=linea && cont< maxc
           then from_node linea cont visitati x
           else if(cont>maxc)
           then failwith "maxc"
           else from_node s (cont+1) visitati x
          
        
       with NotFound ->from_list linea cont visitati rest
  in from_node "" (-1) [] start       



