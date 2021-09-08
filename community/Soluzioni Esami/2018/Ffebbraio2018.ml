

let durations_to_end_times list =
  let rec aux somma = function
[]-> []
  |x::rest -> (somma+x):: aux (somma+x) rest
  in aux 0 list


type 'a graph = ('a * 'a) list

type 'a money = ('a * int) list

let rec vicini n = function
    []-> []
  |(x,y) :: rest -> 
     if x=n then y:: vicini n rest
     else if y=n then x:: vicini n rest
     else vicini n rest

let safe_path g wallet start goal init  =
  let rec from_node visitati init n =
    if List.mem n visitati 
    then raise NotFound 
    else if n = goal && init >=0
    then [n]
    else
      try if ((List.assoc n wallet)+init) >0
          then  n::from_list (n::visitati) ((List.assoc n wallet)+init) (vicini n g)
          else raise NotFound 
      with Not_found ->n::from_list (n::visitati) init (vicini n g)
  and from_list visitati init = function
      []-> raise NotFound
   |x::rest ->
      try from_node visitati init x
      with NotFound -> from_list visitati init rest
  in from_node [] init start

let grafo = [('A','B');('A','C');('A','D');('B','E');('C','E');('C','F');('D','F');('E','G');('F','G')]
let wallet =[('C',-7);('D',-15);('F',3);('G',-5)]
