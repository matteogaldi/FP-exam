

let rec is_sorted = function
 []| _::[]-> true
  |x::y::rest -> x<=y && is_sorted (y::rest) 


type 'a tree = 
 Empty
  |Tr of 'a * 'a tree * 'a tree

let alb = Tr(3,
           Tr(4,Tr(3,Empty,Empty),Tr(16,Tr(15,Empty,Empty),Empty)),
             Tr(6,Tr(9,Tr(13,Tr(15,Empty,Empty),Empty),Tr(-19,Empty,Empty)),Empty))

let  sorted_branch t x=
  let rec aux temp = function
   Empty -> raise NotFound
  |Tr(y,Empty,Empty) -> 
                if y=x && is_sorted (temp@[y]) then [y]
                else raise NotFound 
  |Tr( y,t1,t2)->
     if is_sorted (temp@[y])
     then y::( try aux (temp@[y]) t1
          with NotFound-> aux (temp@[y]) t2)
     else raise NotFound 
  in aux [] t


type 'a graph = ('a * 'a ) list

let rec successivi n = function
    []->[]
  |(x,y)::rest -> if x=n then y::successivi n rest
    else successivi n rest

let sorted_branch g start goal =
  let rec from_node visitati n =
    if List.mem n visitati 
    then raise NotFound 
    else if is_sorted (visitati@[n]) && n = goal
    then [n]
    else if is_sorted (visitati@[n])
    then n :: from_list (visitati@[n]) (successivi  n g )
  and from_list visitati = function
      []->raise NotFound
    |x::rest -> 
       try from_node visitati x
       with NotFound-> from_list visitati rest
  in from_node [] start
