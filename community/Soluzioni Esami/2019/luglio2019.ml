

type 'a tree = 
Empty
  |Tr of 'a * 'a tree * 'a tree

let alb = Tr(3,
                Tr(4,Tr(3,Empty,Empty),Tr(16,Tr(15,Empty,Empty),Empty)),
                    Tr(6,Tr(9,Tr(13,Tr(15,Empty,Empty),Empty),Tr(-19,Empty,Empty)),Empty))

let n_ramo_bin n tr =
  let rec aux somma = function
   Empty -> raise NotFound
  |Tr(x,Empty,Empty)-> if (somma+x) = n then [x] else raise NotFound 
  |Tr(x,t1,t2) -> 
     if (x+somma)<n
     then     
       try x:: aux (somma+x) t1 
       with NotFound-> x::aux (somma+x) t2
     else raise NotFound
in aux 0 tr



type 'a ntree = Ntree of 'a * 'a ntree list

let leaf x = Ntree(x,[])
let nalb = Ntree(5,
    [Ntree(7,
        [leaf 3; leaf 9; leaf 12]);
    Ntree(1,
        [leaf 4; leaf 5; leaf 10]);
    Ntree(3,
        [leaf 9; leaf 7; leaf 10])])


let n_ramo_bin n ntr=
 let rec from_node somma = function
  |Ntree(x,[])-> if (somma+x) = n then [x] else raise NotFound 
  |Ntree(x,tlist) -> 
     if (x+somma)<n
     then x:: from_list (somma+x) tlist
     else raise NotFound
 and from_list somma = function
     []-> raise NotFound
   |x::rest -> try from_node somma x 
     with NotFound -> from_list somma rest
 in from_node 0 ntr
