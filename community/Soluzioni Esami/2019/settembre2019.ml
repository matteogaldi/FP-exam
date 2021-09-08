

let rec elimina n = function
    []-> []
  |x::rest -> if x= n then rest
    else x::elimina n rest


let rec complemento superset = function
    []-> superset
  |x::rest -> if List.mem x superset then complemento (elimina x superset) rest
    else raise NotFound


type 'a tree = 
Empty
  |Tr of 'a * 'a tree * 'a tree

let alb = Tr(1,
                Tr(2,Tr(4,Tr(5,Empty,Empty),Tr(6,Empty,Empty)),Tr(4,Tr(7,Empty,Empty),Empty)),
                Tr(3,Tr(8,Tr(9,Empty,Empty),Tr(10,Empty,Empty)),Tr(2,Tr(4,Tr(3,Empty,Empty),Empty),Tr(11,Empty,Empty))))

let rec discendenti n = function
Empty-> []
  |Tr(x,Empty,Empty)-> if x = n then [x] else []
  |Tr(x,t1,t2) ->
    if  n=x then x:: (sottoalbero t1) @ (sottoalbero t2)
    else (discendenti n t1)@(discendenti n t2)

    

let rec sottoalbero = function
Empty ->raise NotFound
  |Tr(x,Empty,Empty) -> [x]
  |Tr(x,t1,t2)->
     try  x:: (sottoalbero t1)
     with NotFound -> x:: (sottoalbero t2)




