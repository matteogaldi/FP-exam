let verifica (h, m) = 
	h>=0 && h<24 && m>=0 && m<60;; 

let normalizza (h,m) = ((h+m/60) mod 24, m mod 60);;

let somma_ore (h1,m1) (h2,m2) = 
	if verifica (h1, m1) && verifica (h2, m2)
	then normalizza (h1+h2, m1+m2)
else failwith "Formato scorretto"

let rec sumbetween n m = 
	if n > m 
	then 0
	else n + sumbetween (succ n) m;;

let rec sumto = (function
| 0 -> 0
| n -> n + sumto (pred n))

let rec power n k = 
	if k=0 then 1
	else n * power n (k-1)

let rec fib = (function
| 0 -> 0
| 1 -> 1
| n -> fib (n-1) + fib (n-2))