
(*PARTE1*)

let e1 =
  let pi = 2. *. asin 1. in pi *. (pi +. 1.);;
(*SOLUCION*)
let e1 =
  (function pi -> pi *. (pi +. 1.)) (2. *. asin 1.);;


let e2 =
  let lg2 = log 2. in
  let log2 = function x -> log x /. lg2
  in log2 (float (1024 * 1024));;
(*SOLUCION*)
let e2 = 
  (function lg2 -> function log2 -> log log2 /. lg2) (log 2.) (float (1024*1024));;


let e3 =
  let pi_2 = 4. *. asin 1. in
  function r -> pi_2 *. r;;
(*SOLUCION*)
let e3 =
  (function pi_2 -> function r -> pi_2 *. r) (4. *. asin 1.);;


let e4 =
  let sqr = function x -> x *. x in
  let pi = 2. *. asin 1. in
  function r -> pi *. sqr r;;
(*SOLUCION*)
let e4= 
  (function sqr -> function pi -> function r -> pi *. sqr r)(function x-> x *. x)(2. *. asin 1.);;
  
  
 (*PARTE 2*)
 
 let abs n = if n >= 0 then n else - n;;
 (*SOLUCION*)
 let abs n =
 (function true -> n | false -> -n) (n >= 0);;
 	
 
 let par n = if n mod 2 = 0 then true else false;;
 (*SOLUCION*)
 let par n =
 (function true -> true | false -> false) (n mod 2 = 0);;
 
 
 let saluda s = if s = "Hola" then print_endline "Hola!" else ();;
 (*SOLUCION*)
 let saluda s =
 (function true -> print_endline "Hola!" | false -> ()) (s = "Hola");;
 
 let f n = if n mod 2 = 0 then "es par" else "es impar";;
 (*SOLUCION*)
 let f n =
 (function true -> "es par" | false -> "es impar") (n mod 2 = 0);;
 
 
 let f n = if n mod 2 = 0 then "m ́ultiplo de 2"else if n mod 3 = 0 then "m ́ultiplo de 3"else "impar";;
(*SOLUCION*)
let f n =
(function true -> "múltiplo de 2" | false -> ((function true -> "multiplo de 3" | false -> "impar ") (n mod 3 = 0))) (n mod 2 = 0);; 
 
 
 
