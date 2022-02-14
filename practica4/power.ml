
(*funcion elevar un numero a otro*)
let rec power (x:int) (y:int) =
 if  y = 1  then x
 else x * power x (y-1);; 

(*funcion mejorada de power ya que no es recursiva y por lo tanto solo una iteracion ya nos dara el resultado*)
let power' x y =
if y mod 2 = 0 then int_of_float((float_of_int(x * x)) ** (float_of_int(y / 2)))
else int_of_float(float_of_int(x*int_of_float((float_of_int(x*x))**float_of_int(y/2))));;


(*funcion elevar numero tipo float a int*)
let rec powerf (x:float) (y:int) =
 if  y = 1  then x
 else x *. powerf x (y-1);;



