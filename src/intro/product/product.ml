let firstNumber = float_of_string Sys.argv.(1);;
let secondNumber = float_of_string Sys.argv.(2);;
let product = firstNumber *. secondNumber;;

let _ = Printf.printf "Produto dos dois numeros: %f \n" product;;