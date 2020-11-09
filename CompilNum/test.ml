let square x = x * x;;

let x = 4.6 in
if x = 5.0 then 1.9 else 5.3 ;;


let x = 4.0 in
let y = 5.0  in
  x + y ;;


let x = 2.0 in
let k = 3.0 in
let y = 3.0 * x * k in
let z = 4.0 * (y + 1.0) in
 (x + y + z) ;;

let m = (3 , 6) in
 fst m ;;


let volume_cylindre=
let x = 4.0 in 
let h = 3.0 in 
let carre  = x *x in
let pi = 3.141592654 in
 pi * (carre ) * h  ;;
let moy = volume_cylindre + 4.0 ;;
 

let k = [4 ; 6 ; 8 ] in
 hd k ;;

let average a b =
    let sum = a + b in
    sum / 2.0 ;;
