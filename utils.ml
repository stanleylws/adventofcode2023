let first lst =
  match lst with
  | [] -> failwith "Cannot operate on an empty list"
  | x :: _ -> x

let first_opt lst =
  match lst with
  | [] -> None
  | x :: _ -> Some x
  
let rec last lst =
  match lst with
  | [] -> failwith "Cannot operate on an empty list"
  | [x] -> x
  | _ :: t -> last t

let is_digit c =
  let int_val = int_of_char c in
  int_val >= 48 && int_val <= 57

let sum nums =
  let rec sum_aux list acc =
    match list with
    | [] -> acc
    | h :: t -> sum_aux t (h + acc) in
  sum_aux nums 0

let prod nums =
  let rec prod_aux list acc =
    match list with
    | [] -> acc
    | h :: t -> prod_aux t (h * acc) in
  match nums with
  | [] -> 0
  | _ -> prod_aux nums 1

let explode s = 
  let rec exp idx lst =
    if idx < 0 then lst
    else exp (idx - 1) (s.[idx] :: lst) in
  exp (String.length s - 1) []

let rec implode chars =
  match chars with
  | [] -> ""
  | h :: t -> (String.make 1 h) ^ implode t

let rec from i j acc =
  if i > j then acc
  else from i (j - 1) (j :: acc)

let ( -- ) i j = from i j []