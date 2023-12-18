open Utils

let rec get_digits_aux chars lst =
  match chars with
  | [] -> lst
  | h :: t -> get_digits_aux t (if is_digit h then (h :: lst) else lst)

let get_digits chars = 
  get_digits_aux chars []
  |> List.rev

let get_calib_val s =
  explode s
  |> get_digits
  |> fun digits -> [first digits; last digits]
  |> implode
  |> int_of_string

let convert_num s =
  let rec convert_num_aux chars acc prev = 
    match prev, chars with
    | _ , [] -> acc
    | Some 'o', 'n' :: 'e' :: t -> convert_num_aux t (acc ^ "1") (Some 'e')
    | Some 't', 'w' :: 'o' :: t -> convert_num_aux t (acc ^ "2") (Some 'o')
    | Some 't', 'h' :: 'r' :: 'e' :: 'e' :: t -> convert_num_aux t (acc ^ "3") (Some 'e')
    | Some 'e', 'i' :: 'g' :: 'h' :: 't' :: t -> convert_num_aux t (acc ^ "8") (Some 't')
    | Some 'n', 'i' :: 'n' :: 'e' :: t -> convert_num_aux t (acc ^ "9") (Some 'e')
    | _ , 'o' :: 'n' :: 'e' :: t -> convert_num_aux t (acc ^ "1") (Some 'e')
    | _ , 't' :: 'w' :: 'o' :: t -> convert_num_aux t (acc ^ "2") (Some 'o')
    | _ , 't' :: 'h' :: 'r' :: 'e' :: 'e' :: t -> convert_num_aux t (acc ^ "3") (Some 'e')
    | _ , 'f' :: 'o' :: 'u' :: 'r' :: t -> convert_num_aux t (acc ^ "4") (Some 'r')
    | _ , 'f' :: 'i' :: 'v' :: 'e' :: t -> convert_num_aux t (acc ^ "5") (Some 'e')
    | _ , 's' :: 'i' :: 'x' :: t -> convert_num_aux t (acc ^ "6") (Some 'x')
    | _ , 's' :: 'e' :: 'v' :: 'e' :: 'n' :: t -> convert_num_aux t (acc ^ "7") (Some 'n')
    | _ , 'e' :: 'i' :: 'g' :: 'h' :: 't' :: t -> convert_num_aux t (acc ^ "8") (Some 't')
    | _ , 'n' :: 'i' :: 'n' :: 'e' :: t -> convert_num_aux t (acc ^ "9") (Some 'e')
    | _ , h :: t -> convert_num_aux t (acc ^ (String.make 1 h)) (Some h) in
  convert_num_aux (explode s) "" None

let part_1 () =
  Files.read_file_lines "day1_input.txt" 
  |> List.map get_calib_val 
  |> sum 
  |> string_of_int 
  |> print_endline

let part_2 () =
  Files.read_file_lines "day1_input.txt"
  |> List.map convert_num 
  |> List.map get_calib_val 
  |> sum 
  |> string_of_int 
  |> print_endline
  