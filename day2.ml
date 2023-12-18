open Utils

let get_int c1 c2 =
  if is_digit c1 then int_of_string (implode [c1; c2])
  else int_of_string (String.make 1 c2)

let get_num_cubes s =
  let rec get_num_cubes_aux chars r g b =
    match chars with
    | [] -> [r;g;b]
    | c1 :: c2 :: ' ' :: 'r' :: t -> get_num_cubes_aux t (max r (get_int c1 c2)) g b
    | c1 :: c2 :: ' ' :: 'g' :: t -> get_num_cubes_aux t r (max g (get_int c1 c2)) b
    | c1 :: c2 :: ' ' :: 'b' :: t -> get_num_cubes_aux t r g (max b (get_int c1 c2))
    | _ :: t -> get_num_cubes_aux t r g b in
  get_num_cubes_aux (explode s) 0 0 0

let determine max_r max_g max_b lst =
  match lst with
  | [r;g;b] -> r <= max_r && g <= max_g && b <= max_b
  | _ -> false

let get_game_ids lst =
  let rec get_game_ids_aux lst' ids idx =
    match lst' with
    | [] -> ids
    | h :: t -> get_game_ids_aux t (if h then (idx + 1::ids) else ids) (idx + 1) in
  get_game_ids_aux lst [] 0

let rec print_list lst =
  match lst with
  | [] -> print_newline ()
  | h :: t -> let _ = print_string ((string_of_int h) ^ ";") in print_list t

let rec power lst =
  match lst with
  | [] -> 1
  | h :: t -> h * power t

let part_1 () = 
  Files.read_file_lines "day2_input.txt"
  |> List.map get_num_cubes
  |> List.map (determine 12 13 14)
  |> get_game_ids
  |> sum
  |> string_of_int
  |> print_endline

let part_2 () = 
  Files.read_file_lines "day2_input.txt"
  |> List.map get_num_cubes
  |> List.map power
  |> sum
  |> string_of_int
  |> print_endline
