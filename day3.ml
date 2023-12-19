open Utils

let to_chars_arr s = Array.init (String.length s) (fun i -> s.[i])

let to_chars_matrix lines = lines |> List.map to_chars_arr |> Array.of_list

let is_valid_symbol c = c != '.' && not (is_digit c) 

let get_elem m i j =
  match ArrayExt.get_opt m i with
  | None -> None
  | Some row -> ArrayExt.get_opt row j

let pos_range i j_from j_to = List.map (fun j -> (i, j)) (j_from -- j_to)

let adjacent_pos i j len =
  let top_pos = pos_range (i - 1) (j - 1) (j + len) in
  let bottom_pos = pos_range (i + 1) (j - 1) (j + len) in
  let side_pos = [(i, j - 1); (i, j + len)] in
  top_pos @ side_pos @ bottom_pos 

let get_adjacent m i j len =
  adjacent_pos i j len
  |> List.filter_map (fun (i, j) -> get_elem m i j)

let rec get_pos_aux j head f row_i row =
  match ArrayExt.get_opt row j, head with
  | None, None -> []
  | None, Some j_head -> [(row_i, j_head, j - j_head)]
  | Some c, None -> get_pos_aux (j + 1) (if f c then Some j else None) f row_i row
  | Some c, Some j_head -> if f c
                           then get_pos_aux (j + 1) head f row_i row
                           else (row_i, j_head, j - j_head) :: get_pos_aux (j + 1) None f row_i row

let get_pos m f =
  m
  |> Array.mapi (get_pos_aux 0 None f)
  |> Array.to_list
  |> List.flatten

let get_num_pos m = get_pos m is_digit

let get_astr_pos m = get_pos m (fun c -> c == '*')

let is_part_num m (i, j, len) = (get_adjacent m i j len) |> List.exists is_valid_symbol

let get_num m (i, j, len) =
  let num_chars = Array.sub m.(i) j len in
  num_chars |> Array.to_list |> implode |> int_of_string
  
let find_part_nums m =
  m
  |> get_num_pos
  |> List.filter (is_part_num m)
  |> List.map (get_num m)

let rec find_num_edge m i j dir =
  match get_elem m i (j + dir * 1) with
  | None -> j
  | Some c -> if is_digit c
              then find_num_edge m i (j + dir * 1) dir
              else j
              
let find_left_edge m i j = find_num_edge m i j (-1)

let find_right_edge m i j = find_num_edge m i j (+1)

let find_num_pos m i j =
  let j_left = find_left_edge m i j in
  let j_right = find_right_edge m i j in
  (i, j_left, j_right - j_left + 1)

let get_adj_part_num m (i, j, len) =
  let rec get_part_num pos acc =
    match pos with
    | [] -> acc
    | (ii, jj) :: t ->
      match get_elem m ii jj with
      | None -> get_part_num t acc
      | Some c -> if is_digit c
                  then 
                    let p = find_num_pos m ii jj in
                    match first_opt acc with
                    | None -> get_part_num t (p :: acc)
                    | Some pp -> get_part_num t (if p = pp then acc else (p :: acc))
                  else
                    get_part_num t acc
  in
  get_part_num (adjacent_pos i j len) []
  |> List.map (get_num m)

let find_gear_ratios m =
  m
  |> get_astr_pos
  |> List.map (get_adj_part_num m)
  |> List.filter (fun parts -> List.length parts == 2)
  |> List.map prod

let part_1 () =
  Files.read_file_lines "day3_input.txt"
  |> to_chars_matrix
  |> find_part_nums
  |> sum
  |> print_int

let part_2 () =
  Files.read_file_lines "day3_input.txt"
  |> to_chars_matrix
  |> find_gear_ratios
  |> sum
  |> print_int