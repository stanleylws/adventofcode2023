let read_lines ic =
  let rec read_lines_aux ic lines =
    try
      read_lines_aux ic (input_line ic :: lines);
    with End_of_file -> lines in
  read_lines_aux ic []
  |> List.rev

let read_file_lines file =
  let ic = open_in file in
  try
    let lines = read_lines ic in
    close_in ic;
    lines
  with e ->
    close_in_noerr ic;
    raise e