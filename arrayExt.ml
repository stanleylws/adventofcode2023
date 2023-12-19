let is_in_range arr i = i >= 0 && i < Array.length arr

let get_opt arr i =
  if is_in_range arr i
  then Some arr.(i)
  else None