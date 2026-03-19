let read_file file_name =
  let file = open_in file_name in
  let rec read_file_impl accumulated_lines =
    try
      let line = input_line file in
      read_file_impl (line :: accumulated_lines)
    with End_of_file ->
      close_in file;
      List.rev accumulated_lines
  in
  read_file_impl []

let solve input =
  let rec solve_impl index count =
    let count =
      if input.[index] = '(' then
        count + 1
      else if input.[index] = ')' then
        count - 1
      else
        count
    in
    if count = -1 then
      index + 1
    else
      solve_impl (index + 1) count
  in
  let result = solve_impl 0 0 in
  print_endline (string_of_int result)

let input_lines = read_file "input.txt"

let () =
  let s = String.concat "" input_lines in
  solve s