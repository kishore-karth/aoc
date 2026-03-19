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

let solve input_lines =
  let count = ref 0 in
  List.iter (fun line ->
    match String.split_on_char 'x' line |> List.map int_of_string with
    | [l; w; h] ->
        let max_len = max l (max w h) in
        count := !count + (2 * l + 2 * w + 2 * h) - (2 * max_len) + l * w * h
    | _ -> ()
  ) input_lines;
  print_endline (string_of_int !count)

let () =
  let input_lines = read_file "input.txt" in
  solve input_lines