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
        let side1 = l * w in
        let side2 = w * h in
        let side3 = h * l in
        let surface_area = 2 * (side1 + side2 + side3) in
        let extra = List.fold_left min side1 [side2; side3] in
        count := !count + surface_area + extra
    | _ -> ()
  ) input_lines;
  print_endline (string_of_int !count)

let () =
  let input_lines = read_file "input.txt" in
  solve input_lines