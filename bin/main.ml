let format_size bytes =
  let f = Int64.to_float bytes in
  if f < 1024.0 then Printf.sprintf "%Ld B" bytes
  else if f < 1024.0 ** 2.0 then Printf.sprintf "%.2f KB" (f /. 1024.0)
  else if f < 1024.0 ** 3.0 then Printf.sprintf "%.2f MB" (f /. 1024.0 ** 2.0)
  else Printf.sprintf "%.2f GB" (f /. 1024.0 ** 3.0)
;;

let bs_to_string v =
  Bencode.Value.get_bytestring v |> Byteslice.to_string
;;

let () =
  let s = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  match Bencode.parse_value (Bencode.Parsing.ByteSource.of_string s) with
  | None -> print_endline "parsing failed"
  | Some (v, _) ->
    let find k d = Bencode.Value.StringMap.find k d in
    let find_opt k d = Bencode.Value.StringMap.find_opt k d in
    let dict = Bencode.Value.get_dict v in
    let info = find "info" dict |> Bencode.Value.get_dict in
    let name = find "name" info |> bs_to_string in
    (match find_opt "length" info with
     | Some len ->
       let size = Bencode.Value.get_int len in
       Printf.printf "Name: %s\nSize: %s\n" name (format_size size)
     | None ->
       let files = find "files" info |> Bencode.Value.get_list in
       let total =
         List.fold_left
           (fun acc f ->
             find "length" (Bencode.Value.get_dict f) |> Bencode.Value.get_int |> Int64.add acc)
           0L
           files
       in
       Printf.printf "Name: %s\nTotal size: %s\nFiles:\n" name (format_size total);
       List.iter
         (fun f ->
           let fd = Bencode.Value.get_dict f in
           let len = find "length" fd |> Bencode.Value.get_int in
           let path =
             find "path" fd
             |> Bencode.Value.get_list
             |> List.map bs_to_string
             |> String.concat "/"
           in
           Printf.printf "  %s (%s)\n" path (format_size len))
         files)
;;
