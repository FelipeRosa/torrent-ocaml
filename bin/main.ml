let () =
  let s = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  match Bencode.parse_value (Bencode.Parsing.ByteSource.of_string s) with
  | None -> print_endline "parsing failed"
  | Some (v, _) ->
    v
    |> Bencode.Value.get_dict
    |> Bencode.Value.StringMap.iter (fun k _ -> print_endline k)
;;
