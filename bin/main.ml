let () =
  let str = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let bs = Bencode.ByteSource.of_string str in
  let v, _ = Option.get (Bencode.parse_value bs) in
  List.iter (fun (k, _) -> print_endline k) (Bencode.Value.dict_bindings v)
;;
