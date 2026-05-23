let () =
  let str = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let bs = Bencode.ByteSource.of_string str in
  let v, _ = Option.get (Bencode.parse_value bs) in
  let m = Bencode.Value.get_dict v in
  let info = Bencode.Value.get_dict (Bencode.Value.StringMap.find "info" m) in
  Printf.printf
    "name = %s\npiece length = %Ld\n"
    (Bytes.to_string
       (Bencode.Value.get_bytestring (Bencode.Value.StringMap.find "name" info)))
    (Bencode.Value.get_int (Bencode.Value.StringMap.find "piece length" info))
;;
