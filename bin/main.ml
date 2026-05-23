let () =
  let _ = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  ()
;;
