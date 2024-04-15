let rec eval code =
  let rec eval_bf code pc ptr mem =
    if pc >= String.length code then ()
    else
      match code.[pc] with
      | '>' -> eval_bf code (pc + 1) (ptr + 1) mem
      | '<' -> eval_bf code (pc + 1) (ptr - 1) mem
      | '+' ->
        eval_bf code (pc + 1) ptr
          (Array.mapi (fun i x -> if i = ptr then x + 1 else x) mem)
      | '-' ->
        eval_bf code (pc + 1) ptr
          (Array.mapi (fun i x -> if i = ptr then x - 1 else x) mem)
      | '.' ->
        print_char (Char.chr mem.(ptr));
        eval_bf code (pc + 1) ptr mem
      | ',' ->
        mem.(ptr) <- int_of_char (input_char stdin);
        eval_bf code (pc + 1) ptr mem
      | '[' ->
        if mem.(ptr) = 0 then
          let rec skip code pc cnt =
            if pc >= String.length code then failwith "unmatched '['"
            else if code.[pc] = '[' then skip code (pc + 1) (cnt + 1)
            else if code.[pc] = ']' then
              if cnt = 0 then pc + 1 else skip code (pc + 1) (cnt - 1)
            else skip code (pc + 1) cnt
          in
          eval_bf code (skip code (pc + 1) 0) ptr mem
        else eval_bf code (pc + 1) ptr mem
      | ']' ->
        if mem.(ptr) = 0 then eval_bf code (pc + 1) ptr mem
        else
          let rec back code pc cnt =
            if pc < 0 then failwith "unmatched ']'"
            else if code.[pc] = ']' then back code (pc - 1) (cnt + 1)
            else if code.[pc] = '[' then
              if cnt = 0 then pc + 1 else back code (pc - 1) (cnt - 1)
            else back code (pc - 1) cnt
          in
          eval_bf code (back code (pc - 1) 0) ptr mem
      | _ -> eval_bf code (pc + 1) ptr mem
  in
  eval_bf code 0 0 (Array.make 1000 0)

let () =
  let path = ref "" in
  Arg.parse [] (fun s -> path := s) "usage: bf_interpreter <file>.bf";
  let ic = open_in !path in
  let code = ref "" in
  try
    while true do
      code := !code ^ input_line ic
    done
  with End_of_file ->
    close_in ic;
    eval !code
