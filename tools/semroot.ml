let () =
  List.iter
    (fun opcode ->
       match
         Octra_vm.Inference_conformance_template.vm_semantics_root_for_opcode
           ~opcode
       with
       | None -> Printf.printf "%s NONE\n%!" opcode
       | Some root -> Printf.printf "%s %s\n%!" opcode root)
    ["LOAD_F64_LE_FP"; "ARGMAX_FP"]
