(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type entrypoint = {
  entry_name : string;
  entry_label : int;
}

type t = {
  program_root : string;
  requirement_root : string;
  model_root : string;
  execution_descriptor_root : string;
  store_root : string;
  session_abi_root : string;
  entrypoints : entrypoint list;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_entrypoint of string * int
  | Uncertified_entrypoint of string * int
  | Uncertified_program
  | Duplicate_entrypoint of string
  | Program_root_mismatch of string * string
  | Missing_requirement
  | Requirement_root_mismatch of string * string

let hex = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let valid_root value =
  String.length value = 64 && String.for_all hex value

let name_char = function
  | 'a' .. 'z'
  | '0' .. '9'
  | '.'
  | '_'
  | '-' -> true
  | _ -> false

let valid_name value =
  value <> "" && String.for_all name_char value

let check_root value =
  if valid_root value then Ok () else Error (Bad_root value)

let check_name value =
  if valid_name value then Ok () else Error (Bad_name value)

let entrypoint_order left right =
  match String.compare left.entry_name right.entry_name with
  | 0 -> compare left.entry_label right.entry_label
  | order -> order

let sort_entrypoints entrypoints =
  List.sort entrypoint_order entrypoints

let entrypoint_json entrypoint =
  `Assoc [
    "name", `String entrypoint.entry_name;
    "label", `Int entrypoint.entry_label;
  ]

let to_json target =
  `Assoc [
    "program_root", `String target.program_root;
    "requirement_root", `String target.requirement_root;
    "model_root", `String target.model_root;
    "execution_descriptor_root", `String target.execution_descriptor_root;
    "store_root", `String target.store_root;
    "session_abi_root", `String target.session_abi_root;
    "entrypoints",
    `List (List.map entrypoint_json (sort_entrypoints target.entrypoints));
  ]

let root target =
  let payload = Yojson.Safe.to_string (to_json target) in
  Digestif.SHA256.(
    digest_string ("octra:inference:target\000" ^ payload) |> to_hex)

let program_root admitted =
  Digestif.SHA256.(
    digest_string
      ("octra:inference:program\000"
       ^ Bytecode.encode (Admission.code admitted))
    |> to_hex)

let rec check_entrypoints seen = function
  | [] -> Ok ()
  | entrypoint :: rest ->
    if List.mem entrypoint.entry_name seen then
      Error (Duplicate_entrypoint entrypoint.entry_name)
    else
      match check_name entrypoint.entry_name with
      | Error error -> Error error
      | Ok () ->
        if entrypoint.entry_label < 0 then
          Error (Bad_entrypoint (entrypoint.entry_name, entrypoint.entry_label))
        else
          check_entrypoints (entrypoint.entry_name :: seen) rest

let validate target =
  match check_root target.program_root with
  | Error error -> Error error
  | Ok () ->
    (match check_root target.requirement_root with
     | Error error -> Error error
     | Ok () ->
       (match check_root target.model_root with
        | Error error -> Error error
        | Ok () ->
          (match check_root target.execution_descriptor_root with
           | Error error -> Error error
           | Ok () ->
             (match check_root target.store_root with
              | Error error -> Error error
              | Ok () ->
                (match check_root target.session_abi_root with
                 | Error error -> Error error
                 | Ok () -> check_entrypoints [] target.entrypoints)))))

let jdest_labels code =
  Array.fold_left
    (fun labels instr ->
      match instr with
      | Contract_vm.JDEST label -> label :: labels
      | _ -> labels)
    []
    code

let check_entrypoint_labels code entrypoints =
  let labels = jdest_labels code in
  let rec loop = function
    | [] -> Ok ()
    | entrypoint :: rest ->
      if entrypoint.entry_label <> 100 then
        Error (Uncertified_entrypoint
                 (entrypoint.entry_name, entrypoint.entry_label))
      else if List.mem entrypoint.entry_label labels then loop rest
      else Error (Bad_entrypoint (entrypoint.entry_name, entrypoint.entry_label))
  in
  loop entrypoints

let entry_label target name =
  match
    List.find_opt
      (fun entrypoint -> String.equal entrypoint.entry_name name)
      target.entrypoints
  with
  | Some entrypoint -> Some entrypoint.entry_label
  | None -> None

let check ~admitted target =
  match validate target with
  | Error error -> Error error
  | Ok () ->
    let actual_program_root = program_root admitted in
    if not (String.equal target.program_root actual_program_root) then
      Error (Program_root_mismatch (target.program_root, actual_program_root))
    else
      match Admission.requirement admitted with
      | None -> Error Missing_requirement
      | Some requirement ->
        let actual_requirement_root = Execution_requirement.root requirement in
        if not (String.equal target.requirement_root actual_requirement_root) then
          Error (Requirement_root_mismatch
                   (target.requirement_root, actual_requirement_root))
        else if not (Admission.certified_source admitted) then
          Error Uncertified_program
        else
          check_entrypoint_labels
            (Admission.code admitted)
            target.entrypoints

let error_message = function
  | Bad_root root -> Printf.sprintf "invalid root: %s" root
  | Bad_name name -> Printf.sprintf "invalid entrypoint name: %s" name
  | Bad_entrypoint (name, label) ->
    Printf.sprintf "invalid entrypoint %s: %d" name label
  | Uncertified_entrypoint (name, label) ->
    Printf.sprintf "uncertified entrypoint %s: %d" name label
  | Uncertified_program -> "uncertified inference program"
  | Duplicate_entrypoint name ->
    Printf.sprintf "duplicate entrypoint: %s" name
  | Program_root_mismatch (expected, actual) ->
    Printf.sprintf
      "target program root mismatch: expected %s actual %s"
      expected actual
  | Missing_requirement ->
    "admitted program has no execution requirement"
  | Requirement_root_mismatch (expected, actual) ->
    Printf.sprintf
      "target requirement root mismatch: expected %s actual %s"
      expected actual
