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

type memory_access =
  | Read
  | Write
  | Read_write

type register_binding = {
  register : int;
  name : string;
  kind : string;
  value : string;
}

type memory_binding = {
  name : string;
  path : string option;
  root : string;
  byte_length : int option;
  sha256 : string option;
  encoding : string;
  base : int;
  cells : int;
  access : memory_access;
}

type expected_span = {
  span_name : string;
  base : int;
  cells : int;
  output_root : string option;
  output_sha256 : string option;
}

type failure_case = {
  case_name : string;
  expected : string;
  mutations : string list;
  unchanged_spans : string list;
}

type t = {
  opcode : string;
  primitive : string;
  profile : string;
  vm_semantics_root : string;
  numerical_profile_root : string;
  expected_effort : int;
  effects : string list;
  registers : register_binding list;
  memory : memory_binding list;
  expected : expected_span list;
  failure_cases : failure_case list;
}

type error =
  | Json_error of string
  | Template_error of string

type requirement = {
  opcode : string;
  primitives : string list;
  effects : string list;
}

let error_message = function
  | Json_error message -> "invalid conformance template json: " ^ message
  | Template_error message -> "invalid conformance template: " ^ message

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error error -> Error error

let requirements = [
  {
    opcode = "LINEAR_Q1_G128_FP";
    primitives = ["linear_q1_0_g128_fp"; "q1_g128_projection"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "RMSNORM_FP_EPS";
    primitives = ["rmsnorm_fp_eps"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "L2NORM_FP";
    primitives = ["l2norm_fp"; "l2norm_fp_eps"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "SOFTMAX_FP";
    primitives = ["softmax_fp"];
    effects = ["memory_read"; "memory_write"];
  };
  {
    opcode = "GATED_DELTA_RULE_FP";
    primitives = ["gated_delta_rule_fp"];
    effects = ["memory_read"; "memory_write"];
  };
]

let p0_opcodes = List.map (fun requirement -> requirement.opcode) requirements

let assoc name = function
  | `Assoc fields -> Ok fields
  | _ -> Error (Json_error ("expected object: " ^ name))

let field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Ok value
  | [] -> Error (Json_error ("missing field: " ^ name))
  | _ -> Error (Json_error ("duplicate field: " ^ name))

let optional_field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [] -> Ok None
  | [(_, value)] -> Ok (Some value)
  | _ -> Error (Json_error ("duplicate field: " ^ name))

let check_known fields names =
  match
    List.find_opt
      (fun (name, _) -> not (List.exists (String.equal name) names))
      fields
  with
  | None -> Ok ()
  | Some (name, _) -> Error (Json_error ("unknown field: " ^ name))

let string_field name fields =
  match field name fields with
  | Ok (`String value) -> Ok value
  | Ok _ -> Error (Json_error ("field must be a string: " ^ name))
  | Error error -> Error error

let optional_string_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some `Null) -> Ok None
  | Ok (Some (`String value)) -> Ok (Some value)
  | Ok (Some _) -> Error (Json_error ("field must be a string: " ^ name))
  | Error error -> Error error

let int_field name fields =
  match field name fields with
  | Ok (`Int value) -> Ok value
  | Ok _ -> Error (Json_error ("field must be an int: " ^ name))
  | Error error -> Error error

let optional_int_field name fields =
  match optional_field name fields with
  | Ok None -> Ok None
  | Ok (Some `Null) -> Ok None
  | Ok (Some (`Int value)) -> Ok (Some value)
  | Ok (Some _) -> Error (Json_error ("field must be an int: " ^ name))
  | Error error -> Error error

let list_field name fields =
  match field name fields with
  | Ok (`List values) -> Ok values
  | Ok _ -> Error (Json_error ("field must be a list: " ^ name))
  | Error error -> Error error

let assoc_field name fields =
  match field name fields with
  | Ok (`Assoc fields) -> Ok fields
  | Ok _ -> Error (Json_error ("field must be an object: " ^ name))
  | Error error -> Error error

let string_list_field name fields =
  let* values = list_field name fields in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | `String value :: rest -> loop (value :: acc) rest
    | _ :: _ -> Error (Json_error ("field must be a string list: " ^ name))
  in
  loop [] values

let relative_path_ok path =
  String.length path > 0
  && path.[0] <> '/'
  && not (String.contains path '\\')
  && List.for_all
       (fun part -> part <> "" && part <> "." && part <> "..")
       (String.split_on_char '/' path)

let optional_exists predicate = function
  | None -> false
  | Some value -> predicate value

let hex_char = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let root_ok value =
  String.length value = 64
  && String.for_all hex_char value

let require_string expected name fields =
  let* actual = string_field name fields in
  if String.equal actual expected then Ok ()
  else
    Error
      (Template_error
         (Printf.sprintf
            "field %s expected %s actual %s"
            name
            expected
            actual))

let rec parse_list parse acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    let* parsed = parse value in
    parse_list parse (parsed :: acc) rest

let parse_access = function
  | "read" -> Ok Read
  | "write" -> Ok Write
  | "read_write" -> Ok Read_write
  | value -> Error (Template_error ("unknown memory access: " ^ value))

let access_string = function
  | Read -> "read"
  | Write -> "write"
  | Read_write -> "read_write"

let parse_register value =
  let* fields = assoc "register binding" value in
  let* () = check_known fields ["register"; "name"; "kind"; "value"] in
  let* register = int_field "register" fields in
  let* name = string_field "name" fields in
  let* kind = string_field "kind" fields in
  let* value = string_field "value" fields in
  if register < 0 || register > 63 then
    Error (Template_error ("register out of range: " ^ string_of_int register))
  else Ok { register; name; kind; value }

let parse_memory value =
  let* fields = assoc "memory binding" value in
  let* () =
    check_known
      fields
      [
        "name"; "path"; "root"; "byte_length"; "sha256"; "encoding";
        "base"; "cells"; "access";
      ]
  in
  let* name = string_field "name" fields in
  let* path = optional_string_field "path" fields in
  let* root = string_field "root" fields in
  let* byte_length = optional_int_field "byte_length" fields in
  let* sha256 = optional_string_field "sha256" fields in
  let* encoding = string_field "encoding" fields in
  let* base = int_field "base" fields in
  let* cells = int_field "cells" fields in
  let* access_name = string_field "access" fields in
  let* access = parse_access access_name in
  if optional_exists (fun path -> not (relative_path_ok path)) path then
    Error (Template_error ("memory path must be relative: " ^ name))
  else if
    (access = Read || access = Read_write)
    && (path = None || byte_length = None || sha256 = None)
  then
    Error
      (Template_error
         ("readable memory requires path, byte_length, and sha256: " ^ name))
  else if optional_exists (fun length -> length <= 0) byte_length then
    Error (Template_error ("byte_length must be positive: " ^ name))
  else if base < 0 then
    Error (Template_error ("memory base must be non-negative: " ^ name))
  else if cells <= 0 then
    Error (Template_error ("memory cells must be positive: " ^ name))
  else Ok { name; path; root; byte_length; sha256; encoding; base; cells; access }

let parse_expected_span value =
  let* fields = assoc "expected span" value in
  let* () =
    check_known
      fields
      ["name"; "base"; "cells"; "output_root"; "output_sha256"]
  in
  let* span_name = string_field "name" fields in
  let* base = int_field "base" fields in
  let* cells = int_field "cells" fields in
  let* output_root = optional_string_field "output_root" fields in
  let* output_sha256 = optional_string_field "output_sha256" fields in
  if base < 0 then
    Error (Template_error ("expected span base must be non-negative: " ^ span_name))
  else if cells <= 0 then
    Error (Template_error ("expected span cells must be positive: " ^ span_name))
  else if output_root = None && output_sha256 = None then
    Error
      (Template_error
         ("expected span output_root or output_sha256 is required: " ^ span_name))
  else Ok { span_name; base; cells; output_root; output_sha256 }

let parse_expected fields =
  let* fields = assoc_field "expected" fields in
  let* () = check_known fields ["spans"] in
  let* values = list_field "spans" fields in
  parse_list parse_expected_span [] values

let parse_failure_case value =
  let* fields = assoc "failure case" value in
  let* () =
    check_known fields ["case"; "expected"; "mutations"; "unchanged_spans"]
  in
  let* case_name = string_field "case" fields in
  let* expected = string_field "expected" fields in
  let* mutations = string_list_field "mutations" fields in
  let* unchanged_spans = string_list_field "unchanged_spans" fields in
  if mutations = [] then
    Error (Template_error ("failure case mutations are required: " ^ case_name))
  else if unchanged_spans = [] then
    Error
      (Template_error
         ("failure case unchanged_spans are required: " ^ case_name))
  else Ok { case_name; expected; mutations; unchanged_spans }

let requirement_for_opcode opcode =
  List.find_opt (fun item -> String.equal item.opcode opcode) requirements

let check_primitive requirement primitive =
  if List.exists (String.equal primitive) requirement.primitives then Ok ()
  else
    Error
      (Template_error
         (Printf.sprintf
            "primitive %s does not match opcode %s"
            primitive
            requirement.opcode))

let missing_effects required actual =
  List.filter
    (fun effect -> not (List.exists (String.equal effect) actual))
    required

let has_writable_memory memory =
  List.exists
    (fun item ->
       match item.access with
       | Write
       | Read_write -> true
       | Read -> false)
    memory

let duplicate_by key values =
  let seen = Hashtbl.create 16 in
  List.find_opt
    (fun value ->
       let key = key value in
       if Hashtbl.mem seen key then true
       else begin
         Hashtbl.add seen key ();
         false
       end)
    values

let validate (template : t) =
  match requirement_for_opcode template.opcode with
  | None -> Error (Template_error ("unsupported P0 opcode: " ^ template.opcode))
  | Some requirement ->
    let* () = check_primitive requirement template.primitive in
    let* _profile =
      match
        Inference_numerical_profile.validate_for_opcode
          ~opcode:template.opcode
          ~profile:template.profile
      with
      | Ok profile -> Ok profile
      | Error error ->
        Error
          (Template_error
             (Inference_numerical_profile.error_message error))
    in
    let missing = missing_effects requirement.effects template.effects in
    if missing <> [] then
      Error
        (Template_error
           ("missing required effects: " ^ String.concat "," missing))
    else if not (root_ok template.vm_semantics_root) then
      Error (Template_error "vm_semantics_root must be a 32-byte hex root")
    else if not (root_ok template.numerical_profile_root) then
      Error
        (Template_error "numerical_profile_root must be a 32-byte hex root")
    else if template.registers = [] then
      Error (Template_error "register bindings are required")
    else if template.memory = [] then
      Error (Template_error "memory bindings are required")
    else if not (has_writable_memory template.memory) then
      Error (Template_error "at least one writable memory binding is required")
    else if template.expected = [] then
      Error (Template_error "expected spans are required")
    else if
      String.equal template.opcode "GATED_DELTA_RULE_FP"
      && List.length template.expected < 2
    then
      Error
        (Template_error
           "GATED_DELTA_RULE_FP requires output and next-state expected spans")
    else if template.failure_cases = [] then
      Error (Template_error "failure cases are required")
    else
      match
        duplicate_by (fun (item : register_binding) -> item.register)
          template.registers
      with
      | Some item ->
        Error
          (Template_error
             ("duplicate register binding: " ^ string_of_int item.register))
      | None ->
        (match duplicate_by (fun (item : memory_binding) -> item.name) template.memory with
         | Some item ->
           Error (Template_error ("duplicate memory binding: " ^ item.name))
         | None ->
           (match duplicate_by (fun (item : expected_span) -> item.span_name)
                    template.expected with
            | Some item ->
              Error
                (Template_error
                   ("duplicate expected span: " ^ item.span_name))
            | None -> Ok template))

let of_json json =
  let* fields = assoc "conformance template" json in
  let* () =
    check_known
      fields
      [
        "type"; "schema"; "opcode"; "primitive"; "profile";
        "vm_semantics_root"; "numerical_profile_root"; "expected_effort";
        "effects"; "registers"; "memory"; "expected"; "failure_cases";
      ]
  in
  let* () =
    require_string "litenode_vm_conformance_template" "type" fields
  in
  let* schema = int_field "schema" fields in
  let* opcode = string_field "opcode" fields in
  let* primitive = string_field "primitive" fields in
  let* profile = string_field "profile" fields in
  let* vm_semantics_root = string_field "vm_semantics_root" fields in
  let* numerical_profile_root = string_field "numerical_profile_root" fields in
  let* expected_effort = int_field "expected_effort" fields in
  let* effects = string_list_field "effects" fields in
  let* register_values = list_field "registers" fields in
  let* memory_values = list_field "memory" fields in
  let* expected = parse_expected fields in
  let* failure_values = list_field "failure_cases" fields in
  if schema <> 1 then
    Error
      (Template_error ("unsupported schema: " ^ string_of_int schema))
  else if expected_effort < 0 then
    Error (Template_error "expected_effort must be non-negative")
  else
    let* registers = parse_list parse_register [] register_values in
    let* memory = parse_list parse_memory [] memory_values in
    let* failure_cases = parse_list parse_failure_case [] failure_values in
    validate {
      opcode;
      primitive;
      profile;
      vm_semantics_root;
      numerical_profile_root;
      expected_effort;
      effects;
      registers;
      memory;
      expected;
      failure_cases;
    }

let register_json (item : register_binding) =
  `Assoc [
    "register", `Int item.register;
    "name", `String item.name;
    "kind", `String item.kind;
    "value", `String item.value;
  ]

let optional_string_json = function
  | None -> `Null
  | Some value -> `String value

let memory_json (item : memory_binding) =
  `Assoc [
    "name", `String item.name;
    "path", optional_string_json item.path;
    "root", `String item.root;
    "byte_length",
    (match item.byte_length with None -> `Null | Some value -> `Int value);
    "sha256", optional_string_json item.sha256;
    "encoding", `String item.encoding;
    "base", `Int item.base;
    "cells", `Int item.cells;
    "access", `String (access_string item.access);
  ]

let expected_span_json (item : expected_span) =
  `Assoc [
    "name", `String item.span_name;
    "base", `Int item.base;
    "cells", `Int item.cells;
    "output_root", optional_string_json item.output_root;
    "output_sha256", optional_string_json item.output_sha256;
  ]

let failure_json (item : failure_case) =
  `Assoc [
    "case", `String item.case_name;
    "expected", `String item.expected;
    "mutations", `List (List.map (fun value -> `String value) item.mutations);
    "unchanged_spans",
    `List (List.map (fun value -> `String value) item.unchanged_spans);
  ]

let profile_json (template : t) =
  match
    Inference_numerical_profile.validate_for_opcode
      ~opcode:template.opcode
      ~profile:template.profile
  with
  | Ok profile ->
    Inference_numerical_profile.to_json_for_opcode
      ~opcode:template.opcode
      profile
  | Error error ->
    `Assoc [
      "name", `String template.profile;
      "consensus_status", `String "invalid";
      "error", `String (Inference_numerical_profile.error_message error);
    ]

let to_json (template : t) =
  let profile = profile_json template in
  let consensus_status =
    match profile with
    | `Assoc fields ->
      (match List.assoc_opt "consensus_status" fields with
       | Some (`String status) -> status
       | _ -> "unknown")
    | _ -> "unknown"
  in
  `Assoc [
    "status", `String "accepted";
    "diagnostic_only", `Bool true;
    "opcode", `String template.opcode;
    "primitive", `String template.primitive;
    "profile", `String template.profile;
    "profile_gate", profile;
    "consensus_status", `String consensus_status;
    "vm_semantics_root", `String template.vm_semantics_root;
    "numerical_profile_root", `String template.numerical_profile_root;
    "expected_effort", `Int template.expected_effort;
    "effects", `List (List.map (fun value -> `String value) template.effects);
    "register_count", `Int (List.length template.registers);
    "memory_binding_count", `Int (List.length template.memory);
    "expected_span_count", `Int (List.length template.expected);
    "failure_case_count", `Int (List.length template.failure_cases);
    "registers", `List (List.map register_json template.registers);
    "memory", `List (List.map memory_json template.memory);
    "expected", `List (List.map expected_span_json template.expected);
    "failure_cases", `List (List.map failure_json template.failure_cases);
  ]
