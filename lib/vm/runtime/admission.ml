(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  admitted_code : Contract_vm.instr array;
  admitted_effects : Program_effects.t;
  profile : profile;
  requirement : Execution_requirement.t option;
  provenance : provenance;
}

and profile =
  | Legacy
  | Program of Program_type_flow.facts

and provenance =
  | Raw_code
  | Checked_envelope
  | Attested_envelope

type error =
  | Decode_error of string
  | Verify_error of string
  | Unsafe_error of string

let verifier_error = function
  | Contract_vm.Verifier.InvalidReg (pc, reg) ->
    Printf.sprintf "invalid register r%d at pc %d" reg pc
  | Contract_vm.Verifier.InvalidRegSpan (pc, base, count) ->
    Printf.sprintf "invalid register span r%d+%d at pc %d" base count pc
  | Contract_vm.Verifier.InvalidJumpDest dest ->
    Printf.sprintf "invalid jump destination %d" dest
  | Contract_vm.Verifier.DuplicateJDest name ->
    Printf.sprintf "duplicate JDEST %d" name
  | Contract_vm.Verifier.CodeTooLarge size ->
    Printf.sprintf "code too large: %d instructions" size
  | Contract_vm.Verifier.EmptyCode ->
    "empty code"
  | Contract_vm.Verifier.ReservedKey (pc, _) ->
    Printf.sprintf "write to reserved key at pc %d" pc

let admit ~program code =
  match Contract_vm.Verifier.verify code with
  | Error err -> Error (Verify_error (verifier_error err))
  | Ok () ->
    let policy_error =
      if program then
        Option.map
          (fun hit -> `Consensus_unsafe hit)
          (Opcode_policy.first_host_float code)
      else
        match Opcode_policy.legacy_error code with
        | Some (Opcode_policy.Program_only hit) -> Some (`Program_only hit)
        | Some (Opcode_policy.Consensus_unsafe hit) -> Some (`Consensus_unsafe hit)
        | None -> None
    in
    match policy_error with
    | Some (`Program_only hit) ->
      Error (Unsafe_error (Opcode_policy.program_only_error_message hit))
    | Some (`Consensus_unsafe hit) ->
      Error (Unsafe_error (Opcode_policy.error_message hit))
    | None ->
      Ok {
        admitted_code = Array.copy code;
        admitted_effects = Program_effects.scan code;
        profile = if program then Program Program_type_flow.empty_facts else Legacy;
        requirement = None;
        provenance = Raw_code;
      }

let of_code code = admit ~program:false code

let decode raw =
  match Bytecode.decode raw with
  | Error err -> Error (Decode_error err)
  | Ok code -> of_code code

let of_program ?(facts = Program_type_flow.empty_facts) code =
  match admit ~program:true code with
  | Error error -> Error error
  | Ok admitted ->
    (match Program_type_flow.check ~facts admitted.admitted_code with
     | Ok () -> Ok { admitted with profile = Program facts }
     | Error error -> Error (Verify_error ("Program type flow: " ^ Program_type_flow.error_message error)))

let of_inference_code_with_requirement
    ?(facts = Program_type_flow.empty_facts)
    ~support
    ~requirement
    code =
  (* Inference uses the same consensus-safe admission policy as programs.
     A numerical capability is not sufficient to make host floating-point
     operations deterministic; a numerical profile must bind those semantics
     to execution before this path is widened. *)
  match Execution_requirement.check support requirement with
  | Error error ->
    Error (Unsafe_error
             ("execution requirement: "
              ^ Execution_requirement.error_message error))
  | Ok () ->
    (match Inference_opcode_policy.first_violation ~requirement code with
     | Some violation ->
       Error (Unsafe_error (Inference_opcode_policy.error_message violation))
     | None ->
       (match of_program ~facts code with
        | Error error -> Error error
        | Ok admitted -> Ok { admitted with requirement = Some requirement }))

let cert_field name fields =
  match List.filter (fun (key, _) -> String.equal key name) fields with
  | [(_, value)] -> Some value
  | _ -> None

let cert_text name fields =
  match cert_field name fields with
  | Some (`String value) -> Some value
  | _ -> None

let cert_effects fields =
  let rec read = function
    | [] -> Some []
    | `String value :: rest ->
      (match read rest with
       | Some values -> Some (value :: values)
       | None -> None)
    | _ -> None
  in
  match cert_field "effects" fields with
  | Some (`List values) -> read values
  | _ -> None

let fact_kind fields name =
  match cert_field name fields with
  | Some (`String value) -> Program_type_flow.kind_of_name value
  | _ -> None

let fact_int fields name =
  match cert_field name fields with
  | Some (`Int value) -> Some value
  | _ -> None

let fact_pairs value key =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields key, fact_kind fields "kind" with
         | Some slot, Some kind when slot >= 0 && not (List.mem slot seen) ->
           read (slot :: seen) ((slot, kind) :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read [] [] values
  | _ -> None

let fact_effects value =
  match value with
  | `List values ->
    let rec read acc = function
      | [] -> Some (List.rev acc)
      | `String value :: rest -> read (value :: acc) rest
      | _ -> None
    in
    read [] values
  | _ -> None

let fact_storage value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match cert_text "key" fields, fact_kind fields "kind" with
         | Some key, Some kind
           when key <> "" && kind <> Program_type_flow.Unknown
             && not (List.mem key seen) ->
           read (key :: seen) ((key, kind) :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read [] [] values
  | _ -> None

let fact_entries value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields "target", cert_field "memory" fields,
               cert_field "effects" fields with
         | Some target, Some memory, Some effects
           when target >= 0 && not (List.mem target seen) ->
           (match fact_pairs memory "slot", fact_effects effects with
            | Some mem, Some effects ->
              read (target :: seen)
                ({ Program_type_flow.target; mem; effects } :: acc)
                rest
            | _ -> None)
         | _ -> None)
      | _ -> None
    in
    read [] [] values
  | _ -> None

let fact_kinds value =
  match value with
  | `List values ->
    let rec read acc = function
      | [] -> Some (List.rev acc)
      | `String value :: rest ->
        (match Program_type_flow.kind_of_name value with
         | Some kind when kind <> Program_type_flow.Unknown -> read (kind :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read [] values
  | _ -> None

let fact_capabilities value =
  match value with
  | `List values ->
    let rec read acc = function
      | [] -> Some (List.rev acc)
      | `String value :: rest ->
        (match Program_type_flow.capability_of_name value with
         | Some capability -> read (capability :: acc) rest
         | None -> None)
      | _ -> None
    in
    read [] values
  | _ -> None

let fact_xcalls value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields "pc", cert_text "method" fields,
               cert_field "inputs" fields, fact_kind fields "output",
               cert_field "capabilities" fields with
         | Some pc, Some method_name, Some inputs, Some output, Some capabilities
           when pc >= 0 && method_name <> "" && not (List.mem pc seen) ->
           (match fact_kinds inputs, fact_capabilities capabilities with
            | Some inputs, Some capabilities ->
              read (pc :: seen)
                ({ Program_type_flow.pc; method_name; inputs; output; capabilities } :: acc)
                rest
            | _ -> None)
         | _ -> None)
      | _ -> None
    in
    read [] [] values
  | _ -> None

let fact_calls value =
  match value with
  | `List values ->
    let rec read seen acc = function
      | [] -> Some (List.rev acc)
      | `Assoc fields :: rest ->
        (match fact_int fields "owner", fact_int fields "pc", fact_int fields "target",
               fact_kind fields "kind" with
         | Some owner, Some pc, Some target, Some kind
           when owner >= 0 && pc >= 0 && target >= 0 && not (List.mem pc seen) ->
           read (pc :: seen)
             ({ Program_type_flow.owner; pc; target; kind } :: acc) rest
         | _ -> None)
      | _ -> None
    in
    read [] [] values
  | _ -> None

let cert_facts fields =
  match cert_field "facts" fields with
  | Some (`Assoc values) ->
    (match cert_field "root" values,
           cert_field "entries" values,
           cert_field "calls" values with
     | Some root, Some entries, Some calls ->
       (match fact_pairs root "slot", fact_entries entries, fact_calls calls with
        | Some root, Some entries, Some calls ->
          let storage =
            match cert_field "storage" values with
            | None -> Some []
            | Some value -> fact_storage value
          in
          let xcalls =
            match cert_field "xcalls" values with
            | None -> Some []
            | Some value -> fact_xcalls value
          in
          (match storage, xcalls with
           | Some storage, Some xcalls ->
             Some { Program_type_flow.root; storage; entries; calls; xcalls }
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | _ -> None

let valid_digest = function
  | Some value ->
    String.length value = 64
    && String.for_all
         (function
           | '0' .. '9' | 'a' .. 'f' -> true
           | _ -> false)
         value
  | None -> false

let valid_provenance fields =
  let compiler = cert_text "compiler" fields in
  let version = cert_text "compiler_version" fields in
  let source_mode = cert_text "source_mode" fields in
  let source_hash = cert_text "source_hash" fields in
  let verification_hash = cert_text "verification_hash" fields in
  compiler = Some "octra_aml"
  && (match version with Some value -> value <> "" | None -> false)
  && (match source_mode with
      | Some value -> value = "single" || value = "multi"
      | None -> false)
  && valid_digest source_hash
  && valid_digest verification_hash

let verify_program_cert ~attested ~trusted raw_code code raw =
  try
    match Yojson.Safe.from_string raw with
    | `Assoc fields ->
      let schema = cert_text "schema" fields in
      let declaration = cert_text "declaration" fields in
      let bytecode_hash = cert_text "bytecode_hash" fields in
      let effects = cert_effects fields in
      let facts = cert_facts fields in
      let facts_hash = cert_text "facts_hash" fields in
      let expected = Digestif.SHA256.(digest_string raw_code |> to_hex) in
      if schema <> Some "aml_bytecode_certificate_v2" then
        Error "program certificate schema mismatch"
      else if declaration <> Some "program" then
        Error "program certificate declaration mismatch"
      else if bytecode_hash <> Some expected then
        Error "program certificate bytecode mismatch"
      else if not (valid_provenance fields) then
        Error "program certificate provenance mismatch"
      else
        (match if attested then Program_attestation.verify ~trusted raw else Ok () with
         | Error error -> Error ("program compiler attestation: " ^ Program_attestation.error_message error)
         | Ok () ->
           (match facts with
            | None -> Error "program certificate facts mismatch"
            | Some facts ->
              (match effects with
               | None -> Error "program certificate effects mismatch"
               | Some effects ->
                 (match Program_policy.verify code facts effects with
                  | Error error -> Error ("program effect policy: " ^ error)
                  | Ok () ->
                    if facts_hash = Some (Program_type_flow.facts_hash facts) then Ok facts
                    else Error "program certificate facts hash mismatch"))))
    | _ -> Error "program certificate must be an object"
  with _ -> Error "invalid program certificate"

let envelope_provenance ~attested =
  if attested then Attested_envelope else Checked_envelope

let decode_program_envelope ~attested ~trusted ~admit raw =
  match Program_envelope.decode raw with
  | Error error -> Error (Decode_error (Program_envelope.error_message error))
  | Ok envelope ->
    match Bytecode.decode envelope.code with
    | Error error -> Error (Decode_error error)
    | Ok code ->
      (match verify_program_cert ~attested ~trusted envelope.code code envelope.cert with
       | Error error -> Error (Verify_error error)
       | Ok facts ->
         (match admit ~facts code with
          | Error error -> Error error
          | Ok admitted ->
            Ok {
              admitted with
              provenance = envelope_provenance ~attested;
            }))

let decode_program ?(trusted = []) raw =
  decode_program_envelope
    ~attested:true
    ~trusted
    ~admit:(fun ~facts code -> of_program ~facts code)
    raw

let decode_deploy ?(trusted = []) raw =
  if Program_envelope.is_program raw then decode_program ~trusted raw
  else decode raw

let decode_program_source raw =
  decode_program_envelope
    ~attested:false
    ~trusted:[]
    ~admit:(fun ~facts code -> of_program ~facts code)
    raw

let decode_inference_program ?(trusted = []) ~support ~requirement raw =
  decode_program_envelope
    ~attested:true
    ~trusted
    ~admit:(fun ~facts code ->
      of_inference_code_with_requirement
        ~facts
        ~support
        ~requirement
        code)
    raw

let decode_inference_program_source ~support ~requirement raw =
  decode_program_envelope
    ~attested:false
    ~trusted:[]
    ~admit:(fun ~facts code ->
      of_inference_code_with_requirement
        ~facts
        ~support
        ~requirement
        code)
    raw

let code admitted =
  Array.copy admitted.admitted_code

let effects admitted =
  admitted.admitted_effects

let profile admitted =
  admitted.profile

let requirement admitted =
  admitted.requirement

let provenance admitted =
  admitted.provenance

let provenance_name = function
  | Raw_code -> "raw_code"
  | Checked_envelope -> "checked_envelope"
  | Attested_envelope -> "attested_envelope"

let program_attested admitted =
  match admitted.provenance with
  | Attested_envelope -> true
  | Raw_code
  | Checked_envelope -> false

let error_message = function
  | Decode_error message
  | Verify_error message
  | Unsafe_error message -> message
