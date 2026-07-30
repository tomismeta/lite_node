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

module Corpus = Octra_vm.Inference_determinism_corpus

type paths = {
  mutable artifact_dir : string option;
  mutable summary : string option;
  mutable operation_mapping : string option;
  mutable fixture_corpus : string option;
  mutable failure_cases : string option;
}

let fail message =
  prerr_endline message;
  exit 1

let paths = {
  artifact_dir = None;
  summary = None;
  operation_mapping = None;
  fixture_corpus = None;
  failure_cases = None;
}

let usage =
  "inference_determinism_check --artifact-dir <dir>\n\
   or explicit --summary/--operation-mapping/--fixture-corpus/--failure-cases"

let args = [
  "--artifact-dir",
  Arg.String (fun value -> paths.artifact_dir <- Some value),
  "determinism corpus artifact directory";
  "--summary",
  Arg.String (fun value -> paths.summary <- Some value),
  "determinism qualification summary json";
  "--operation-mapping",
  Arg.String (fun value -> paths.operation_mapping <- Some value),
  "operation mapping json";
  "--fixture-corpus",
  Arg.String (fun value -> paths.fixture_corpus <- Some value),
  "fixture corpus json";
  "--failure-cases",
  Arg.String (fun value -> paths.failure_cases <- Some value),
  "failure cases json";
]

let default_from_artifact filename =
  match paths.artifact_dir with
  | None -> None
  | Some dir -> Some (Filename.concat dir filename)

let path explicit filename label =
  match explicit with
  | Some path -> path
  | None ->
    (match default_from_artifact filename with
     | Some path -> path
     | None -> fail ("missing " ^ label))

let read_json path =
  try Yojson.Safe.from_file path with
  | Sys_error message -> fail message
  | Yojson.Json_error message ->
    fail ("invalid json " ^ path ^ ": " ^ message)

let () =
  Arg.parse args (fun arg -> fail ("unexpected argument: " ^ arg)) usage;
  let summary =
    path
      paths.summary
      "determinism-qualification-summary.cjson"
      "--summary"
  in
  let operation_mapping =
    path paths.operation_mapping "operation-mapping.cjson" "--operation-mapping"
  in
  let fixture_corpus =
    path paths.fixture_corpus "fixture-corpus.cjson" "--fixture-corpus"
  in
  let failure_cases =
    path paths.failure_cases "fixtures/failure-cases.cjson" "--failure-cases"
  in
  match
    Corpus.of_json
      ~summary:(read_json summary)
      ~operation_mapping:(read_json operation_mapping)
      ~fixture_corpus:(read_json fixture_corpus)
      ~failure_cases:(read_json failure_cases)
  with
  | Error error -> fail (Corpus.error_message error)
  | Ok corpus ->
    print_endline (Yojson.Safe.pretty_to_string (Corpus.to_json corpus))
