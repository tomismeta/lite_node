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
  mutable differential_summary : string option;
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
  differential_summary = None;
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
  "determinism summary json";
  "--operation-mapping",
  Arg.String (fun value -> paths.operation_mapping <- Some value),
  "operation mapping json";
  "--fixture-corpus",
  Arg.String (fun value -> paths.fixture_corpus <- Some value),
  "fixture corpus or ingestion fixture pack json";
  "--failure-cases",
  Arg.String (fun value -> paths.failure_cases <- Some value),
  "failure cases json";
  "--differential-summary",
  Arg.String (fun value -> paths.differential_summary <- Some value),
  "optional differential summary json";
]

let artifact_path filename =
  match paths.artifact_dir with
  | None -> None
  | Some dir ->
    let path = Filename.concat dir filename in
    if Sys.file_exists path then Some path else None

let first_existing explicit candidates label =
  match explicit with
  | Some path -> path
  | None ->
    (match List.find_map artifact_path candidates with
     | Some path -> path
     | None -> fail ("missing " ^ label))

let optional_existing explicit candidates =
  match explicit with
  | Some path -> Some path
  | None -> List.find_map artifact_path candidates

let read_json path =
  try Yojson.Safe.from_file path with
  | Sys_error message -> fail message
  | Yojson.Json_error message ->
    fail ("invalid json " ^ path ^ ": " ^ message)

let optional_json = function
  | None -> None
  | Some path -> Some (read_json path)

let () =
  Arg.parse args (fun arg -> fail ("unexpected argument: " ^ arg)) usage;
  let summary =
    first_existing
      paths.summary
      [
        "determinism-ingestion-summary.cjson";
        "determinism-qualification-summary.cjson";
      ]
      "--summary"
  in
  let operation_mapping =
    optional_existing
      paths.operation_mapping
      ["operation-mapping.cjson"]
  in
  let fixture_corpus =
    first_existing
      paths.fixture_corpus
      ["p0-ingestion-fixture-pack.cjson"; "fixture-corpus.cjson"]
      "--fixture-corpus"
  in
  let failure_cases =
    first_existing
      paths.failure_cases
      ["p0-failure-atomicity-cases.cjson"; "fixtures/failure-cases.cjson"]
      "--failure-cases"
  in
  let differential_summary =
    optional_existing
      paths.differential_summary
      ["differential/differential-summary.cjson"]
  in
  match
    Corpus.of_json
      ?operation_mapping:(optional_json operation_mapping)
      ?differential_summary:(optional_json differential_summary)
      ~summary:(read_json summary)
      ~fixture_corpus:(read_json fixture_corpus)
      ~failure_cases:(read_json failure_cases)
      ()
  with
  | Error error -> fail (Corpus.error_message error)
  | Ok corpus ->
    print_endline (Yojson.Safe.pretty_to_string (Corpus.to_json corpus))
