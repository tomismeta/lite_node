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


type range = {
  owner_root : string;
  offset : int;
  length : int;
  encoding : string;
  shape_root : string option;
}

type t = {
  model_root : string;
  store_root : string;
  ranges : range list;
}

type error =
  | Bad_root of string
  | Bad_name of string
  | Bad_range of string * int
  | Range_overflow of int * int
  | Empty_ranges
  | Duplicate_range of string
  | Model_root_mismatch of string * string
  | Store_root_mismatch of string * string

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

let range_json range =
  `Assoc [
    "encoding", `String range.encoding;
    "length", `Int range.length;
    "offset", `Int range.offset;
    "owner_root", `String range.owner_root;
    "shape_root",
    (match range.shape_root with
     | None -> `Null
     | Some root -> `String root);
  ]

let range_root range =
  let payload = Yojson.Safe.to_string (range_json range) in
  Digestif.SHA256.(
    digest_string ("octra:inference:model-range\000" ^ payload) |> to_hex)

let range_order left right =
  String.compare (range_root left) (range_root right)

let sort_ranges ranges =
  List.sort range_order ranges

let to_json model =
  `Assoc [
    "model_root", `String model.model_root;
    "store_root", `String model.store_root;
    "ranges", `List (List.map range_json (sort_ranges model.ranges));
  ]

let root model =
  let payload = Yojson.Safe.to_string (to_json model) in
  Digestif.SHA256.(
    digest_string ("octra:inference:model-ranges\000" ^ payload) |> to_hex)

let check_positive name value =
  if value > 0 then Ok () else Error (Bad_range (name, value))

let check_offset value =
  if value >= 0 then Ok () else Error (Bad_range ("offset", value))

let check_range_bounds range =
  match check_offset range.offset with
  | Error error -> Error error
  | Ok () ->
    (match check_positive "length" range.length with
     | Error error -> Error error
     | Ok () ->
       if range.offset > max_int - range.length then
         Error (Range_overflow (range.offset, range.length))
       else
         Ok ())

let check_shape_root = function
  | None -> Ok ()
  | Some root -> check_root root

let validate_range range =
  match check_root range.owner_root with
  | Error error -> Error error
  | Ok () ->
    (match check_name range.encoding with
     | Error error -> Error error
     | Ok () ->
       (match check_shape_root range.shape_root with
        | Error error -> Error error
        | Ok () -> check_range_bounds range))

let rec check_ranges seen = function
  | [] -> Ok ()
  | range :: rest ->
    (match validate_range range with
     | Error error -> Error error
     | Ok () ->
       let key = range_root range in
       if List.mem key seen then Error (Duplicate_range key)
       else check_ranges (key :: seen) rest)

let validate model =
  match check_root model.model_root with
  | Error error -> Error error
  | Ok () ->
    (match check_root model.store_root with
     | Error error -> Error error
     | Ok () ->
       match model.ranges with
       | [] -> Error Empty_ranges
       | ranges -> check_ranges [] ranges)

let check ~target model =
  match validate model with
  | Error error -> Error error
  | Ok () ->
    if not (String.equal model.model_root target.Inference_target.model_root) then
      Error (Model_root_mismatch
               (model.model_root, target.Inference_target.model_root))
    else if not
        (String.equal model.store_root target.Inference_target.store_root)
    then
      Error (Store_root_mismatch
               (model.store_root, target.Inference_target.store_root))
    else
      Ok ()

let error_message = function
  | Bad_root root -> Printf.sprintf "invalid root: %s" root
  | Bad_name name -> Printf.sprintf "invalid range name: %s" name
  | Bad_range (name, value) ->
    Printf.sprintf "invalid range %s: %d" name value
  | Range_overflow (offset, length) ->
    Printf.sprintf
      "range overflows offset %d length %d"
      offset length
  | Empty_ranges -> "model range set is empty"
  | Duplicate_range root -> Printf.sprintf "duplicate range: %s" root
  | Model_root_mismatch (expected, actual) ->
    Printf.sprintf
      "model root mismatch: expected %s actual %s"
      expected actual
  | Store_root_mismatch (expected, actual) ->
    Printf.sprintf
      "store root mismatch: expected %s actual %s"
      expected actual
