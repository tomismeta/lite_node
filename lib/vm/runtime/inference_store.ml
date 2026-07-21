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


type pinned_range = {
  range_root : string;
  owner_root : string;
  offset : int;
  length : int;
  encoding : string;
  shape_root : string option;
  bytes : string;
}

type pin_set = {
  model_root : string;
  store_root : string;
  model_ranges_root : string;
  ranges : pinned_range list;
}

type error =
  | Missing_owner of string
  | Owner_root_mismatch of string * string
  | Range_out_of_bounds of string * int * int * int
  | Range_overflow of int * int
  | Model_limit_exceeded of int * int

let sha256 raw =
  Digestif.SHA256.(digest_string raw |> to_hex)

let pin_range ~read range =
  match read range.Inference_model.owner_root with
  | None -> Error (Missing_owner range.owner_root)
  | Some owner ->
    let actual_owner_root = sha256 owner in
    if not (String.equal actual_owner_root range.owner_root) then
      Error (Owner_root_mismatch (range.owner_root, actual_owner_root))
    else if range.offset > max_int - range.length then
      Error (Range_overflow (range.offset, range.length))
    else
      let owner_len = String.length owner in
      let stop = range.offset + range.length in
      if stop > owner_len then
        Error (Range_out_of_bounds
                 (range.owner_root, range.offset, range.length, owner_len))
      else
        Ok {
          range_root = Inference_model.range_root range;
          owner_root = range.owner_root;
          offset = range.offset;
          length = range.length;
          encoding = range.encoding;
          shape_root = range.shape_root;
          bytes = String.sub owner range.offset range.length;
        }

let rec pin_ranges ~read acc total = function
  | [] -> Ok (List.rev acc, total)
  | range :: rest ->
    (match pin_range ~read range with
     | Error error -> Error error
     | Ok pinned ->
       if total > max_int - pinned.length then
         Error (Range_overflow (total, pinned.length))
       else
         let next_total = total + pinned.length in
         pin_ranges ~read (pinned :: acc) next_total rest)

let rec preflight_total limit total (ranges : Inference_model.range list) =
  match ranges with
  | [] -> Ok total
  | (range : Inference_model.range) :: rest ->
    if range.offset < 0 || range.length < 0 then
      Error (Range_out_of_bounds
               (range.owner_root, range.offset, range.length, 0))
    else if range.offset > max_int - range.length then
      Error (Range_overflow (range.offset, range.length))
    else if total > max_int - range.length then
      Error (Range_overflow (total, range.length))
    else
      let next_total = total + range.length in
      if next_total > limit then
        Error (Model_limit_exceeded (next_total, limit))
      else
        preflight_total limit next_total rest

let pin ~limits ~read model =
  match
    preflight_total
      limits.Execution_requirement.max_model_bytes
      0
      model.Inference_model.ranges
  with
  | Error error -> Error error
  | Ok _ ->
    (match pin_ranges ~read [] 0 model.Inference_model.ranges with
     | Error error -> Error error
     | Ok (ranges, _) ->
       Ok {
         model_root = model.model_root;
         store_root = model.store_root;
         model_ranges_root = Inference_model.root model;
         ranges;
       })

let model_root pins = pins.model_root
let store_root pins = pins.store_root
let model_ranges_root pins = pins.model_ranges_root
let ranges pins = pins.ranges

let error_message = function
  | Missing_owner root -> Printf.sprintf "missing range owner: %s" root
  | Owner_root_mismatch (expected, actual) ->
    Printf.sprintf
      "range owner root mismatch: expected %s actual %s"
      expected actual
  | Range_out_of_bounds (root, offset, length, available) ->
    Printf.sprintf
      "range out of bounds: owner %s offset %d length %d available %d"
      root offset length available
  | Range_overflow (offset, length) ->
    Printf.sprintf
      "range overflows offset %d length %d"
      offset length
  | Model_limit_exceeded (required, available) ->
    Printf.sprintf
      "model byte limit exceeded: required %d available %d"
      required available
