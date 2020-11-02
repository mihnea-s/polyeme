open Base
open Stdio


type datum = 
    Void 
    | Symbol of string
    | Boolean of bool
    | Character of char
    | Integer of int
    | Real of float
    | String of string
    | Vector of datum array
    | Hash of (datum, datum) Hashtbl.t
    | Port of port
    | Pair of datum * datum
    | Environment of environment
    | Function of (datum list -> datum)
    | Procedure of {
            params: string list;
            var_param: string option;
            body: datum list;
            closure: environment;
        }
    [@@deriving sexp]

and environment = (string, datum) Hashtbl.t

and port = InPort of In_channel.t | OutPort of Out_channel.t

and error =
    NumArgs of int * datum list
    | Default of string
    | UnboundVar of string
    [@@deriving sexp]

and result = (datum, error) Result.t

let rec pair = function
    | x::xs -> Pair (x, pair xs)
    | [] -> Symbol "()"

let rec unpair = function
    | Pair (x, y) -> x :: unpair y
    | other -> [other]

let read_env env var = 
    match Hashtbl.find env var with
    | Some v -> Ok v
    | None -> Error (UnboundVar var)

let write_env env var value =
    Hashtbl.set env ~key:var ~data:value;
    value

let mutate_env env var value =
    match Hashtbl.find env var with
    | None -> Error (UnboundVar var)
    | Some _ ->
        Hashtbl.set env ~key:var ~data:value;
        Ok value

let extend_env env bindings =
    List.map bindings ~f:(fun (key, data) -> Hashtbl.set env ~key ~data)
