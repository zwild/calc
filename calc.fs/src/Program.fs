module Calc.App

open System
open System.Collections.Generic
open Calc.Parser

type Env = Dictionary<string, float>

let opResult op r1 r2 =
  match r1, r2 with
  | (Ok(n1), Ok(n2)) -> Ok((op) n1 n2)
  | (Error err, _) -> Error err
  | (Ok(_), err) -> err

let applyResult funcResult nResult =
  match (funcResult, nResult) with
  | (Ok(func), Ok(n)) -> Ok(func n)
  | (Error err, _) -> Error err
  | (Ok(_), err) -> err

let env = Dictionary<string, float>()
let funcEnv = Dictionary<string, float -> float>()

let addFuncToEnv() =
  [ ("sin", sin)
    ("cos", cos)
    ("tan", tan)
    ("asin", asin)
    ("acos", acos)
    ("atan", atan)
    ("log", log)
    ("exp", exp)
    ("sqrt", sqrt) ]
  |> List.map (fun (k, v) -> funcEnv.Add(k, v))
  |> ignore

let findInEnv (env : Dictionary<_, _>) s errMsg =
  try
    Ok(env.[s])
  with _ -> Error(errMsg)

let rec eval env = function
  | Number n -> Ok(n)
  | Var s -> findInEnv env s (sprintf "Variable not found: %s" s)
  | Assign(var, exp) ->
    let res = eval env exp
    match res with
    | Ok n ->
      if not (env.TryAdd(var, n)) then
        env.Remove(var) |> ignore
        env.Add(var, n)
      res
    | _ -> res
  | Pair exp -> eval env exp
  | Plus(e1, e2) -> opResult (+) (eval env e1) (eval env e2)
  | Minus(e1, e2) -> opResult (-) (eval env e1) (eval env e2)
  | Times(e1, e2) -> opResult (*) (eval env e1) (eval env e2)
  | Div(e1, e2) -> opResult (/) (eval env e1) (eval env e2)
  | Neg exp ->
    let res = eval env exp
    match res with
    | Ok n -> Ok(-n)
    | _ -> res
  | Function(var, exp) ->
    let res = eval env exp
    let f = findInEnv funcEnv var (sprintf "Function not found: %s" var)
    applyResult f res

module Run =
  open FParsec

  let rec loop() =
    printf "> "
    let input = Console.ReadLine()
    match run pExp input with
    | Success(p, _, _) ->
      match (eval env p) with
      | Result.Ok(n) -> printfn "%f" n
      | Result.Error msg -> printfn "Error: %s" msg
      loop()
    | Failure(msg, _, _) -> printfn "ParsingError: %s" msg

[<EntryPoint>]
let main argv =
  addFuncToEnv()
  printfn "A calc from F#!"
  Run.loop()
  0 // return an integer exit code
