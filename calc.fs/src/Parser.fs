module Calc.Parser

// #r @"../packages/FParsec/lib/net40-client/FParsecCS.dll";;
// #r @"../packages/FParsec/lib/net40-client/FParsec.dll";;
open FParsec

type Exp =
  | Number of float
  | Var of string
  | Assign of string * Exp
  | Pair of Exp
  | Plus of Exp * Exp
  | Minus of Exp * Exp
  | Times of Exp * Exp
  | Div of Exp * Exp
  | Neg of Exp
  | Function of string * Exp

type ParserU<'a> = Parser<'a, unit>

//
let pLeftParen : ParserU<string> = pstring "(" .>> spaces
let pRightParen : ParserU<string> = spaces >>. pstring ")"
let betweenParens : ParserU<Exp> -> ParserU<Exp> = between pLeftParen pRightParen

let pIdentifier : ParserU<string> =
  let firstChar c = isLetter c || c = '_'
  let restChar c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L firstChar restChar "identifier" .>> spaces

//
let opp = new OperatorPrecedenceParser<_, _, _>()
let pExp = opp.ExpressionParser

let pNumber : ParserU<Exp> = pfloat |>> Number
let pVar : ParserU<Exp> = pIdentifier |>> Var

let pAssign : ParserU<Exp> =
  pipe3 pIdentifier (pstring "=" .>> spaces) pExp (fun var _ exp -> Assign(var, exp))

let pPair : ParserU<Exp> = betweenParens pExp

let pFunction : ParserU<Exp> =
  pipe4 pIdentifier (pstring "(" .>> spaces) pExp (spaces >>. pstring ")")
    (fun var _ exp _ -> Function(var, exp))

let pTerm = pPair <|> pNumber <|> attempt pAssign <|> attempt pFunction <|> pVar

opp.TermParser <- spaces >>. pTerm .>> spaces
opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> Plus(x, y)))
opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> Minus(x, y)))
opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> Times(x, y)))
opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> Div(x, y)))
opp.AddOperator(PrefixOperator("-", spaces, 2, true, fun x -> Neg(x)))

//
let private test p str =
  match run p str with
  | Success(result, _, _) -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
