module interpreter
import tokenizer
import std/num/decimal

pub type task
  Decimal(i : decimal)
  KeyV(s : string)
  SumV(params : list<task>)
  MulV(params : list<task>)
  MinusV(params : list<task>)
  DivV(params : list<task>)

pub fun interpret-token(t : token) : <exn,div> task
  match t
    Key(k: string) -> KeyV(k)
    Number(n: decimal) -> Decimal(n)
    Func(func-name: string, params : list<token>) -> 
      val params_as_values = params.map(interpret-token)
      match func-name
        "SUM" -> SumV(params_as_values)
        "MINUS" -> MinusV(params_as_values)
        "MUL" -> MulV(params_as_values)
        "DIV" -> DivV(params_as_values)
        _ -> throw("Not yet implemented or function non existant: " ++ func-name)
