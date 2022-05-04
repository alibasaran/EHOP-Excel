module interpreter
import tokenizer

pub type cell-val
  Float(i : float64)
  KeyV(s : string)
  SumV(params : list<cell-val>)
  MulV(params : list<cell-val>)
  MinusV(params : list<cell-val>)
  DivV(params : list<cell-val>)

pub fun interpret-token(t : token) : <exn,div> cell-val
  match t
    Key(k: string) -> KeyV(k)
    Number(n: float64) -> Float(n)
    Func(func-name: string, params : list<token>) -> 
      val params_as_values = params.map(interpret-token)
      match func-name
        "SUM" -> SumV(params_as_values)
        "MINUS" -> MinusV(params_as_values)
        "MUL" -> MulV(params_as_values)
        "DIV" -> DivV(params_as_values)
        _ -> throw("Not yet implemented or function non existant: " ++ func-name)
