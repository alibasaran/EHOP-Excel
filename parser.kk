module parser
import std/text/regex
import helpers

val func-regex = regex("[A-Z]*(?=\\(.*\\))")
val num-regex = regex("^[0-9]*\\.?[0-9]*$")
val key-regex = regex("^[A-Z]+[0-9]+$")

pub fun is-func(f : string) : <console> bool
  f.contains(func-regex)

pub fun is-key(k : string) : <console> bool
  if contains(k, key-regex)
  then True
  else False

pub fun is-number(s : string) : _e bool
  s.contains(num-regex)

pub type token
  Key(k : string)
  Number(n : float64)
  Func(func-name : string, params: list<token>)

pub fun print(t: token) : <console> ()
  match t
    Key(k : string) -> println("Key(" ++ k ++ ")")
    Number(n : float64) -> {println("Number(");print(n);print(")")}
    Func(func-name : string, _) -> println("Func(name=" ++ func-name ++ ")")


pub fun print-input-list(ts : list<token>) : <div, console> ()
  with t <- ts.foreach
  t.print()

pub fun parse-input(input : string) : <div, console, exn> list<token>
  input.split(" ").map(fn(elem) {
    if elem.is-number
    then Number(string-to-float(elem))
    else if elem.is-key
    then Key(elem)
    else if elem.is-func
    then 
      val func-name = input.find(func-regex).unjust
      val func-params-str = input.replace-all(func-name, "").replace-all("(", "").replace-all(")", "").replace-all(",", " ")
      Func(func-name, parse-input(func-params-str))
    else throw("Invalid syntax!")
})
