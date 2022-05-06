module tokenizer
import std/text/regex
import helpers

// Regex used to extract certain strings from user command input

val func-regex = regex("[A-Z]+(?=\\(.+\\))")
val num-regex = regex("^[0-9]+\\.?[0-9]*$")
val key-regex = regex("^[A-Z]+[0-9]+$")

pub type token
  Key(k : string)
  Number(n : float64)
  Func(func-name : string, params: list<token>)

// effect tokenize
//   fun is-func(s : string) : bool
//   fun is-key(s : string) : bool
//   fun is-number(s : string) : bool
//   fun tokenize-string(s : string) : list<token>

pub fun is-func(s : string) : bool
  /*
    Given a command string candidate s returns true if the string is a function
    Ex. "SUM(A1, A2)" => True, "A1" => False
  */
  s.contains(func-regex)

pub fun is-key(s : string) : bool
  /*
    Given a key string candidate s returns true if the string is a key
    Ex. "SUM(A1, A2)" => False, "A1" => True, "1A" => False
  */
  s.contains(key-regex)

pub fun is-number(s : string) : bool
  /*
    Given a key string k returns true if the string is a key
    Ex. "SUM(A1, A2)" => False, "-1" => True, "1.1" => True
  */
  s.contains(num-regex)

pub fun tokenize-string(input : string) : <div, console, exn> list<token>
  /*
    Given an user command input string, returns a list of tokens
    Ex. "1.1" => Number(1.1), "A1" => Key("A1"), SUM(A1, B1) => Func(name="SUM", params=[A1, B1])
  */
  input.split(" ").filter(fn(elem) elem != "").map(fn(elem) {
    if elem.is-number
    then Number(string-to-float(elem))
    else if elem.is-key
    then Key(elem)
    else if elem.is-func
    then 
      // Extract function name from string
      val func-name = elem.find(func-regex).unjust
      // Extract function arguments as " " separated strings. Ex, "SUM(A1, A2)" => "A1 A2"
      val func-args-str = elem.starts-with(func-name).unjust.string.find(regex("(?<=\\()(.*)(?=\\))")).unjust.replace-top-level-comma-with-space
      Func(func-name, tokenize-string(func-args-str))
    else throw("Invalid syntax!")
})

// Below are token printing functionality used in development

// pub fun print(t: token) : <console, div> ()
//   match t
//     Key(k : string) -> println("Key(" ++ k ++ ")")
//     Number(n : float64) -> {print("Number(");print(n);print(")");println("")}
//     Func(func-name : string, params : list<token>) -> 
//       println("Func(name=" ++ func-name ++ ")")
//       print-input-list(params)

// pub fun print-input-list(ts : list<token>) : <div, console> ()
//   with t <- ts.foreach
//   t.print()