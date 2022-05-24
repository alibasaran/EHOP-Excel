module tokenizer
import std/text/regex
import std/num/decimal
import helpers

// Regex used to extract certain strings from user command input
val func-regex = regex("[A-Z]+(?=\\(.+\\))")
val num-regex = regex("^[0-9]+\\.?[0-9]*$")
val key-regex = regex("^[A-Z]+[0-9]+$")

pub type token
  Key(k : string)
  Number(n : decimal)
  Func(func-name : string, params: list<token>)

/*
  Given a command string candidate s returns true if the string is a function
  Ex. "SUM(A1, A2)" => True, "A1" => False
*/
pub fun is-func(s : string) : bool
  s.contains(func-regex)

/*
  Given a key string candidate s returns true if the string is a key
  Ex. "SUM(A1, A2)" => False, "A1" => True, "1A" => False
*/
pub fun is-key(s : string) : bool
  s.contains(key-regex)

/*
  Similar to is-key, but throws an exception when the input string is not a key
*/
pub fun check-key(s : string) : exn ()
  if !s.is-key then throw("String " ++ s ++ " is not a key!")

/*
  Given a key string k returns true if the string is a key
  Ex. "SUM(A1, A2)" => False, "-1" => True, "1.1" => True
*/
pub fun is-number(s : string) : bool
  s.contains(num-regex)

/*
  Given an user command input string, returns a list of tokens
  Ex. "1.1" => Number(1.1), "A1" => Key("A1"), SUM(A1, B1) => Func(name="SUM", params=[A1, B1])
*/
pub fun tokenize-string(input : string) : <div, console, exn> list<token>
  input.split(" ").filter(fn(elem) elem != "").map(fn(elem) {
    if elem.is-number
    then Number(elem.to-decimal)
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