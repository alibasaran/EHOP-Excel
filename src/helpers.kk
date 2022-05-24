module helpers

import std/text/regex
import std/text/parse
import std/num/decimal

/*
Helper functions
*/

/*
  Given an index idx, value and a list replaces the element at idx with value and returns the new list
*/
pub fun replace-at-index(idx : int, value : a, l : list<a>) : _e list<a>
  val list-split = l.split(idx)
  val tail = list-split.snd.drop(1)
  list-split.fst ++ [value] ++ tail

/*
  Given a decimal string converts it into a deciimal
  Ex. "11" => 11.0 or "11.1" => 11.1
*/
pub fun to-decimal(s : string) : _e decimal
  val sign = if s.starts-with("-").is-just then decimal(-1.0) else decimal(1.0)
  val sum-digits = list(s).map(int).filter(fn(x) x <= 57 && x >= 48).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y).decimal
  val dot-index = s.list.index-of(fn(c) c == '.')
  if dot-index == -1
  then sign * sum-digits
  else sign * sum-digits / decimal(10^(s.count - dot-index - 1))

/*
  Given an integer string, converts it into a integer
*/
pub fun to-int(s : string) : _e int
  val sign = if s.starts-with("-").is-just then -1 else 1
  val sum-digits = list(s).map(int).filter(fn(x) x <= 57 && x >= 48).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y)
  sign * sum-digits

/*
  Given a key separates it into a tuplo of (columns row)
*/
pub fun separate-key(k : string) : _e (string, int)
  val key-tuple : (string, string) = k.list.foldl(("",""), fn(tup : (string, string), elem)
    if elem.is-alpha then (tup.fst ++ elem.string, tup.snd)
    else if elem.is-alpha-num then (tup.fst, tup.snd ++ elem.string)
    else throw("Key contains unexpected element: " ++ elem.show)
  )
  (key-tuple.fst, key-tuple.snd.to-int)

/*
  Converts an integer to an excel like key
  Ex. 1 => A, 26 => Z, 27 => AA 
*/
pub fun to-key(i : int) : _e string
  var current := i
  var res := []
  while {current >= 1}
    res := [(65 + (current - 1) % 26).char] ++ res
    current := (current - 1) / 26
  res.string

/*
  Convert a column (Ex. A, Z, AA) into an integer
*/
pub fun col-to-int(col-str : string) : _e int
  list(col-str).map(int).filter(fn(x) x > 64).map(fn(x) x - 64).foldl(0, fn(x, y) 26 * x + y)

/*
  Given two column strings return all columns between them
  Ex. list-from-keys("X", "AA") => ["X", "Y", "Z", "AA"]
*/
pub fun list-from-keys(from  : string, to : string) : _e list<string>
  val from-int = from.col-to-int
  val to-int = to.col-to-int
  list(from-int, to-int).map(to-key)

/*
  Replaces top level comma with a space
  Ex. "SUM(A1, A2),SUM(A1, A2)" => "SUM(A1,A2) SUM(A1,A2)"
*/
pub fun replace-top-level-comma-with-space(s : string) : _e string
  s.split(regex(",(?![^(]*\\))")).foldl("", 
    fn (b, a)
      if a.count("(") > a.count(")")
      then b ++ a ++ ","
      else b ++ a ++ " "
  )

/*
  Given a key as input, converts it into a coordinate
  Ex. A1 => (0, 0) or D2 => (3, 2)
  UNUSED IN ITERATION 1
*/
pub fun calculate-index(k: string): _e (int, int)
  val key-encoded = list(k).map(int)
  val row = key-encoded.filter(fn(x) x <= 57 && x >= 49).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y)
  val column = key-encoded.filter(fn(x) x > 64).map(fn(x) x - 64).foldl(0, fn(x, y) 25 * x + y)
  (row - 1, column - 1)