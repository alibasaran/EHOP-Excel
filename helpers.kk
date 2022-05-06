module helpers

import std/text/regex

/*
Helper functions
*/

pub fun replace-at-index(idx : int, value : a, l : list<a>) : _e list<a>
  // Given an index idx, value and a list replaces the element at idx with value and returns the new list
  val list-split = l.split(idx)
  val tail = list-split.snd.drop(1)
  list-split.fst ++ [value] ++ tail

pub fun string-to-float(s : string) : _e float64
  // Given an integer string converts it into a float
  // Ex. "11" => 11.0
  val multiplier = if s.starts-with("-").is-just then -1.0 else 1.0
  val sum-digits = list(s).map(int).filter(fn(x) x <= 57 && x >= 48).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y).float64
  val dot-index = s.list.index-of(fn(c) c == '.')
  if dot-index == -1
  then multiplier * sum-digits
  else multiplier * sum-digits / float64(10^(s.count - dot-index - 1))

pub fun calculate-index(k: string): _e (int, int)
  // Given a key as input, converts it into a coordinate
  // Ex. A1 => (0, 0) or D2 => (3, 2)
  val key-encoded = list(k).map(int)
  val row = key-encoded.filter(fn(x) x <= 57 && x >= 49).map(fn(x) x - 48).foldl(0, fn(x, y) 10 * x + y)
  val column = key-encoded.filter(fn(x) x > 64).map(fn(x) x - 64).foldl(0, fn(x, y) 25 * x + y)
  (row - 1, column - 1)

pub fun replace-top-level-comma-with-space(s : string) : _e string
  /*
  Replaces top level comma with a space
  Ex. "SUM(A1, A2),SUM(A1, A2)" => "SUM(A1,A2) SUM(A1,A2)"
  */
  s.split(regex(",(?![^(]*\\))")).foldl("", 
    fn (b, a)
      if a.count("(") > a.count(")")
      then b ++ a ++ ","
      else b ++ a ++ " "
  )