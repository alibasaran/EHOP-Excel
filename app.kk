module app

import std/os/file
import std/os/path
import std/os/readline
import helpers

// Idea: for calculating dependencies run tasks under different handler

pub type cell
  Cell(t : () -> <tasks> float64)

pub fun eval(c : cell): <tasks> float64 
  match c
    Cell(t) -> t()

pub effect state
  fun get(k: string) : cell
  fun put(k: string, v: cell) : ()
  // fun print-spreadsheet() : ()

pub effect tasks
  fun fetch(k : string) : float64

val task-handler = handler
  fun fetch(k)
    // Recursive fetch calls to evaluate a cell ex: A1 = A2 + A3 => fetch(A1) = fetch(A2) + fetch(A3), 
    // Problem: new task affects are raised while the previous fetch is handled by task-handler
    // Should be: eval(get(k)) 
    0.0

pub fun handle-app(n-rows : int, n-columns : int, action : () -> <console, state, tasks> a) : <console, exn |_e> a
  // Initialize empty spreadsheet with Nothing
  var spreadsheet : list<list<maybe<cell>>> := replicate(replicate(Nothing, n-columns), n-rows)
  // State handler logic, implements put and get
  with handler
    fun put(k, v)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> () // TODO: Expand spreadsheet
        Just(l : list<maybe<cell>>) ->
          val row-to-append = replace-at-index(idx.snd, Just(v), l)
          spreadsheet := replace-at-index(idx.fst, row-to-append, spreadsheet)
    fun get(k)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> throw("Requested cell is empty")
        Just(row : list<maybe<cell>>) -> match row[idx.snd]
          Nothing -> throw("Requested cell is empty")
          Just(v : maybe<cell>) -> match v
            Just(c : cell) -> c
            Nothing -> throw("Requested cell is empty")
  with task-handler
  action()

fun load-spreadsheet()
  // Example "action" that implements basic functionality 
  put("A1", Cell(fn() 1.0))
  put("A3", Cell(fn() 1.0))
  // print-spreadsheet()
  println("Results:")
  // put("A3", Cell(fn() {fetch("A1") + fetch("A4")}))
  println(fetch("A3"))
  ()

fun main()
  handle-app(10, 10, load-spreadsheet)


// Below are commented-out additional functionality

// fun print-spreadsheet()
//   print("\t")
//   list('A', char(int('A') + n-columns - 1)).foreach(fn (col) print(string(col) ++ "\t"))
//   println("")
//   with x <- list(1, n-rows).foreach
//   print(x); print("\t")
//   match spreadsheet[x - 1]
//     Just(l : list<maybe<cell>>) -> l.foreach(fn (elem) {print(elem.eval()); print("\t")})
//     Nothing -> ()

// fun app() : <state, io> ()
//   val command-list = to-upper(readline()).split(" ")
//   println("Result:")
//   match command-list
//     // What about A1 = A2?
//     [key, "=", value] -> put(key, Float(string-to-float(value)))
//     ["PRINT"] -> print-spreadsheet()
//     [key] -> println(get(key))
//     _ -> println("Invalid input")
//   println("")
//   app()

