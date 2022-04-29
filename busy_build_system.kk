module app

import std/os/file
import std/os/path
import std/os/readline
import helpers

/*
Below is the implementation of a "busy" build system. The term is used in Build Systems à la Carte by ANDREY MOKHOV, NEIL MITCHELL, SIMON PEYTON JONES
meaning that the system is not minimal: 

Definition 2.1 (Minimality). A build system is minimal if it executes tasks at most once per build
and only if they transitively depend on inputs that changed since the previous build

Ex. If A1 = A2 + A2 then the cell on A2 will be fetched twice, even though though in between fetchs the value of A2's dependencies did not change
*/

pub type cell
  Float(i: float64)
  Task(t : () -> <tasks> float64)

pub effect state
  fun get(k: string) : cell
  fun put(k: string, v: cell) : ()

pub effect tasks
  fun fetch(k : string) : float64

pub effect visuals
  fun print-spreadsheet() : ()

pub fun evaluate-task(spreadsheet : list<list<maybe<cell>>>, action: () -> <tasks> float64) :  <exn, div> float64
  with handler
    // Put is not called within evaluation
    fun put(k, v)
      ()
    fun get(k)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> throw("Requested cell is empty")
        Just(row : list<maybe<cell>>) -> match row[idx.snd]
          Nothing -> throw("Requested cell is empty")
          Just(v : maybe<cell>) -> match v
            Just(c : cell) -> c
            Nothing -> throw("Requested cell is empty")
  with handler
    fun fetch(k)
      match get(k)
        Float(i : float64) -> i
        Task(t : () -> <tasks> float64) -> evaluate-task(spreadsheet, t)
  action()

pub fun handle-app(n-rows : int, n-columns : int, action : () -> <console, state, tasks, visuals> a) : <console |_e> a
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
        Nothing -> Float(0.0)
        Just(row : list<maybe<cell>>) -> match row[idx.snd]
          Nothing -> Float(0.0)
          Just(v : maybe<cell>) -> match v
            Just(c : cell) -> c
            Nothing -> Float(0.0)
  // Task handler logic, evaluates the value of a task using evaluate-task helper method
  with handler
    fun fetch(k)
      // Need recursive fetch calls to evaluate a cell ex: A1 = A2 + A3 => fetch(A1) = fetch(A2) + fetch(A3), 
      // [SOLVED] Problem: new task affects are raised within the handler, while fetch is being handled, resulting in unhandled task affects
      // [TODO] New problem: find a way to avoid handler logic repetition
      match get(k)
        Float(i : float64) -> i
        Task(t : () -> <tasks> float64) -> evaluate-task(spreadsheet, t)
  // Visual logic
  with handler
    fun print-spreadsheet()
      print("\t")
      val columns = list('A', char(int('A') + n-columns - 1))
      columns.foreach(fn (col) print(string(col) ++ "\t"))
      println("")
      with x <- list('1', char(int('1') + n-columns - 1)).foreach
      print(x); print("\t")
      columns.foreach(fn(col) {print(fetch(string(x) ++ string(col)));print("\t")})
      println("")
  action()

fun sandbox()
  // Example "action" that implements basic functionality 
  put("A1", Float(1.0))
  put("A3", Float(2.0))
  put("C4", Float(1.0))
  println("Results:")
  val task-fun = fn() {
    if fetch("C4") == 1.0
    then fetch("A1") + fetch("A3")
    else fetch("A3")
  }
  put("A4", Task(task-fun))
  // println(fetch("A4"))
  print-spreadsheet()
  ()

private fun main()
  handle-app(4, 4, sandbox)


// Below are commented-out additional functionality

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

