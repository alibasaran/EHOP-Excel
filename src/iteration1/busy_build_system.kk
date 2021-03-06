module busy_build_system

import std/os/file
import std/os/path
import std/os/readline
import helpers
import tokenizer
import interpreter

/*
Below is the implementation of a "busy" build system. The term is used in Build Systems à la Carte by ANDREY MOKHOV, NEIL MITCHELL, SIMON PEYTON JONES
meaning that the system is not minimal: 

Definition 2.1 (Minimality). A build system is minimal if it executes tasks at most once per build
and only if they transitively depend on inputs that changed since the previous build

Ex. If A1 = A2 + A2 then the cell on A2 will be fetched twice, even though though in between fetchs the value of A2's dependencies did not change
*/

pub struct cell
  cell-desc : string = ""
  value : cell-val = Float(0.0)

pub effect state
  // Gets cell at key
  fun get(key: string) : cell
  // Puts value into key
  fun put(key: string, value: cell) : ()

pub fun spreadsheet-handler(n-rows : int, n-columns : int, action : () -> <state,console,div|e> a) : <div,console|e> a
  // Initialize empty spreadsheet with Nothing
  var spreadsheet : list<list<cell>> := replicate(replicate(Cell(), n-columns), n-rows)
  // State handler logic, implements put and get
  with handler
    fun put(k, v)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> println(k ++ "is out of the spreadsheet boundary")
        Just(l : list<cell>) ->
          val row-to-append = replace-at-index(idx.snd, v, l)
          spreadsheet := replace-at-index(idx.fst, row-to-append, spreadsheet)
    fun get(k)
      val idx = calculate-index(k)
      match spreadsheet[idx.fst]
        Nothing -> Cell()
        Just(row : list<cell>) -> match row[idx.snd]
          Nothing -> Cell()
          Just(v : cell) -> v
  action()

/*
Cell evaluation logic
*/

pub effect tasks
  // Given a key return the value of the keys cell
  fun fetch(k : string) : float64

fun task-handler(action : () -> <state,tasks,exn,console,div|e> a) : <state,console,div,exn|e> a
  with handler
    fun fetch(k) get(k).value.evaluate-cell-val
  action()

pub fun evaluate-cell-val(cv : cell-val) :  <exn, div, console, state> float64
  // IMPORTANT: A helper function is needed because recursive calls to the same function cannot be made inside handlers
  // Helper for the fetch-function, evaluates a cell-val into float
  with task-handler()
  match cv
    Float(i : float64) -> i
    KeyV(s : string) -> fetch(s)
    SumV(params : list<cell-val>) -> params.foldl(0.0, fn(total, v) total + v.evaluate-cell-val)
    MulV(params : list<cell-val>) -> params.foldl(1.0, fn(total, v) total * v.evaluate-cell-val)
    DivV(params : list<cell-val>) -> 
      val first-val = params[0].unjust.evaluate-cell-val
      params.tail.foldl(first-val, fn(total, v) total / v.evaluate-cell-val)
    MinusV(params : list<cell-val>) -> 
      val first-val = params[0].unjust.evaluate-cell-val
      params.tail.foldl(first-val, fn(total, v) total - v.evaluate-cell-val)

/*
  Spreadsheet or cell visualization effect
*/
pub effect visuals
  // Print the full spreadsheet
  fun print-spreadsheet() : ()
  // Print the description of a cell
  fun print-cell-desc(k : string) : ()

pub fun visuals-handler(n-rows : int, n-columns : int, action : () -> <visuals,state,tasks,console|e> a) : <state,tasks,console|e> a
  with handler
    // Prints the cell description
    fun print-cell-desc(k)
      get(k).cell-desc.println()
    // Prints the full spreadsheet
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

pub fun handle-app(n-rows : int, n-columns : int, action : () -> <state,tasks,visuals|_e> a) : _e a
  /*
    Handle application operations by combining all of the handlers defined above 
  */
  with spreadsheet-handler(n-rows, n-columns)
  with task-handler()
  with visuals-handler(n-rows, n-columns)
  action()

pub fun handle-command(command : string) : <state,tasks,visuals|_e> ()
  // Seperate command handling to a separate function for simpler test code
  match command.split(" ")
    // When assigning a new value to a key
    [key, "=", value] -> {
      // Check if key is valid.
      if key.is-key
      then put(key, Cell(value, tokenize-string(value)[0].unjust.interpret-token))
      else println("Invalid key format: " ++ key)
    }
    ["PRINT"] -> print-spreadsheet()
    [key] -> if key.is-key then println(fetch(key)) else println("Invalid key format: " ++ key)
    ["DESC", key] -> print-cell-desc(key)
    _ -> println("Invalid input")

fun app() : <state,tasks,visuals|_e> ()
  print("<App Name>$ ")
  readline().to-upper().handle-command
  println("")
  app()

fun main()
  handle-app(9, 9, app)
  