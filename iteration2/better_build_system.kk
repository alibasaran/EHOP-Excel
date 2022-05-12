module better_build_system

import std/os/file
import std/os/path
import std/os/readline
import helpers
import tokenizer
import interpreter
import bucketed_hash_table

/*
Below is the implementation of a "busy" build system. The term is used in Build Systems à la Carte by ANDREY MOKHOV, NEIL MITCHELL, SIMON PEYTON JONES
meaning that the system is not minimal: 

Definition 2.1 (Minimality). A build system is minimal if it executes tasks at most once per build
and only if they transitively depend on inputs that changed since the previous build

Ex. If A1 = A2 + A2 then the cell on A2 will be fetched twice, even though though in between fetchs the value of A2's dependencies did not change
*/

/*
  Spreadsheet logic, implemented as a bucketed hash table.
*/

pub struct cell
  cell-desc : string = ""
  value : cell-val = Float(0.0)

pub effect state
  fun set-key(k : string, v : cell) : () 
  fun get(k : string) : cell

pub fun spreadsheet-handler(action : () -> <state|_e> a) : _e a
  var sprsh := hashtable()
  with handler
    fun set-key(k, v)
      sprsh := sprsh.set(k, v)
    fun get(k)
      match sprsh.get(k)
        Nothing -> Cell()
        Just(c : cell) -> c
  action()

/*
Cell evaluation logic
*/
pub effect tasks
  // Given a key return the value of the keys cell
  fun fetch(k : string) : float64

fun task-handler(action : () -> <state,tasks,exn,console,div|e> a) : <state,console,div,exn|e> a
  with handler
    fun fetch(k)
      get(k).value.fetch-cell-val
  action()

pub fun fetch-cell-val(cv : cell-val) :  <exn, div, console, state> float64
  match cv
    Float(i : float64) -> i 
    KeyV(s : string) -> get(s).value.fetch-cell-val
    SumV(params : list<cell-val>) -> params.foldl(0.0, fn(total, v) total + v.fetch-cell-val)
    MulV(params : list<cell-val>) -> params.foldl(1.0, fn(total, v) total * v.fetch-cell-val)
    DivV(params : list<cell-val>) -> 
      val first-val = params[0].unjust.fetch-cell-val
      params.tail.foldl(first-val, fn(total, v) total / v.fetch-cell-val)
    MinusV(params : list<cell-val>) -> 
      val first-val = params[0].unjust.fetch-cell-val
      params.tail.foldl(first-val, fn(total, v) total - v.fetch-cell-val)

/*
  Cell rebuilding logic
  Follows dirty bit rebuilder from Build Systems a La Carte
*/
pub effect dirty-bit
  fun is-dirty(k : string) : bool
  fun clean(k : string) : ()
  fun put(k : string, v : cell) : ()

pub fun dirty-bit-handler(action : () -> <dirty-bit,state,tasks,console,div,exn|e> a) : <console,div,exn,state,tasks|e> a
  // A map that maps a key to its dependents.
  // Ex. A1 = SUM(1, A2) => dep-map "A2" = [A1]
  var dep-map : hashtable<list<string>> := hashtable()
  // A map that given a key returns true if dirty false otherwise
  var is-dirty-map : hashtable<bool> := hashtable()
  with handler
    fun put(k, v)
      v.value.calculate-dependencies.foreach(fn (dep)
        match dep-map.get(dep)
          // If initial entry of dep
          Nothing -> dep-map := dep-map.set(dep, [k])
          // If dep already in dep-map
          Just(deps : list<string>) -> if deps.index-of(fn(e) e == k) == -1 then dep-map := dep-map.set(dep, deps ++ [k])
      )
      // Dirty all cells dependent to k T
      match dep-map.get(k)
        Nothing -> ()
        Just(deps : list<string>) -> deps.foreach(fn(dep) is-dirty-map := is-dirty-map.set(dep, True))
      // Set initial key that we put to True
      is-dirty-map := is-dirty-map.set(k, True)
      // Put k,v pair into the spreadsheet
      set-key(k, v)
    fun clean(k)
      is-dirty-map := is-dirty-map.set(k, False)
    fun is-dirty(k) 
      val dirty = is-dirty-map.get(k)
      match dirty
        Nothing -> False
        Just(b : bool) -> b
  action()

pub fun calculate-dependencies(cv : cell-val) : _e list<string>
  /*
    Using a different handler for fetch calls, 
    Given a cell value, returns the keys it is dependent to
  */
  var deps := []
  with handler
    fun fetch(k)
      deps := deps ++ [k]
      1.0
  fetch-cell-val(cv)
  deps

/*
  Cell building logic, optimized
*/

pub effect build
  fun eval(k : string) : float64

pub fun rebuild-handler(action : () -> <build,dirty-bit,state,tasks,console,div,exn|e> a) : <console,div,exn,state,tasks,dirty-bit|e> a
  var value-map : hashtable<float64> := hashtable()
  with handler
    fun eval(key)
      with handler
        fun fetch(k)
          if !is-dirty(k)
          then match value-map.get(k)
            Nothing -> 0.0
            Just(v : float64) -> v
          else 
            val res = fetch(k)
            value-map := value-map.set(k, res)
            clean(k)
            res
      fetch-cell-val(get(key).value)
  action()

/*
  Spreadsheet or cell visualization effect
*/
pub effect visuals
  // Print the full spreadsheet
  fun print-spreadsheet() : ()
  // Print the description of a cell
  fun print-cell-desc(k : string) : ()

pub fun visuals-handler(n-rows : int, n-columns : int, action : () -> <visuals,build,dirty-bit,tasks,state,console|e> a) : <build,dirty-bit,tasks,state,console|e> a
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
      columns.foreach(fn(col) {print(eval(string(x) ++ string(col)));print("\t")})
      println("")
  action()

pub fun handle-app(n-rows : int, n-columns : int, action : () -> <state,tasks,dirty-bit,build,visuals|_e> a) : _e a
  /*
    Handle application operations by combining all of the handlers defined above 
  */
  with spreadsheet-handler()
  with task-handler()
  with dirty-bit-handler()
  with rebuild-handler()
  with visuals-handler(n-rows, n-columns)
  action()

pub fun handle-command(command : string) : <state,tasks,visuals|_e> ()
  // Seperate command handling to a separate function for simpler test code
  match command.split(" ")
    // Commands that are useful for testing
    ["DEPENDENCIES", k] -> 
      get(k).value.calculate-dependencies.foreach(fn(dep)print(dep ++ " "))
      println("")
    // When assigning
    [key, "=", value] -> {
      // Check if key is valid.
      if key.is-key
      then put(key, Cell(value, tokenize-string(value)[0].unjust.interpret-token))
      else println("Invalid key format: " ++ key)
    }
    ["PRINT"] -> print-spreadsheet()
    ["DESC", key] -> print-cell-desc(key)
    [key] -> if key.is-key then println(eval(key)) else println("Invalid key format: " ++ key)
    _ -> println("Invalid input")

fun app() : <state,tasks,dirty-bit,build,visuals|_e> ()
  print("<App Name>$ ")
  readline().to-upper().handle-command
  app()

fun main()
  handle-app(9, 9, app)
  