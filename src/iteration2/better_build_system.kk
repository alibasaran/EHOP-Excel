module better_build_system

import std/os/file
import std/os/path
import std/num/decimal
import std/os/readline
import helpers
import tokenizer
import interpreter
import hashtable

/*
Below is the implementation of a "busy" build system. The term is used in Build Systems à la Carte by ANDREY MOKHOV, NEIL MITCHELL, SIMON PEYTON JONES
meaning that the system is not minimal: 

Definition 2.1 (Minimality). A build system is minimal if it executes tasks at most once per build
and only if they transitively depend on inputs that changed since the previous build

Ex. If A1 = A2 + A2 then the cell on A2 will be fetched twice, even though though in between fetchs the value of A2's dependencies did not change
*/

pub effect debug
  fun out(s : string) : ()

pub fun debug-handler(debug=False,action : () -> <debug|_e> a) : _e a
  with handler
    fun out(s) if debug then println(s)else ()
  action()

/*
  Spreadsheet logic, implemented as a bucketed hash table.
*/
pub struct cell
  cell-desc : string = ""
  value : cell-val = Decimal(decimal(0.0))

pub effect state
  fun set-key(k : string, v : cell) : () 
  fun get(k : string) : cell
  fun get-nonempty-keys() : list<string>

pub fun spreadsheet-handler(action : () -> <state,debug|_e> a) : <debug|_e> a
  var sprsh := hashtable()
  with handler
    fun set-key(k, v)
      out("SET key: " ++ k ++ ", value: " ++ v.cell-desc)
      sprsh := sprsh.set(k, v)
    fun get-nonempty-keys()
      sprsh.keys()
    fun get(k)
      out("GET key: " ++ k)
      match sprsh.get(k)
        Nothing -> Cell()
        Just(c : cell) -> c
  action()

/*
Cell evaluation logic
*/
pub effect tasks
  // Given a key return the value of the keys cell
  fun fetch(k : string) : decimal

fun task-handler(action : () -> <tasks,state,debug|_e> a) : <state,debug|_e> a
  with handler
    fun fetch(k)
      get(k).value.fetch-cell-val
  action()

pub fun fetch-cell-val(cv : cell-val) :  <exn, div, console, debug, state> decimal
  with task-handler()
  fetch-cell-val-helper(cv)

pub fun fetch-cell-val-helper(cv : cell-val) : <exn, div, tasks, console> decimal
  match cv
    Decimal(i : decimal) -> i
    KeyV(s : string) -> fetch(s)
    SumV(params : list<cell-val>) -> params.foldl(decimal(0.0), fn(total, v) total + v.fetch-cell-val-helper)
    MulV(params : list<cell-val>) -> params.foldl(decimal(1.0), fn(total, v) total * v.fetch-cell-val-helper)
    DivV(params : list<cell-val>) -> 
      val first-val = params.head.unjust.fetch-cell-val-helper
      params.tail.foldl(first-val, fn(total, v) total / v.fetch-cell-val-helper)
    MinusV(params : list<cell-val>) -> 
      val first-val = params.head.unjust.fetch-cell-val-helper
      params.tail.foldl(first-val, fn(total, v) total - v.fetch-cell-val-helper)

/*
  Cell rebuilding logic
  Follows dirty bit rebuilder from Build Systems a La Carte
*/
pub effect dirty-bit
  fun is-dirty(k : string) : bool
  fun clean(k : string) : ()
  fun put(k : string, v : cell) : ()

pub fun dirty-bit-handler(action : () -> <dirty-bit|_e> a) : _e a
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
      // Dirty all cells dependent to k T as well as k
      dep-map.get-keys-to-dirty(k).foreach(fn (dk) is-dirty-map := is-dirty-map.set(dk, True))
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

pub fun calculate-dependencies(cv : cell-val) : <exn, div | _e> list<string>
  /*
    Using a different handler for fetch calls, 
    Given a cell value, returns the keys it is dependent to
  */
  var deps := []
  with handler
    fun fetch(k)
      deps := deps ++ [k] 
      decimal(1.0)
  fetch-cell-val-helper(cv)
  deps

pub fun get-keys-to-dirty(dep-map : hashtable<list<string>>, dirty-key : string) : <div, exn> list<string> 
  var res : list<string> := [dirty-key]
  match dep-map.get(dirty-key) 
    Nothing -> ()
    Just(deps : list<string>) -> deps.foreach(fn(dep) res := res ++ dep-map.get-keys-to-dirty(dep))
  res

/*
  Cell building logic, optimized
*/

pub effect build
  fun eval(k : string) : decimal

pub fun rebuild-handler(action : () -> <build|_e> a) : _e a
  var value-map : hashtable<decimal> := hashtable()
  with handler
    fun eval(key)
      if !is-dirty(key) then 
        out(key ++ " is clean")
        match value-map.get(key)
          Nothing -> decimal(0.0)
          Just(v : decimal) -> v
      else
        out(key ++ " is dirty")
        with handler
          fun fetch(k)
            if !is-dirty(k) then 
              match value-map.get(k)
                Nothing -> decimal(0.0)
                Just(v : decimal) -> v
            else
              val res = fetch(k)
              out("FETCH key: " ++ k ++ " result " ++ res.show())
              value-map := value-map.set(k, res)
              clean(k)
              res
        val res = get(key).value.fetch-cell-val-helper
        out("FETCH key: " ++ key ++ " result " ++ res.show())
        clean(key)
        value-map := value-map.set(key, res)
        res
  action()

/*
  Spreadsheet or cell visualization effect
*/
pub effect visuals
  // Print the full spreadsheet
  fun print-spreadsheet() : string
  // Print the description of a cell
  fun print-cell-desc(k : string) : string
  // Change the dimensions of the spreadsheet
  fun change-spreadsheet-dimensions(n-rows : int, n-cols : int) : ()

pub fun visuals-handler(action : () -> <visuals | _e> a) : _e a
  /*
    Problem: Works when columns and rows are initialized with val, but not with var
    Raises error: abstract type(s) escape(s) into the context
    
    var columns : list<char> := list('A', 'H').map(string)
    var rows : list<char> := list('1', '9').map(string)
  */
  val columns : list<string> = list('A', 'H').map(string)t
  val rows : list<string> = list('1', '9').map(string)
  with handler
    // Prints the cell description
    fun print-cell-desc(k)
      get(k).cell-desc
    // Prints the full spreadsheet
    fun print-spreadsheet()
      val non-empty-keys = get-nonempty-keys()
      var out-str := "\t"
      columns.foreach(fn (col) out-str := out-str ++ col ++ "\t")
      out-str := out-str ++ "\n"
      rows.foreach(fn (row) 
        out-str := out-str ++ row ++ "\t"
        columns.foreach(fn(col) 
          val key-to-eval = col ++ row
          if non-empty-keys.index-of(fn(e) e == key-to-eval) != -1
          then out-str := out-str ++ eval(key-to-eval).show(-3) ++ "\t"
          else out-str := out-str ++ "-\t"
        )
        out-str := out-str ++ "\n"
      )
      out-str
    fun change-spreadsheet-dimensions(n-rows, n-cols)
      println(n-rows.show ++ " " ++ n-cols.show())
      // columns := list('A', ('A'.int + n-cols).char).map(string)
      // rows := list(1, n-cols + 1).map(fn (row-int) row-int.show())
  action()

pub fun handle-app(action : () -> <debug,state,tasks,dirty-bit,build,visuals|_e> a, debug=False) : _e a
  /*
    Handle application operations by combining all of the handlers defined above 
  */
  with debug-handler(debug)
  with spreadsheet-handler()
  with task-handler()
  with dirty-bit-handler()
  with rebuild-handler()
  with visuals-handler()
  action()

pub fun handle-command(command : string) : <debug,state,tasks,dirty-bit,build,visuals|_e> string
  var res-str : string := command ++ "\n" 
  // Seperate command handling to a separate function for simpler test code
  with handler
    ctl throw-exn(e)
      res-str := e.show()
      res-str
  match command.split(" ")
    ["CHANGE-DIMS", n-rows-str, n-cols-str] -> {
      val n-cols = string-to-decimal(n-cols-str).int
      val n-rows = string-to-decimal(n-rows-str).int
      if n-cols < 1 || n-cols > 25
      then println("Specified number of calls is out of the valid range: " ++ "1 < " ++ n-cols.show() ++ " < 26 is not true\n fitting specified value in the dimension range...")
      val sanitized-n-cols = n-cols.max(1).min(25)
      change-spreadsheet-dimensions(n-rows, n-cols)
    }
    // Commands that are useful for testing
    ["DEPENDENCIES", k] -> 
      get(k).value.calculate-dependencies.foreach(fn(dep)res-str := res-str ++ dep ++ " ")
    // When assigning
    [key, "=", value] -> {
      // Check if key is valid.
      if key.is-key
      then {put(key, Cell(value, tokenize-string(value)[0].unjust.interpret-token))}
      else res-str := "Invalid key format: " ++ key
    }
    ["DESC", key] -> res-str := print-cell-desc(key)
    ["PRINT"] -> res-str := print-spreadsheet()
    [key] -> if key.is-key then res-str := res-str ++ eval(key).show(-3) ++ "\n" else res-str := "Invalid key format: " ++ key
    _ -> res-str := "Invalid input"
  res-str

fun app() : <debug,state,tasks,dirty-bit,build,visuals|_e> ()
  // print("<App Name>$ ")
  print("\nEnter a command $ ")
  val res-str = readline().to-upper().handle-command
  print-spreadsheet().println()
  println(res-str)
  app()

fun main()
  handle-app(app, True)
  