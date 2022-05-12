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
    fun out(s)
      if debug then println(s)
      else ()
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

pub fun spreadsheet-handler(action : () -> <state,debug|_e> a) : <debug|_e> a
  var sprsh := hashtable()
  with handler
    fun set-key(k, v)
      out("Set called with key: " ++ k ++ ", value: " ++ v.cell-desc)
      sprsh := sprsh.set(k, v)
    fun get(k)
      out("Get called on key: " ++ k)
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
    fun fetch(k) get(k).value.fetch-cell-val
  action()

pub fun fetch-cell-val(cv : cell-val) :  <exn, div, console, debug, state> decimal
  // IMPORTANT: A helper function is needed because recursive calls to the same function cannot be made inside handlers
  // Helper for the fetch-function, evaluates a cell-val into float
  with task-handler()
  fetch-cell-val-helper(cv)

pub fun fetch-cell-val-helper(cv : cell-val) : <exn, div, tasks, console> decimal
  match cv
    Decimal(i : decimal) -> i
    KeyV(s : string) -> fetch(s)
    SumV(params : list<cell-val>) -> params.foldl(decimal(0.0), fn(total, v) total + v.fetch-cell-val-helper)
    MulV(params : list<cell-val>) -> params.foldl(decimal(1.0), fn(total, v) total * v.fetch-cell-val-helper)
    DivV(params : list<cell-val>) -> 
      val first-val = params[0].unjust.fetch-cell-val-helper
      params.tail.foldl(first-val, fn(total, v) total / v.fetch-cell-val-helper)
    MinusV(params : list<cell-val>) -> 
      val first-val = params[0].unjust.fetch-cell-val-helper
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
      out("Put " ++ k ++ " as " ++ v.cell-desc)
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

/*
  Cell building logic, optimized
*/

pub effect build
  fun eval(k : string) : decimal
  fun get-nonempty-cells() : list<string>

pub fun rebuild-handler(action : () -> <build|_e> a) : _e a
  var value-map : hashtable<decimal> := hashtable()
  with handler
    fun eval(key)
      out("Eval called with " ++ key)
      with handler
        fun fetch(k)
          if !is-dirty(k)
          then 
            out(k ++ " is clean")
            match value-map.get(k)
              Nothing -> decimal(0.0)
              Just(v : decimal) -> v
          else
            out(k ++ " is dirty")
            val res = fetch(k)
            value-map := value-map.set(k, res)
            clean(k)
            res
      val res = fetch-cell-val-helper(get(key).value)
      value-map := value-map.set(key, res)
      clean(key)
      res
    fun get-nonempty-cells()
      value-map.keys()
  action()

/*
  Spreadsheet or cell visualization effect
*/
pub effect visuals
  // Print the full spreadsheet
  fun print-spreadsheet() : string
  // Print the description of a cell
  fun print-cell-desc(k : string) : string

pub fun visuals-handler(action : () -> <visuals | _e> a) : _e a
   //var keys-to-print : list<string> := []
  val columns = list('A', 'H')
  val rows = list('1', '9')
  with handler
    // Prints the cell description
    fun print-cell-desc(k)
      get(k).cell-desc
    // Prints the full spreadsheet
    fun print-spreadsheet()
      val non-empty-cells = get-nonempty-cells()
      var out-str := "\t"
      columns.foreach(fn (col) out-str := out-str ++ string(col) ++ "\t")
      out-str := out-str ++ "\n"
      rows.foreach(fn (row) 
        out-str := out-str ++ row.string ++ "\t"
        columns.foreach(fn(col) 
          val key-to-eval = col.string ++ row.string
          if non-empty-cells.index-of(fn(e) e == key-to-eval) != -1
          then out-str := out-str ++ eval(key-to-eval).show(3) ++ "\t"
          else out-str := out-str ++ "-\t"
        )
        out-str := out-str ++ "\n"
      )
      out-str
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
  var res-str : string := "" 
  // Seperate command handling to a separate function for simpler test code
  match command.split(" ")
    // Commands that are useful for testing
    ["DEPENDENCIES", k] -> 
      get(k).value.calculate-dependencies.foreach(fn(dep)res-str := res-str ++ dep ++ " ")
    // When assigning
    [key, "=", value] -> {
      // Check if key is valid.
      if key.is-key
      then {put(key, Cell(value, tokenize-string(value)[0].unjust.interpret-token));eval(key);()}
      else res-str := "Invalid key format: " ++ key
    }
    ["DESC", key] -> res-str := print-cell-desc(key)
    ["PRINT"] -> res-str := print-spreadsheet()
    [key] -> if key.is-key then res-str := eval(key).show(4) else res-str := "Invalid key format: " ++ key
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
  handle-app(app)
  