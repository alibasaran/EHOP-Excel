module better_build_system

import std/os/file
import std/os/path
import std/num/decimal
import std/os/readline
import src/helpers
import src/tokenizer
import src/interpreter
import hashtable

/*
  Debugger logic, simple debugger handler which outputs a given msg into console iff debug option is True
*/

pub effect debug
  fun out(msg : string) : ()

pub fun debug-handler(debug=False,action : () -> <debug|_e> a) : _e a
  with handler
    fun out(msg) if debug then println(msg) else ()
  action()

/*
  Spreadsheet logic, implemented using a bucketed hashtable
*/
pub struct cell
  cell-desc : string = ""
  task : task = Decimal(decimal(0.0))

pub effect store
  // Set key to value cell
  fun set-key(k : string, v : cell) : () 
  // Get the cell corresponding to k
  fun get(k : string) : cell
  // Get all keys that are nonempty i.e that point to a cell
  fun get-nonempty-keys() : list<string>

pub fun store-handler(action:() -> <store|_e> a) : _e a
  var sprsh := hashtable()
  with handler
    fun set-key(k, v)
      out("SET key: " ++ k ++ ", value: " ++ v.cell-desc)
      sprsh := sprsh.set(k, v)
    fun get(k)
      out("GET key: " ++ k)
      match sprsh.get(k)
        Nothing -> Cell()
        Just(c : cell) -> c
    fun get-nonempty-keys() sprsh.keys()
  action()

/*
  Cell evaluation logic
*/
pub effect tasks
  // Given a key return the value of the keys cell
  fun fetch(k : string) : decimal

fun task-handler(action : () -> <tasks|_e> a) : _e a
  with handler
    fun fetch(k)
      get(k).task.fetch-task
  action()

pub fun fetch-task(t : task) :  <exn, div, console, debug, store> decimal
  with task-handler()
  fetch-task-helper(t)

pub fun fetch-task-helper(t : task) : <exn, div, tasks, console> decimal
  match t
    Decimal(i : decimal) -> i
    KeyV(key : string) -> fetch(key)
    SumV(params : list<task>) -> params.foldl(decimal(0.0), fn(total, tsk) total + tsk.fetch-task-helper)
    MulV(params : list<task>) -> params.foldl(decimal(1.0), fn(total, tsk) total * tsk.fetch-task-helper)
    DivV(params : list<task>) -> 
      val first-val = params.head.unjust.fetch-task-helper
      params.tail.foldl(first-val, fn(total, tsk) total / tsk.fetch-task-helper)
    MinusV(params : list<task>) -> 
      val first-val = params.head.unjust.fetch-task-helper
      params.tail.foldl(first-val, fn(total, tsk) total - tsk.fetch-task-helper)

/*
  Cell rebuilding logic
  Follows dirty bit rebuilder from Build Systems a La Carte
*/
pub effect dirty-bit
  /*
    Given a key string as input returns True if the key is dirty i.e the cell it points to or its dependencies have changed
  */
  fun is-dirty(k : string) : bool
  /*
    Given a key as input cleans the key's cell
  */
  fun clean(k : string) : ()
  /*
    Intermediary function that is called before the final set call.
    Makes input key k dirty as well as the cells dependent to k
  */
  fun put(k : string, desc : string) : ()

pub fun dirty-bit-handler(action : () -> <dirty-bit|_e> a) : _e a
  // A map that maps a key to its dependents.
  // Ex. A1 = SUM(1, A2) => dep-map "A2" = [A1]
  var dep-map : hashtable<list<string>> := hashtable()
  // A map that given a key returns true if dirty false otherwise
  var is-dirty-map : hashtable<bool> := hashtable()
  with handler
    fun put(k, desc)
      // Convert the value from string to cell
      val cell = Cell(desc, tokenize-string(desc)[0].unjust.interpret-token)
      cell.task.calculate-dependencies.foreach(fn (dep)
        match dep-map.get(dep)
          // If initial entry of dep
          Nothing -> dep-map := dep-map.set(dep, [k])
          // If dep already in dep-map
          Just(deps : list<string>) -> if deps.index-of(fn(e) e == k) == -1 then dep-map := dep-map.set(dep, deps ++ [k])
      )
      // Dirty all cells dependent to k T as well as k
      dep-map.get-keys-to-dirty(k).foreach(fn (dk) is-dirty-map := is-dirty-map.set(dk, True))
      // Put k,v pair into the spreadsheet
      set-key(k, cell)
    fun clean(k)
      is-dirty-map := is-dirty-map.set(k, False)
    fun is-dirty(k) 
      val dirty = is-dirty-map.get(k)
      match dirty
        Nothing -> False
        Just(b : bool) -> b
  action()

/*
  Calculates direct dependencies of a task
  Uses EHOP to its advantage and easily gathers dependencies by simply using a different handler
*/
pub fun calculate-dependencies(t : task) : <exn, div|_e> list<string>
  var deps := []
  with handler
    fun fetch(k)
      deps := deps ++ [k] 
      decimal(1.0)
  t.fetch-task-helper
  deps

/*
  Given a dependency map and a dirty key returns a list of all the keys that should be dirty
*/
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
  /*
    Highest level cell evaluation function, depending on if the input key k is dirty calls fetch
    otherwise returns computed value of k in the previous runs
  */
  fun eval(k : string) : decimal

pub fun build-handler(action : () -> <build|_e> a) : _e a
  var value-map : hashtable<decimal> := hashtable()
  with handler
    fun eval(key)
      // If key is clean return its value directly from the value-map
      if !is-dirty(key) then 
        out(key ++ " is clean")
        match value-map.get(key)
          Nothing -> decimal(0.0)
          Just(v : decimal) -> v
      // Else recompute the key
      else
        out(key ++ " is dirty")
        // Use a handler where whenever a fetch is called i.e the initial key's one of the dependencies
        // needs to be computed, only compute it iff it is dirty
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
        val res = get(key).task.fetch-task-helper
        out("FETCH key: " ++ key ++ " result " ++ res.show())
        clean(key)
        value-map := value-map.set(key, res)
        res
  action()

/*
  Spreadsheet or cell visualization effect
*/
pub effect visuals
  // Returns the spreadsheet as a string
  fun print-spreadsheet() : string
  // Returns the description of the cell i.e with what command it is being computed as a string
  fun print-cell-desc(k : string) : string
  // Change the dimensions of the spreadsheet
  fun update-view-window(n-rows : int, n-cols : int) : ()
  // Change the view pointer
  fun update-pointer(key : string) : ()

pub fun visuals-handler(action : () -> <visuals|_e> a) : _e a
  // Store a tuple for the window dimensions
  var window-size := (10, 10)
  // Store a tuple for the pointer where first value is the column and second is the row
  var pointer-key := ("A", 1)
  with handler
    // Prints the cell description
    fun print-cell-desc(k) get(k).cell-desc
    // Prints the full spreadsheet
    fun print-spreadsheet()
      val non-empty-keys = get-nonempty-keys()
      val rows = list(pointer-key.snd, pointer-key.snd + window-size.fst - 1).map(show)
      val columns = list-from-keys(pointer-key.fst, (pointer-key.fst.col-to-int + window-size.snd - 1).to-key)
      // Write columns
      val header = "\t" ++ columns.join("\t") ++ "\n"
      val body = rows.map(fn (row) row ++ "\t" ++ columns.map(fn(col) 
          val key-to-eval = col ++ row
          if non-empty-keys.index-of(fn(e) e == key-to-eval) != -1
          then eval(key-to-eval).show(-3)
          else "-"
      ).join("\t"))
      val view-details = "Window: " ++ window-size.fst.show ++ " x " ++ window-size.snd.show ++ "\t Pointer on: " ++ pointer-key.fst ++ pointer-key.snd.show
      header ++ body.join("\n") ++ "\n" ++ view-details
    fun update-view-window(n-rows, n-cols) window-size := (n-rows, n-cols)
    fun update-pointer(key) pointer-key := key.separate-key()
  action()

/*
  Handle application operations by combining all of the handlers defined above 
*/
pub fun handle-app(action : () -> <debug,store,tasks,dirty-bit,build,visuals|_e> a, debug=False) : <console|_e> a
  with debug-handler(debug)
  with store-handler()
  with task-handler()
  with dirty-bit-handler()
  with build-handler()
  with visuals-handler()
  action()

/*
  Application logic
  Where user input commands are handled and directed into application calls
*/
pub fun handle-command(command : string) : <debug,store,tasks,dirty-bit,build,visuals|_e> string
  var res-str : string := "\n" ++ command ++ ": "
  // Handle exceptions by appending them to the resulting string, we do not want the application to close
  with handler
    ctl throw-exn(e)
      res-str := res-str ++ e.show()
      res-str
  match command.split(" ")
    // Assign key to a value
    [key, "=", value] -> {key.check-key; put(key, value)}
    
    // Get cell description i.e with what command it is being computed
    ["DESC", key] -> {key.check-key; res-str := res-str ++ print-cell-desc(key)}
    
    // When called, returns the value of the requested key
    [key] -> {key.check-key; res-str := res-str ++ eval(key).show(-3) ++ "\n"}
    
    // Window size changing operation
    ["WINDOW", n-rows-str, n-cols-str] -> 
      val n-cols = n-cols-str.to-int
      val n-rows = n-rows-str.to-int
      if n-cols < 1 || n-cols > 25
      then throw("Specified number of columns is out of the valid range: " ++ "1 < " ++ n-cols-str ++ " < 26 is not true\nFitting specified value in the dimension range...")
      if n-rows < 1
      then throw("Specified number of rows cannot be less than 1! Changing specified number of rows into 1")
      update-view-window(n-rows.max(1), n-cols.max(1).min(25))

    // Moves the pointer to the specified key
    ["MV-TO", key] -> {key.check-key; update-pointer(key)}
    _ -> throw("Invalid command")
  res-str

fun app() : <debug,store,tasks,dirty-bit,build,visuals|_e> ()
  print("\nEnter a command $ ")
  val res-str = readline().to-upper().handle-command
  println("\n" ++ print-spreadsheet() ++ "\n" ++ res-str)
  app()

fun main()
  handle-app(app, False)
  