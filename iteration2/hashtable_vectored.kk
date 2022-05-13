module hashtable_vectored
import helpers

/*
 Bucketed hashtable implemetation
*/

pub struct node<a>
  key : string
  item : a
  
pub alias hashtable<a> = vector<list<node<a>>>

val num-buckets = 16

pub fun hashtable<a>() : hashtable<a>
  vector(num-buckets, [])

pub fun get(hashtable : hashtable<a>, k : string) : _e maybe<a>
  val bucket-idx = bad-hash(k) % (num-buckets - 1)
  val curr-bucket = hashtable[bucket-idx]
  match curr-bucket.find(fn (n) n.key == k)
    Nothing -> Nothing
    Just(n) -> Just(n.item)

pub fun set(hashtable : hashtable<a>, k : string, v : a) : _e hashtable<a>
  var hashtable-local := hashtable.local-new()
  val bucket-idx = bad-hash(k) % 15
  var curr-bucket := hashtable[bucket-idx]
  val item-idx = curr-bucket.index-of(fn(n) n.key == k)
  if item-idx == -1
  then hashtable-local[bucket-idx, curr-bucket ++ [Node(k, v)]] // replace-at-index(bucket-idx, curr-bucket ++ [Node(k, v)], hashtable).vector
  else hashtable-local[bucket-idx, replace-at-index(item-idx, Node(k, v), curr-bucket)]
  hashtable-local.local-get()

pub fun keys(hashtable : hashtable<a>) : _e list<string>
  var res : list<string> := []
  hashtable.foreach(fn (bucket) bucket.foreach(fn(node) res := res ++ [node.key]))
  res

pub fun bad-hash(s : string) : int
  // TODO: Find better hash implementation
  s.list.foldl(0, fn(total, elem) total + int(elem))

// fun main()
//   var table := hashtable()
//   table := table.set("A2", 4)
//   table := table.set("A1", 4)
//   table := table.set("A4", 4)
//   table := table.set("Z2", 4)
//   table.keys.foreach(println)