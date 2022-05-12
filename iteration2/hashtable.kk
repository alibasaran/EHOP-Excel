module hashtable
import helpers

/*
 Bucketed hashtable implemetation
*/

pub struct node<a>
  key : string
  item : a
  
pub alias hashtable<a> = list<list<node<a>>>

val num-buckets = 16

pub fun hashtable<a>() : hashtable<a>
  replicate([], num-buckets)

pub fun get(hashtable : hashtable<a>, k : string) : _e maybe<a>
  val bucket-idx = bad-hash(k) % (num-buckets - 1)
  val curr-bucket = hashtable[bucket-idx].unjust
  match curr-bucket.find(fn (n) n.key == k)
    Nothing -> Nothing
    Just(n) -> Just(n.item)

pub fun set(hashtable : hashtable<a>, k : string, v : a) : _e hashtable<a>
  val bucket-idx = bad-hash(k) % 15
  val curr-bucket = hashtable[bucket-idx].unjust
  val item-idx = curr-bucket.index-of(fn(n) n.key == k)
  if item-idx == -1
  then replace-at-index(bucket-idx, curr-bucket ++ [Node(k, v)], hashtable)
  else replace-at-index(bucket-idx, replace-at-index(item-idx, Node(k, v), curr-bucket), hashtable)

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