module hashtable_vectored
import helpers

/*
 Bucketed hashtable implemetation
*/

pub struct node<a>
  key : string
  item : a
  
pub alias hashtable<a> = forall<h :: H> local-var<h,vector<list<node<a>>>>

val num-buckets = 16

pub fun hashtable<a>()
  vector(num-buckets, []).local-new()

pub fun get(hashtable : hashtable<a>, k : string) : <exn> maybe<a>
  val bucket-idx = bad-hash(k) % (num-buckets - 1)
  val curr-bucket = (hashtable.local-get())[bucket-idx]
  match curr-bucket.find(fn (n) n.key == k)
    Nothing -> Nothing
    Just(n) -> Just(n.item)

pub fun set(hashtable : hashtable<a>, k : string, v : a) : <div, exn>()
  val bucket-idx : int = bad-hash(k) % 15
  val curr-bucket = (hashtable.local-get())[bucket-idx]
  val item-idx :int = curr-bucket.index-of(fn(n) n.key == k)
  if item-idx == -1
  then hashtable[bucket-idx, curr-bucket ++ [Node(k, v)]] // replace-at-index(bucket-idx, curr-bucket ++ [Node(k, v)], hashtable).vector
  else hashtable[bucket-idx, replace-at-index(item-idx, Node(k, v), curr-bucket)]

pub fun keys(hashtable : hashtable<a>) : <div> list<string> 
  var res : list<string> := []
  hashtable.local-get().foreach(fn (bucket) bucket.foreach(fn(node) res := res ++ [node.key]))
  res

pub fun bad-hash(s : string) : int
  // TODO: Find better hash implementation
  s.list.foldl(0, fn(total, elem) total + int(elem))

fun main()
  val table : hashtable<int> = hashtable()
  table.set("A2", 1)
  table.set("A1", 2)
  table.set("A4", 3)
  table.set("Z2", 4)
  table.keys.foreach( fn (key)
    table.set(key, 1)
  )
  table.keys.foreach(fn(k) table.get(k).unjust.println)