module bucketed_hash_table
import helpers

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

pub fun bad-hash(s : string) : int
  // TODO: Find better hash implementation
  s.list.foldl(0, fn(total, elem) total + int(elem))

fun main()
  var ht := hashtable()
  ht := ht.set("A1", True)
  println(ht.get("A1").unjust)
  ht := ht.set("A1", False)
  println(ht.get("A1").unjust)


// pub fun hashtable(action : () -> <hashtable|_e> a) : _e a
//   var buckets-list := replicate([], 16)
//   with handler
//     fun insert(k, v)
//       val bucket-idx = bad-hash(k) % 15
//       val curr-bucket = buckets-list[bucket-idx].unjust
//       val item-idx = curr-bucket.index-of(fn(n) n.key == k)
//       if item-idx == -1
//       then buckets-list := replace-at-index(bucket-idx, curr-bucket ++ [Node(k, v)], buckets-list)
//       else buckets-list := replace-at-index(bucket-idx, replace-at-index(item-idx, Node(k, v), curr-bucket), buckets-list)
//     fun get(k)
//       val bucket-idx = bad-hash(k) % 15
//       val curr-bucket = buckets-list[bucket-idx].unjust
//       match curr-bucket.find(fn (n) n.key == k)
//         Nothing -> Nothing
//         Just(n) -> Just(n.item)
//   action()