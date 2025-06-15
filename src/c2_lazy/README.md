# Being Lazy

Type definition for OCaml built-in
```ocaml
type 'a list = Nil | Cons of 'a * 'a list
```
This either is an empty list or a _head_ of `'a` or a tail of `'a list`.

An __infinitely__ long list where elements are created as needed is a _lazy list_.

Instead of a _tail_ as a concrete datastructure you have a _tail function_.

```ocaml
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)
```
No need for [] nil as there is no end to the list.

# Build a list of arbitrary length
```ocaml
let rec lseq n =
    Cons (n, fun () -> lseq (n + 1))
val lseq : int -> int lazylist = <fun>

let lhd (Cons (n, _)) = n
val lhd : 'a lazylist -> 'a = <fun>

let ltl (Cons (_, tf)) = tf ()
val ltl : 'a lazylist -> 'a lazylist = <fun>
```
