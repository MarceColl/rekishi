# Rekishi (歴史)

Rekishi is an experimental integrated CVS and development environment for Common Lisp.

It keeps track of all modifications you make to your functions and macros and creates a version graph
with all your past editions. You can traverse this graph, inspect it, and rebind the symbol
(or any other symbol) to a previous version.

This software is in absolute ALPHA proof-of-concept quality, do not use for anything serious, but,
if you want to collaborate on this let me know :)

## Roadmap

- [x] Save and load function definitions from local sqlite3 database
- [x] Forwards and backwards navigation on the history graph of a function
- [ ] Keep track of dependencies between functions and macros (object -> binding)
- [ ] Commit system
- [ ] Save and load macros from local sqlite3 database
- [ ] Save and load other kinds of top-level constructs
  - [ ] What to do with top level forms, ifs, lets, etc, should we just not take care of that and handle functions and macros only?
- [ ] Remote deployment of commits
  - [ ] Keep track of loaded objects and their bindings
  - [ ] Be able to deploy only needed objects
  - [ ] If failures appear when deploying, rollback to previous objects
  - [ ] Run tests before deploying with the commit version of the functions
- [ ] Emacs package
  - [ ] `REKISHI` major mode
  - [ ] Basic binding functionality (load symbol, auto-reload on re-bind)

## Emacs Lisp package idea

The idea is to serve as kind of an structural editor. You don't operate with files but with symbols.

[Q: How would that later translate into a file?]

Let's see a sample flow of the idea I have in mind:

You start with an empty buffer in the `REKISHI` major mode, from there you can create a new symbol.

`(rekishi-new-symbol)` asks you for a symbol name and a symbol type, we'll write `add-1` for name and `function` for type,
then this shows up:

```lisp
SYMBOL: ADD-1

(defun/r add-1 (a))
```

You edit it to what you want and `C-c C-c` (eval-defun in sly, same keybinding)

```lisp
SYMBOL: ADD-1

(defun/r add-1 (a)
  (+ a 2))
```

Now you have created a function in your package as with a normal defun, but something else has happened

```SQL
sqlite> SELECT * FROM objects WHERE hash = (SELECT object FROM bindings WHERE binding = 'ADD-1' AND package = '...');

| hash       | definiton | mtime     |
| c0a4b4f0...|(A) (+ A 2)|1709884073 |
```

Rekishi has saved the definition of this function in the local rekishi database, let's change the definition of the function
and `C-c C-c` again.

```lisp
SYMBOL: ADD-1

(defun/r add-1 (a)
  (+ a 1))
```

We get another entry if we run the same query as before

```SQL
sqlite> SELECT * FROM objects WHERE hash = (SELECT object FROM bindings WHERE binding = 'ADD-1' AND package = '...');

| hash       | definiton | parent    | mtime     |
| a1341fb0...|(A) (+ A 2)|c0a4b4f0...|1709884073 |
```

Now the binding `ADD-1` points to the new definition, and we have a reference to the parent.

We can inspect the history of the function with `(get-history 'add-1)`

Let's say we liked our previous definition better, we can place our cursor on top of the symbol in the buffer
and run `(rekishi-prev)`. This does several things, automatically changes the definition in your buffer so you now see 
the old one again:

```lisp
SYMBOL: ADD-1                        2 versions

(defun/r add-1 (a)
  (+ a 2))
```

It also rebinds the function locally to the old one

```lisp
(add-1 1)

=> 3
```

And it sets the binding in the `bindings` table to point to the old one.

Now let's add an existing symbol to the buffer. We can use the `(rekishi-bring-symbol)` function, that shows us a list
of symbols to bring in the emacs completion framework.

Let's say we want to bring a function called `setup-connection` in the `database` package. Now our buffer would look like this:

```lisp
SYMBOL: ADD-1                         2 versions

(defun/r add-1 (a)
  (+ a 2))

  
SYMBOL: SETUP-CONNECTION (DATABASE)   14 versions

(defun/r setup-connection (db-uri)
  (setf *connection* (dbi:connect :postgres db-uri)))
```

As you can see you basically bring what you need in the current buffer and operate on it at the same time, you can return symbols,
reorder them and even bring the same symbol at a different version.
