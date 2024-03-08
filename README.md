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
- [ ] Emacs plugin
  - [ ] `REKISHI` major mode
  - [ ] Basic binding functionality (load symbol, auto-reload on re-bind)
