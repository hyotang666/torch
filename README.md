# TORCH - Making the graph for helping the maintainer to grasp the system.

TORCH makes the graph file which represents lisp system meta info.
It helps you to grasp the third parson's system strcuture.

TORCH uses GRAPHVIZ.
You need to install it.

## From developer

* Product's goal
mmmm...  already?

* License
* Supported implementation
CLISP CCL SBCL ECL.
But maybe works well any other lisps.

## For light user
```lisp
;;; Tutorial - primary API
(code-graph :torch)
;=> #P"/current/working/directory/torch.png"
```
* FAQ
none

## For heavy user

* Dictionary
See doc/\*.md

## For resolver

* Specification of API which integrates implementation dependent behaviors
See package.lisp especially SYMBOL<=FUNCTION.

## For improver

* Specification of DSL
none

## For maintainer

* Structure of the system model
See doc/torch.png
