# [Function] CODE-GRAPH

## Syntax:

(CODE-GRAPH system &key (ignore-privates `*ignore-privates*`) (ignore-standalone `*ignore-standalone*`) (split `*split*`) (type :png) package) => result

## Arguments and Values:

system := asdf:system designator

ignore-privates := BOOLEAN

ignore-standalone := BOOLEAN

split := BOOLEAN

type := KEYWORD

package := package designator

result := (or null pathname (pathname+))

## Description:
Making the graph file which format is PNG.
The graph represents program structures.

When ignore-privates is T, privates (The function which is defined globally but be used by only one function.) does not appear in the result graph.
The deafult is `*IGNORE-PRIVATES*` and its default value is NIL.

When ignore-standalone is T, standalone (The nodes which does not have edge.) is removed from result graph.
The default is `*IGNORE-STANDALONE*` and its default value is NIL.

When split is T, result is the list which element is pathname.
Making each external function graph.

Type specifies output file format.
The default is :png.
To see what format is supported, eval +supported-formats+.
This type is used as both file extension and parameter for underlying dot.
Sometime you may need modify file extension after graph is made.

System may have differnt package name.
In such case, you need to specify its package name with keyword parameter PACKAGE.

## Example:
```lisp
(code-graph :torch)
;=> #P"/current/working/directory/torch.png"
```

## Affected-By:
`TORCH:*IGNORE-PRIVATES*` `TORCH:*IGNORE-STANDALONE*` `TORCH:*SPLIT*`

File system status.
The system is really installed?

## Side-Effects:
Making the graph file into file system.

When still system is not loaded into current lisp image yet, system shall be loaded.

## Notes:
CODE-GRAPH needs to reASDF:LOAD-SYSTEM internally with specifies :force T.
It works slow.

CODE-GRAPH is useful for grasping the structure of the third parson's products.

## See-Also:

## Exceptional-Situations:
When SYSTEM is not found, an error type of ASDF/FIND-SYSTEM:MISSING-COMPONENT will be signaled.

When system name (or function name when split is specified t) has invalid character as file name for file system, an error may be signaled.
This is implementation and/or platform dependent.
In such case you need to `(push #\such-invalid-char *invalid-chars*)`.
