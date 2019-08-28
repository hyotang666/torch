# [Function] FUNCTION-GRAPH

## Syntax:

(FUNCTION-GRAPH function &key (ignore-privates `*ignore-privates*`) (type :png) system) => result

## Arguments and Values:

function := function designator

ignore-privates := BOOLEAN

type := KEYWORD

system := (or null asdf:system-designator)

result := pathname

## Description:
Making the graph file which format is PNG.
The graph represents program structure of the FUNCTION.

When ignore-privates is T, privates (The function which is defined globally but be used by only one function.) does not appear in the result graph.
The deafult is `*IGNORE-PRIVATES*` and its default value is NIL.

System may have differnt package name.
In such case, you need to specify its system name with keyword parameter SYSTEM.
When it is NIL (default), PACKAGE-NAME is used as system name.

Type specifies output file format.
The default is :png.
To see what format is supported, eval +supported-formats+.
This type is used as both file extension and parameter for underlying dot.
Sometime you may need modify file extension after graph is made.

## Example:

## Affected-By:
`TORCH:*IGNORE-PRIVATES*`

File system status.
The system is really installed?

## Side-Effects:
Making the graph file into file system.

## Notes:
FUNCTION-GRAPH needs to reASDF:LOAD-SYSTEM internally with specifies :force T.
It works slow.

This is useful for making the internal function graph, especially the system is very huge.

## See-Also:

## Exceptional-Situations:
When system is not found, an error type of ASDF/FIND-SYSTEM:MISSING-COMPONENT will be signaled.

When system name (or function name when split is specified t) has invalid character as file name for file system, an error may be signaled.
This is implementation and/or platform dependent.
In such case you need to `(push #\such-invalid-char *invalid-chars*)`.

