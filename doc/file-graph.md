# [Function] FILE-GRAPH

## Syntax:

(FILE-GRAPH system &key (type :png)) => result

## Arguments and Values:

system := asdf:system designator

type := KEYWORD

result := pathname

## Description:
Making the graph file which format it PNG.
The graph represents asdf:system's component dependencies.

Type specifies output file format.
The default is :png.
To see what format is supported, eval +supported-formats+.
This type is used as both file extension and parameter for underlying dot.
Sometime you may need modify file extension after graph is made.

## Example:

## Affected-By:
File system status.
The system is really installed?

## Side-Effects:
Making the graph file into file system.

When system still not loaded into current lisp image yet, system definition shall be loaded.
(Not system.)

## Notes:

## See-Also:

## Exceptional-Situations:
When SYSTEM is not found, an error type of ASDF/FIND-SYSTEM:MISSING-COMPONENT will be signaled.

When system name (or function name when split is specified t) has invalid character as file name for file system, an error may be signaled.
This is implementation and/or platform dependent.
In such case you need to `(push #\such-invalid-char *invalid-chars*)`.
