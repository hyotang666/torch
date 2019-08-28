# [Function] PACKAGE-GRAPH

## Syntax:

(PACKAGE-GRAPH package &key (type :png)) => result

## Arguments and Values:

package := package designator

type := KEYWORD

result := pathname

## Description:
Making the graph file which format is PNG.
The graph represents package dependencies.

Type specifies output file format.
The default is :png.
To see what format is supported, eval +supported-formats+.
This type is used as both file extension and parameter for underlying dot.
Sometime you may need modify file extension after graph is made.

## Example:

## Affected-By:
Current lisp image status.

## Side-Effects:
Making the graph file into file system.

## Notes:
PACKAGE-GRAPH uses CL:PACKAGE-USE-LIST internaly, so IMPORTed symbol's package or prefixed symbol's package is not recognized.

## See-Also:

## Exceptional-Situations:
When package is not found, an error will be signaled.

When system name (or function name when split is specified t) has invalid character as file name for file system, an error may be signaled.
This is implementation and/or platform dependent.
In such case you need to `(push #\such-invalid-char *invalid-chars*)`.
