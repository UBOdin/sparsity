# sparsity
Fast, Free, Pure Scala SQL Parser built with [fastparse](http://www.lihaoyi.com/fastparse/)

## Example
```
import sparsity.parser.SQL
import fastparse.Parsed

val tree = SQL("SELECT * FROM R")
```

## ASTs
* [Statement AST](https://github.com/UBOdin/sparsity/blob/master/src/main/scala/sparsity/statement/Statement.scala)
* [Expression AST](https://github.com/UBOdin/sparsity/blob/master/src/main/scala/sparsity/expression/Expression.scala)

## Maven
```
libraryDependencies ++= Seq("info.mimirdb" %% "sparsity" % "1.5")
```
