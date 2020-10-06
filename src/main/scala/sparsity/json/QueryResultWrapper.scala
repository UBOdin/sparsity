package sparsity.json

/**
 * How should a Json query be returned.  
 * 
 * A [Path] can return a singleton element (e.g., isSingleton), 
 * or may reference multiple elements.  This class specifies
 * whether a path result should be interpreted as a singleton
 * and returned bare, or automatically wrapped in an array.
 *
 * e.g., Take the input
 * ```
 *   { 'a': 1, 'b' : 2, 'c' : [3] }
 * ```
 * 
 * * [NoWrapper]()
 *     - `$.a` returns `1`
 *     - `$.*` is an error
 *     - `$.c[0]` returns `3`
 *     - `$.c[*]` is an error
 * * [ArrayWrapper]
 *     - `$.a` returns `[1]`
 *     - `$.*` returns `[1, 2, [3]]`
 *     - `$.c[0]` returns `[3]`
 *     - `$.c[*]` returns `[3]`
 * * [ConditionalArrayWrapper]
 *     - `$.a` returns `1`
 *     - `$.*` returns `[1, 2, [3]]`
 *     - `$.c[0]` returns `3`
 *     - `$.c[*]` returns `3`
 */
sealed trait QueryResultWrapper

/**
 * Assume the result is a singleton and return it bare
 */
case class NoWrapper() extends QueryResultWrapper
  { override def toString = "WITHOUT ARRAY WRAPPER" }

/**
 * Always return the result with an array wrapper.
 */
case class ArrayWrapper() extends QueryResultWrapper
  { override def toString = "WITH UNCONDITIONAL ARRAY WRAPPER" }

/**
 * Return singletons bare and wrap multiple results in an array
 */
case class ConditionalArrayWrapper() extends QueryResultWrapper
  { override def toString = "WITH CONDITIONAL ARRAY WRAPPER" }
