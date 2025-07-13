[![Release](https://img.shields.io/github/v/release/unprosaiclabyrinth/vara?sort=semver)](https://github.com/unprosaiclabyrinth/Vara/releases)

# Vara DSL

Vara is a domain-specific language for simple algebraic symbolic computation implemented in Scala. Why is this powerful? Consider the following problem: a farmer wants to come up with a closed-form formula that computes an hour's total milk yield $$M$$, given the number of dairy cows $$C$$, the average feed-quality index $$Q$$ (1 - 5 scale), and the average daytime temperature $$T$$ (˚F). A friend of hers has come up with a heavy computational routine that exactly computes $$M(C, Q, T)$$, but only runs on a computer. This algorithm would easily time out on the barn's lightweight controller, and it's logistically inconvenient to contact the friend every hour for the answer. The farmer desperately needs a closed-form in terms of $$C$$, $$Q$$, and $$T$$ so that she can plug numbers on her pocket calculator, or compute it by hand. All her friend can tell her are the exact values for $$M(50, 4, 83)$$, $$M(100, 3, 72)$$, and so on but not a closed-form for $$M(C, Q, T)$$. If only the computation could be done symbolically...

This is where Vara comes in! Suppose the implementation of the complex algorithm looks like:
```scala
def milkYield(numCows: Int, quality: Int, temp: Double): Double = {
  // long, complex computation
}

val M = milkYield(50, 4, 83)
println(M) // prints a value e.g. 54.21
```

This can be tweaked using Vara to look like:
```scala
def milkYield(numCows: VaraExpr, quality: VaraExpr, temp: VaraExpr): VaraExpr = {
  // long, complex computation tweaked for VaraExprs
}

val M = milkYield("C", "Q", "T")
println(M) // prints out a closed-form expression e.g. \frac{CQ}{T^2}!
```
The closed-form expression is in LaTeX syntax so that it can be easily interpreted or copy-pasted into a tool like [QuickLatex](https://www.quicklatex.com/) to make it human-readable.

## Syntax and Semantics

### Operations

### Constructs

## Examples

## Extensibility

## Future Work
