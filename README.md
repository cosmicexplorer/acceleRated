acceleRated
===========

Why not?

# Discussions

## Functions

[Functions](R/functions.R) are defined to ease the use of other modules.

## Typing

[Types](R/types.R) are defined primarily so that functions can check and manipulate their arguments. Often, these checks and coercions may be complex, and also may need to be performed many times for different functions exposed as an API, or even internally. R has a *highly* usable type system in S3/S4, but constructing these classes can be cumbersome for small functions, and if not used intelligently, can use up more time or memory than necessary in repeated construction. Implementing the desired type system through classes would be redundant.

We would like the ability to "tag" values with types, but for this to not take up any space at runtime. Luckily, R has a fantastic metaprogramming interface. We will allow values to be declared with a type in an environment, and with *metaprogramming magic*, these types can bubble up to delegating functions, if they also use this type system. Types can be composed, and variables declared with types resolve to their actual values at runtime.

Example:

``` R
newtype(degree, double, explicit_coercions = TRUE)
newtype(radian, double, explicit_coercions = TRUE)
coercion(degree, radian, _ / 180.0 * pi) # uses single-arg anonymous function syntax
coercion(radian, degree, _ / pi * 180.0)

newtype(trigResult, and(double, between(-1, 1)))
safe_sin <- (trigResult <- radian) %f% sin(_)

theta <- typed(and(double, nonNegative), 3.14)
print(safe_sin(theta)) # prints something close to 0
print(safe_sin(radian(1.5))) # prints something close to 1
print(safe_sin(1.5)) # error; need explicit coercion
print(safe_sin(radian(""))) # error; incompatible types

# argument to safe_sin doesn't need to be coerced, because its type is declared
some_math <- (double <- radian) %f% 2 * safe_sin(_)
```


# LICENSE

[GPL 3.0+](LICENSE)
