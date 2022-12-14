# Zack's Programmable Calculator (zpcalc)

An interactive [RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator and programming language inspired by Forth and Lisp.

![zpcalc demo](zpcalc.gif)

## Installation

Requirements: sbcl (or ecl)

1. Git clone this repository and `cd` into it.
2. Run `make build`.
3. Now, you have a static executable called `zpcalc`! Simply run with `./zpcalc`.

## How to Use

It's a simple [RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator.
Type a number and then press enter to push it on to the stack.
You can execute a function by typing its name.
For example, if you have two numbers on the stack and you enter "+", you will get the sum of those two numbers.
The stack is displayed as a vertical list of numbers, but don't get confused - the bottom-most element is the "top" element of the stack.
For a list of builtin functions, see [below](#builtins).
Quit by entering `quit` or sending EOF with ctrl-d.

Note: If you enter multiple tokens, they will all be processed sequentially.
For example, typing `30 20 * <return>` will result in `600` (it's the same as doing `30 <return> 20 <return> * <return>`).
If you want to group actions together, simply put them in parentheses.
For example, entering `(30 20 *)` will return `600` without causing the stack to be printed 3 different times.
This also registers it as a single action that can be undone using `undo` (instead of 3 different actions).

## Builtin types

I'm piggybacking off of Common Lisp's numeric types: integer (aka bignum), rational (one bignum divided by another), double-precision float, and complex.
Operations typically preserve exactness if possible, but any irrational operations (such as `exp`, `sqrt`, and `sin`) will likely coerce the number into an inexact float.
Also, note that symbols are not case-sensitive - they will be automatically uppercased (thanks Common Lisp!).

## Functions
You can define new functions as a sequence of existing functions (or constants).
Note that the builtin functions/constants cannot be overridden.
Simply enter in `(def <your-fn-name> <body> ...)`.
For example, you can enter `(def c 3e8)` to define the speed of light, c, to be `3 * 10^8`.
In addition, you can define a function to calculate `x + 1/x` where `x` is the top stack element as follows: `(def foo dup inv +)`.
Now, running `5 foo` will return `26/5`.

## Variables
This calculator supports lexically-scoped variables.
Variables are always prefixed with a colon (like `:x`, for example).
You can store the topmost stack value into a variable (without popping it) by entering `(store <var-name>)`.
If you *do* want the top value to be popped before being stored, use `store!` instead of `store`..
Then, entering `:<var-name>` will put the value of the variable onto the stack.
If you store a value as part of the body of a function, the variable will not be accessible outside of the function's scope.

There is also a function `sto` which stores the top value of the stack into an unnamed global register (plus a popping alternative `sto!`).
The value of this register can be returned using `rcl`.
This register is useful for temporary values that you need to store but don't want to bother assigning to an actual variable.

Variables are a key part of functions.
You can declare a function with named arguments by entering `(def (<fn-name> <arg-name> ...) <body> ...)`.
When you run the function, each of the arguments will popped off the stack and stored into their named variable.
For example, here is an implementation of a function for calculating the roots of a quadratic (assuming the coefficients `a`, `b`, and `c` are on the stack).

```scheme
;; takes 3 values, leaves two roots on the stack
(def (quadratic a b c)
  :b 2 :a * / neg sto     ;; calculate -b/(2a)
  square :c :a / - sqrt   ;; get value under square root
  dup rcl - swap rcl +    ;; apply plus/minus with -b/(2a)
)
```

And also, yes, closures are possible (since functions establish a lexical scope).

```scheme
(def (counter x) (def count :x 1 + (store x)))

0 counter

count     ;; 1
count     ;; 2
count     ;; 3
```

## Quoting
You can push a symbol onto the stack by prepending it with a single quote.
For example, entering `'gcd` will push `gcd` onto the stack instead of immediately evaluating it.
To evaluate the top function on the stack, enter `eval`.

You can do the same thing with multiple elements at the same time.
For example, you can think of `'(2 *)` as a sort of "quoted function" that will double the number on the top of the stack when it gets evaluated.
Entering `2 '(2 *) eval` does the same thing as `2 2 *` (except maybe with a tiny bit more overhead).
As an example, consider the function `bi`, which takes one value and applies two quoted functions to it to produce two values.

```scheme
(def (bi x f1 f2)
  :x :f1 eval
  :x :f2 eval
)
```

Now, entering `10 '(2 *) '(2 +) bi` will result in `20` and `12` being on top of the stack.

## Conditionals

A basic conditional construct is the function `switch`, which takes three arguments.
If its third argument is "true", then it will evaluate to the second argument, otherwise the first.

For example:
```scheme
10 20 2 3 < switch  ;; evaluates to 20

10 20 false switch  ;; evaluates to 10
```

This works fine in certain circumstances, but it can be unwieldy.
Another option is to the `if` construct.
It takes three lists of instructions.
It will evaluate the first set of actions, check if the topmost element of the stack is true or false (which will be popped from the stack), then decide whether to run the *then* clause or the *else* clause.
Note that the *else* clause is optional.

```scheme
;; (if (<condition>) (<then>) [(<else>)])

123 (if (1 2 <) (2 *) (3 *)) ;; 246

(def (my-abs x)
  :x (if (dup minusp) (neg))
)

-30 my-abs ;; 30
20 my-abs  ;; 20
```

## Looping

By combining logic operators, quotation, and evaluation, you can express recursive functions and therefore looping.
This requires passing a function into itself, which is an idea borrowed from the [y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator).

```scheme
;; recursive factorial implementation
(def (fact self x acc)
  ;; recurse
  '(:self :acc :x * :x 1 - swap :self eval) 
  ;; return
  ':acc 
  ;; choose whether to recurse or return
  :x zerop switch eval 
)

'fact 10 1 fact ;; 3628800
```

However, this is extremely tedious to use.
You should probably use the `while` construct instead, which is very similar to `if`.

```scheme
;; (while (<condition>) (<body>))

(def (factorial x)
  ;; init loop counter
  :x sto drop 
  ;; initial value/accumulator
  1 
  (while (rcl plusp) ;; while x is positive
    ;; update accumulator
    rcl *
    ;; decrement loop counter
    rcl dec sto drop)
)
```
## Loading

A basic file loading mechanism is provided as follows: `(load <file-path>)`.
This will execute all of the code in `<file-path>`.
Note that if an error occurs while loading a file, loading will immediately stop, and any changes to the stack will be reverted (though function definitions may remain).

```scheme
;; In double.zpc
(def double dup +)

;; In REPL
(load double.zpc)

20 double ;; 40
```

## Packages

A package is a namespace where functions are located.
Packages are only for organization - they aren't meant to provide encapsulation or other OOP-like abstractions.
The name of the currently "active" package is shown in the top left of the stack.
Every time you use `def`, the function you define will become part of the current package.
Note: variables, on the other hand, are part of a global top-level scope - they are not associated with packages in any way.

The default package is named USER, and it is automatically entered when you start the calculator.
You can enter a different package with `(in-package <name>)`.
If a package of that name does not already exist, it will be automatically created and then entered.

You can access functions in a different package with the dot operator.
For example, running `foo.bar` will cause the calculator to look for a function named `bar` in the package `foo`.
For the sake of ambiguity, neither functions nor packages can have the character `"."` in their name.
In addition, note that functions from a different package cannot be redefined from the current package.
As a result, `(def foo.bar 10)` is simply illegal (you can't define a function named `foo.bar`, nor can you define a function named `bar` in `foo` - you would have to enter `foo` and then define `bar` from there).

Builtin functions are located in BUILTINS and can therefore be accessed like so: `2 2 builtins.+`.
However, `2 2 +` will also work, since builtins are treated as implicitly being part of every package.
The BUILTINS package cannot be entered, so builtin operations can never be redefined.
You can, however, define functions in the current package with the same name as a builtin function.
This is not recommended, but it may be useful in specific circumstances.
If you ever need to reset the definition of a function with the same name as a builtin, entering `(def + builtins.+)` will suffice.

Here is an example of how packages can be used.

```lisp
(in-package foo)

(def (bar x y) (:x :x * :y +))

(in-package user)

(10 20 foo.bar) ;; 120
```

If you would like to enter a package only temporarily, you can use the `with-package` construct.
It will run code within a package and then return to the previous package.

```lisp
(with-package foo
  (def bar dup *)
)

;; still in current package
10 foo.bar ;; 100
```

## Structs

Custom data types can be created by conjoining elements into a *struct*.
Structs are simply Common Lisp vectors (resizeable arrays).
Structs can be pushed directly onto the stack using `#(a b c)` syntax (where a, b, and c are the elements in the struct).
Additionally, you can create a struct from the top `n` elements of the stack using `n make-struct`.
You can also "unmake" a struct, which will cause all of its elements to be pushed onto the stack, along with its length.

To get an element out of a struct, provide a one-based index and use `elt`.
For example, `#(a b c) 2 elt` will result in `b`.
You can get the number of elements in a struct with `size`.

## Putting it all together

Here is a simple implementation of lazy integer ranges with the ZPCalc language (also accessible in `std/range.zpc`).

```
(with-package range

(def make 2 make-struct)

(def start 1 elt)
(def end 2 elt)

;; produces the next value in the range along with the updated range
(def (next r)
  :r range.start
  (if (:r range.start :r range.end <)
    (dup inc :r range.end range.make))
)

;; expands out an entire range, pushing all of its elements onto the stack
(def (expand r)
  :r range.start
  (while (dup :r range.end <)
    dup inc)
)

;; reduces a range r given an initial value i and a binary operator f
(def (reduce r i f)
  :i
  :r range.start (store! i)
  (while (:i :r range.end <= )
    :i :f eval
    :i inc (store! i)
  )
)

;; repeats a function for each value in a range
;; the current index i is provided as an argument to the function
(def (for r f)
  :r range.start (store! i)
  (while (:i :r range.end <= )
    :i :f eval
    :i inc (store! i)
  )
)

)
```

Here are some ways you can use this range library:

```
;; generating lots of numbers
(1 100 range.make range.expand) ;; 1 2 3 4 ... 100

;; calculating factorials
(1 10 range.make 1 '* range.reduce) ;; 3628800

;; generate all prime numbers less than 100
(1 100 range.make
  '(dup (if (primep not) (drop)))
  range.for)
```

## Builtins

### Stack Manipulation
- `drop` (or `pop`) - removes the top element of the stack
- `dup` - duplicates the top element of the stack
- `swap` - swaps the top two stack elements
- `rot` - rotates the top 3 elements (moves the top element to 3rd)
- `-rot` - does the opposite of rot
- `roll` - rotates the entire stack (the top element becomes the bottom)
- `-roll` - does the opposite of roll (the bottom element becomes the top)

### Basic Numeric Operations
- `+` - adds the top two numbers on the stack
- `-` - subtracts the top two numbers on the stack
- `*` - multiplies the top two numbers on the stack
- `/` - divides the top two numbers on the stack
- `//` - divides the top two numbers on the stack, truncating the result into an integer (aka integer division)
- `_` (or `neg`) - negates the top element of the stack
- `inc` - adds 1 to the topmost stack element
- `dec` - subtracts 1 from the topmost stack element
- `inv` - replaces the top element of the stack with its reciprocal

### Additional Numeric Operations
 - `max` - returns the maximum of the top two stack elements
 - `min` - returns the minimum of the top two stack elements
 - `gcd` - returns the greatest common denominator of the top two stack elements
 - `lcm` - returns the least common multiple of the top two stack elements
 - `abs` - returns the absolute value of the top stack element
 - `signum` - returns -1, 0, or 1 depending on the top stack element if negative, zero, or positive
 - `floor` - returns the floor of the top stack elements
 - `ceiling` - returns the ceiling of the top stack elements
 - `truncate` - removes the fractional component of the top stack element
 - `round` - rounds the top stack element
 - `mod` - computes the modulo of the top two stack elements
 - `rem` - computes the remainder of the top two stack elements
 - `random` - returns a random integer between 0 (inclusive) and the top stack element (exclusive)
 - `rand` - returns a random float between 0 and 1
 - `isqrt` - returns the integer square root (rounded down) of the top stack element (which must be an integer)
 - `fib` - returns the nth fibonacci number, where n is the top stack element
 - `fact` - returns the factorial of the top stack element
 - `prime` - returns the nth prime number, where n is the top stack element
 - `primep` - returns 1 if the top stack element is prime, otherwise 0
 - `totient` - returns phi of the top stack element (also known as the totient function)
 - `choose` - returns n choose k, where n and k are on top of the stack (also known as the binomial coefficient)
 - `permute` - returns n permute k, where n and k are on top of the stack

### Irrational Operations
 - `pow`  - returns a^b, where a and b are the top two stack elements. Tries to preserve exactness
 - `invpow`  - returns a^(1 / b), where a and b are the top two stack elements. Tries to preserve exactness
 - `sqrt` - returns the square root of the top stack element. Tries to preserve exactness
 - `log` - returns a log b, where a and b are the top two stack elements. Tries to preserve exactness
 - `lg` - returns the base 2 log of the top stack element. Tries to preserve exactness
 - `log10` - returns the base 10 log of the top stack element. Tries to preserve exactness
 - `exp` - returns e raised to the top stack element
 - `ln` - returns the natural log of the top stack element
 - `sin` - returns the sin of the top stack element
 - `cos` - returns the cos of the top stack element
 - `tan` - returns the tan of the top stack element
 - `asin` - returns the inverse sin of the top stack element
 - `acos` - returns the inverse cos of the top stack element
 - `atan` - returns the inverse tan of the top stack element
 - `atan2` - returns the inverse tan of a/b, where and b are the top two stack elements
 - `cis` - returns the cis of the top stack element
 - `sinh` - returns the hyperbolic sin of the top stack element
 - `cosh` - returns the hyperbolic cos of the top stack element
 - `tanh` - returns the hyperbolic tan of the top stack element
 - `asinh` - returns the inverse hyperbolic sin of the top stack element
 - `acosh` - returns the inverse hyperbolic cos of the top stack element
 - `atanh` - returns the inverse hyperbolic tan of the top stack element

### Type-based Operations
 - `float` - converts the top stack element into a float
 - `rational` - converts the top stack element into a rational
 - `numerator` - if the top element is rational, returns its numerator
 - `denominator` - if the top element is rational, returns its denominator
 - `complex` - constructs the complex number a+bi where a and b are the two top stack elements
 - `conjugate` - if the top element is complex, return its conjugate
 - `phase` - if the top element if complex, return its phase (aka argument)
 - `realpart` - if the top element is complex, return its real part
 - `imagpart` - if the top element is complex, return its imaginary part

### Bitwise Operations (integers only, assuming two's complement representation)
 - `lnot` - returns the bitwise not of the top two stack elements
 - `land` - returns the bitwise and of the top two stack elements
 - `lor` - returns the bitwise or of the top two stack elements
 - `lxor` - returns the bitwise xor of the top two stack elements
 - `lnand` - returns the bitwise nand of the top two stack elements
 - `lnor` - returns the bitwise nor of the top two stack elements
 - `bit` - returns the nth bit of an integer x, where x and n are the two top stack elements
 - `<<` - returns the bitwise left shift a << b, where a and b are the top two stack elements
 - `>>` - returns the bitwise right shift a >> b, where a and b are the top two stack elements

### Logic Operations (where 0 is "false" and everything else is "true")
 - `not` - returns 0 if top element is true, 1 otherwise
 - `and` - returns 1 if the top two elements are both true, 0 otherwise
 - `or` - returns 0 if the top two elements are false, 1 otherwise
 - `xor` - returns 0 if the top two elements are either both true or both false, 1 otherwise
 - `nand` - returns the 0 if the top two elements are both true, 1 otherwise
 - `nor` - returns 1 if the top two elements are false, 0 otherwise
 - `switch` - if the top element is true, then the second top element, otherwise the third
 - `truep` - returns 1 if the top element is truthy, otherwise 0 (this should do nothing)
 - `falsep` - returns 1 if the top element is falsy, otherwise 0 (this should do the same as `zerop`)
 - `zerop` - returns 1 if the top element is 0, otherwise 0 (this should do the same as `falsep`)
 - `plusp` - returns 1 if the top element is positive, otherwise 0
 - `minusp` - returns 1 if the top element is negative, otherwise 0
 - `evenp` - returns 1 if the top element is even, otherwise 0
 - `oddp` - returns 1 if the top element is odd, otherwise 0
 - `>` - returns 1 if a > b, where a and b are the top two stack elements, otherwise 0
 - `>=` - returns 1 if a >= b, where a and b are the top two stack elements, otherwise 0
 - `<` - returns 1 if a < b, where a and b are the top two stack elements, otherwise 0
 - `<=` - returns 1 if a <= b, where a and b are the top two stack elements, otherwise 0
 - `=` - returns 1 if a = b, where a and b are the top two stack elements, otherwise 0
 - `approx` - returns 1 if the float of a is approximately equal to the float of b, where a and b are the top two stack elements, otherwise 0

### Structures (see [Structs](#structs))
 - `make-struct` - returns a struct made from the top `n` elements of the stack
 - `unmake-struct` - "unmakes" the top element on the stack
 - `elt` - returns an element from a stack given an index
 - `size` - returns the number of elements in a stack

### Constants (more to come)
 - `pi`, `e`, `phi`, `i`, `true`, `false`

### Misc Functions
 - `clear` - clears the stack
 - `eval` - tries to "execute" the topmost value on the stack (see [Quoting](#quoting)).
 - `sto` - stores the top stack value into a global, unnamed register (without a pop)
 - `sto!` - stores the top stack value into a global, unnamed register (with a pop)
 - `rcl` - recalls the value stored in the global, unnamed register onto the stack

### Special Constructs
 - `(def <name-or-args> <body>...)` - creates a user-defined function (see [Functions](#functions))
 - `(store <var>)` - stores the top stack element into a named variable without popping it (see [Variables](#variables))
 - `(store! <var>)` - stores the top stack element into a named variable, popping it at the same time (see [Variables](#variables))
 - `(if (<condition>) (<then>) (<else>))` - a conditional construct that allows for branched execution (see [Conditionals](#conditionals))
 - `(while (<condition>) <body>...)` - a construct that allows for looping (see [Looping](#looping))
 - `(in-package <package>)` - enters a package (see [Packages](#packages))
 - `(with-package <package>)` - executes code within a package (see [Packages](#packages))
 - `(repeat <n> <body>...)` - executes the `body` `n` times.

### Top-Level Actions (cannot be evaluated)
 - `quit` - quits the calculator
 - `undo` - tries to undo the last operation. You can undo any number of times.
 - `redo` - tries to redo the last undo. If you undo and then make a change to the stack, you can no longer "redo" back to the previous state.
 - `load` - executes all of the forms within a file (see [Loading](#loading))

## Addendum

Float precision is annoying... `2 2 sqrt square =` returns `0` :(

If you would like to read about my software design thought process, check out this blog post [here](https://www.zwhuang.dev/blog/monads_useful), where I explain why I chose to use the state monad as the core computational unit for this calculator.
