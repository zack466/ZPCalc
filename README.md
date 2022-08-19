# rpncalc - a RPN calculator written in Common Lisp

## Installation

Requirements: sbcl (or ecl)

1. Git clone this repository and `cd` into it.
2. Run `make rpncalc`.
3. Now, you have a static executable called `rpncalc`! Simply run with `./rpncalc`.

## How to Use

It's a simple [RPN](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator.
Type a number and then press enter to push it on to the stack.
You can execute a function by typing its name.
For example, if you have two numbers on the stack and you enter "+", you will get the sum of those two numbers.
For a list of builtin functions, see below.
Quit by entering `quit` or sending EOF with ctrl-d.

## Builtin datatypes

I'm piggybacking off of Common Lisp's datatypes: integer (aka bignum), rational (one bignum divided by another), double-precision float, and complex.
Operations typically preserve exactness if possible, but any irrational operations (such as `exp`, `sqrt`, and `sin`) will coerce the number into an inexact float.

### Builtin functions

#### Stack Manipulation
- `drop` (or `pop`) - removes the top element of the stack
- `dup` - duplicates the top element of the stack
- `swap` - swaps the top two stack elements
- `rot` - rotates the top 3 elements

#### Basic Numeric Operations
- `+` - adds the top two numbers on the stack
- `-` - subtracts the top two numbers on the stack
- `*` - multiplies the top two numbers on the stack
- `/` - divides the top two numbers on the stack
- `//` - divides the top two numbers on the stack, truncating the result into an integer (aka integer division)
- `_` (or `neg`) - negates the top element of the stack
- `inv` - replaces the top element of the stack with its reciprocal

#### Additional Numeric Operations
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
 - `isqrt` - returns the integer square root (rounded down) of the top stack element
 - `random` - returns a random integer between 0 (inclusive) and the top stack element (exclusive)
 - `rand` - returns a random float between 0 and 1

#### Irrational Operations (will always result in a float)
 - `exp` - returns e raised to the top stack element
 - `expt` (or `pow`) - returns a^b, where a and b are the top two stack elements
 - `ln` - returns the top stack element log e
 - `lg` - returns the top stack element log 2
 - `log10` - returns the top stack element log 10
 - `log` - returns a log b, where a and b are the top two stack elements
 - `sqrt` - returns the square root of the top stack element
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

#### Type-based Operations
 - `float` - converts the top stack element into a float
 - `rational` - converts the top stack element into a rational
 - `numerator` - if the top element is rational, returns its numerator
 - `denominator` - if the top element is rational, returns its denominator
 - `complex` - constructs the complex number a+bi where a and b are the two top stack elements
 - `conjugate` - if the top element is complex, return its conjugate
 - `phase` - if the top element if complex, return its phase (aka argument)
 - `realpart` - if the top element is complex, return its real part
 - `imagpart` - if the top element is complex, return its imaginary part

#### Logic Operations (where 0 is "false" and everything else is "true")
 - `not` - returns the bitwise not of the top two stack elements
 - `and` - returns the bitwise and of the top two stack elements
 - `or` - returns the bitwise or of the top two stack elements
 - `xor` - returns the bitwise xor of the top two stack elements
 - `nand` - returns the bitwise nand of the top two stack elements
 - `nor` - returns the bitwise nor of the top two stack elements
 - `bit` - returns the nth bit of an integer x, where x and n are the two top stack elements
 - `<<` - returns the bitwise left shift a << b, where a and b are the top two stack elements
 - `>>` - returns the bitwise right shift a >> b, where a and b are the top two stack elements
 - `if` - if the top element is true, then the second top element, otherwise the third
 - `zerop` - returns 1 if the top element is 0, otherwise 0
 - `onep` - returns 1 if the top element is 1, otherwise 0
 - `plusp` - returns 1 if the top element is positive, otherwise 0
 - `minusp` - returns 1 if the top element is negative, otherwise 0
 - `evenp` - returns 1 if the top element is even, otherwise 0
 - `oddp` - returns 1 if the top element is odd, otherwise 0
 - `>` - returns 1 if a > b, where a and b are the top two stack elements, otherwise 0
 - `>=` - returns 1 if a >= b, where a and b are the top two stack elements, otherwise 0
 - `<` - returns 1 if a < b, where a and b are the top two stack elements, otherwise 0
 - `<=` - returns 1 if a <= b, where a and b are the top two stack elements, otherwise 0
 - `=` - returns 1 if a = b, where a and b are the top two stack elements, otherwise 0

#### Constants (more to come)
 - `pi`, `e`, `phi`, `i`

#### Special Functions
 - `quit` - quits the calculator
 - `clear` - clears the stack
 - `undo` - tries to undo the last operation. You can undo any number of times.
 - `redo` - tries to redo the last undo. If you undo and then make a change to the stack, you can no longer "redo" back to the previous state.
 - `eval` - tries to execute the function associated with the symbol on the top of the stack

## Defining your own functions
You can define a new function to be a sequence of existing functions (or constants).
Note that the builtin functions cannot be overriden.
Simply enter in `(def <your-fn-name> <thing1> <thing2> ...)`.
For example, we can define a function to calculate `x + 1/x` where `x` is the top stack element as follows: `(def foo dup inv +)`.
Now, running `10 foo` will return `101/10`.

## Advanced input strategies
If you enter multiple tokens, they will all be processed sequentially.
For example, typing `30 20 * <return>` will result in `600` (it's the same as doing `30 <return> 20 <return> * <return>`).

You can push a symbol onto the stack by prepending it with a single quote.
For example, entering `'gcd` will push `gcd` onto the stack instead of immediately evaluating it.
To evaluate the top function on the stack, enter `eval`.

## Addendum

Float precision makes me sad :(
