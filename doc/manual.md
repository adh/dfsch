dfsch is programming language inspired by Scheme and C-based
implementation of said language. Although dfsch is heavily inspired by
Scheme, language design tries to stress practical usability and rapid
development (of both implementation and user code) instead of Scheme's
theoretical foundations. In some rather significant aspects, dfsch
draws inspiration from Common Lisp.

This documentation tries to explain and document whole language, but
in it's current state probably contain parts that are not
understandable without at least passing familiarity with Scheme and or
Common Lisp.

# Syntax

As dfsch is one of many dislects of Lisp, it uses parenthesis based
syntax. Source code read from text files on disk is converted into
in-memory representation consisting of lists and other normal user
accessible objects. Runtime behavior of programs does not directly
depend on program source code, but on this in memory representation
and will be therefore described in that terms in following
chapters. This chapter describes rules used in construction of this in
memory structures when reading source code as well as syntax of
literal data.

Source code essentially describes tree of in memory objects. Some of
these objects can contain references to other objects, other are only
atomic values.

## Numbers

## Symbols

Any space delimited string that does not conform to syntax of numbers
is parsed as so called symbol (|<symbol>|). Symbols are unique named objects that
are used as names. Most notably, symbols name variables.

Each symbol belong into so called package (<package>). Part of symbol
representation up to first colon names package of that symbol. When
symbol name begins with colon, it's so called keyword (symbol in
package named keyword), which is special case of symbols. When package
name is absent from symbol name, so called current package and
packages used by it are searched for symbol with that name and finally
if no such symbol exists, it's created in current package.

For example, these are valid symbols:

    foo
    with-open-file
    1+
    dfsch%implementation:%loop
    :has-argument?
    &optional

## Strings

Strings (|<string>|) are delimited by double quotes. Backlash can be used to escape
quote characters insode string, as well as for some C-style escape
sequences. String literals can contain any characters including
newlines and other control characters. All strings are UTF-8 encoded
and immutable.

    "this is a string"
    "this: \"is also a string\""

String literal prefixed with # encodes so called
byte-vector (|<byte-vector>|). Bytevectors are like strings, but consist of bytes and
not characters and can be modified.

## Lists

List (|<list>|) is one of most important objects in dfsch source code. It's
representation consists of multiple objects separated by spaces inside
parenthesis. For example:

    (1 2 3)
    (define pi 3.141592)
    (+ 3 4 (* 5 6))
    ((:a 1) (:b 2) (:c 3))

Internally, lists are construed from so called pairs that are linked
together by their second element (|cdr|). Pairs can be directly
written as two objects in parenthesis separated by single dot:

    ( car . cdr )

## Vectors

While vectors (|<vector>|) are syntactically similar to lists, they are very
different kind of object. Vectors are arrays of objects references,
because of that, they can be accessed in constant time, but cannot be
resized once created. They are represented by parenthesised list of
objects prefixed by #.

    #(1 2 3)
    #(2 (foo) 4 5)

In contrast to Scheme, vectors in dfsch are self-evaluating atomic
objects and thus valid when present unquoted in program source.

# Program structure

As previously noted, program code is represented by list structure, in
this structure, each object represents some expression to be
evaluated. All expressions evaluate to some value - there is no
difference between statements and expressions.

Objects of most types evaluate directly to themselves. Two important
exceptions are non-empty lists and symbols. Symbols represent
variables and are thus evaluated to value of relevant variable.

## Compound expressions

Non-empty lists represent all other useful expressions. At first,
first element of such list is evaluated as expression. Resulting value
can be of multiple types:

 * Function or other object than can be used as function
 * Macro (|<macro>|)
 * Special form (|<form>|)

Rest of such list is used as arguments to object resulting from such
evaluation.

### Function calls

In case of function call, all arguments are evaluated first (in
unspecified order) and then passed to relevant function. For example:


    ]=> (+ 1 2)
    3
    ]=> (* 2 (+ 3 4))
    14

### Macro expansions

Macro is special case of function that operates on program source code
during compilation. Arguments are passed without any modification into
this function which can process them in any way it wants. Result of
this function is then executed as program code instead of original
expression.

Many control structures are actually implemented as macros that expand
into simpler representations.

### Special forms

Some control structures have to be implemented by special cases in
interpreter and compiler. Such structures are represented by so called
special forms. As is the case with macros, exact behavior is
completely dependent on implementation of special form.

## Defining functions and variables

Special form |define| allows you to define new variables. For example:

    (define pi 3.141592)
    (define sound "nyan!")

Also, you can use this special form to define functions:

    (define (speak)
      (display "nyan!")
      (newline))

Functions are simplest construct that can be used to organize programs
into reusable components and thus are very important. In contrast to
many commonly used programming languages, functions are not only parts
of program, but also usable values that can be passed around. Special
form |lambda| evaluates to function object, which does not have any
name. For example this function adds 3 to it's argument.

     (lambda (x) (+ 3 x))

dfsch supplies many built-in functions, that are parametrized by
function that is passed to them as argument. For example |map|:
     
    ]=> (map (lambda (x) (+ 3 x)) '(1 2 3 4))
    (4 5 6 7)

As functions are also values, there is no difference between variable
naming function and any other variable. Previously shown function
definition using |define| is essentially equivalent to this code:

    (define speak
      (lambda ()
        (display "nyan!")
        (newline)))

There are some important differences, but they are more relevant to
implementation of dfsch than to program meaning.

For variables directly defined by user, two additional forms are
avaiable:

    (define-variable *foo* 123)
    (define-constant +bar+ 456)
    
In both cases, variable is defined when it does not already exist (in
contrast to |define|, which would overwriteit). |define-constant| also
signals to compiler, that you do not intend to modify this
variable. Symbols like \* and \+ around such variable names are
traditionally used to distinguish global variables and constants from
other names.

# Collections, sequences and mappings

# Strings

# Numbers

# Input and output

# Objects and types
