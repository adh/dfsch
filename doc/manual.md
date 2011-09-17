dfsch is programming language inspired by Scheme and C-based
implementation of said language. Although dfsch is heavily inspired by
Scheme, language design tries to stress practical usability and rapid
development (of both implementation and user code) instead of Scheme's
theoretical foundations. In some aspects, dfsch draws inspiration from
Common Lisp.

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
is parsed as so called symbol. Symbols are unique named objects that
are used as names. Most notably, symbols name variables.

Each symbol belong into so called package. Part of symbol
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

