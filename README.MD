# headerlisp

Lisp in C++.

More fundamentally, this is dead-simple dynamically-typed tool for processing lists inside C++.

```cpp
auto lst = list(1, 2, 3, list("hello, world"));
     lst = map([](auto x) { return is_num(x) ? x * 2 : x; }, lst);
std::string result = print(lst); // (2, 4, 6, ("hello, world"))
```

## Motivation

There are three main use cases for headerlisp:

1. *Parsers* - Instead of using [lex](https://en.wikipedia.org/wiki/Lex_(software)) to write custom parser, lisp s-expressions can be used to define syntax. Headerlisp allows parsing s-expressions and traversing them.
2. *DSLs* - Custom parsers allow easy writing of DSLs for different tasks, where using anything more complex would be too time-consuming.
3. *Quick serialization/deserialization* - Headerlisp has abilities to read and write user-defined into and from s-expression trees. This can be used to implement ad-hoc serialization.

## Examples

1. [interp.cpp](examples/interp.cpp) - headerlisp used to implement parser for expression interpreter
2. [processing.cpp](examples/interp.cpp) - showcase of list processing functions (`map`, `filter`, iteration etc.)
3. [serialize.cpp](examples/serialize.cpp) - using headerlisp as serialization tool and format
4. [wire.cpp](examples/wire.cpp) - using headerlisp as a format for passing commands between threads

## Design decisions

1. Supported types:
    - cons pair 
    - number - double
    - true `t`
    - nil `()`
    - string 
2. Values are stored and accessed using Nan-Boxing. Only strings and conses are heap-allocated.
3. There is no garbage collector. All memory is allocated within pre-allocated memory context, that can be changed using `headerlisp::set_context` or `headerlisp::context_guard`
    ```cpp
    {   
        headerlisp::context_guard ctx{};
        // Do list processing
        // ...
        // All memory is freed here
    }
    ```
4. All lists are implemented as cons pairs. There are no optimizations to use constant-time access. Type checking is at every API call.
5. User-defined types are supported using traits. 
6. Exceptions are used to report errors.
7. Strings are always copied inside headerlisp memory context.