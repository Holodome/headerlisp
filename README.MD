# headerlisp

Lisp in C++.

More fundamentally, this is dead-simple dynamically-typed tool for processing lists inside C++.

```cpp
auto lst = list(1, 2, 3, list("hello, world"));
     lst = map([](auto x) { return is_num(x) ? x * 2 : x; }, lst);
std::string result = print2s(lst); // (2, 4, 6, ("hello, world"))
```

## Motivation

There are two main use cases for headerlisp:

1. DSLs
2. Quick serialization/deserialization

### DSLs

In some cases, when quickly prototyping something on C++, I face a need to parse user input and produce internal data structures.
This would require writing a separate parser program, which is tedious for simple cases.
Instead we can write the DSL as a S-expression and interpret it to get required structure.

Suppose we have tree-like in-memory data strcture that we want to able to read from user:

```cpp
class Node{};
class Int : public Node { Int(int x); };
class Sum : public Node { Sum(Node *left, Node *right); }
class Mul : public Node { Mul(Node *left, Node *right); }

std::string user = "(+ (* 2 3) 8)";
auto ast = read(user);

Node *parse_node(auto ast) {
    if (is_list(ast)) {
        auto func = first(ast);
        if (func == "+") {
            return new Sum(parse_node(second(ast), third(ast)));
        }
        if (func == "*") {
            return new Mul(parse_node(second(ast), third(ast)));
        }
    }
    return new Int(ast.to_int());
}

Node *result = parse_node(ast);

```

In just a few lines of code we were able to write interpreter for S-expressions, allowing easy writing of DSLs.

### Serialization

Because C++ lacks proper metaprogamming, serialization and deserialization is either done by hand or using special tools. 
In any case it is very verbose.

We adopt approach from [JSON for Modern C++](https://github.com/nlohmann/json) to serialize and deserialize used-defined types.

```cpp
struct Person {
    std::string name;
    std::string address;
    int age;  
};

void to_list(auto &lst, const Person &person) {
    lst = list(person.name, person.address, person.age);
}
void from_list(const auto &lst, Person &person) {
    person.name = first(lst).to_string();
    person.address = second(lst).to_string();
    person.age = third(lst).to_int();
}

Person p1 { "John", "Here", 23 };
Person p2 { "Adam", "There", 32 };
auto lst = list(p1, p2);
std::string serialized = print(lst); // (("John", "Here", 23), ("Adam", "There", 32))
auto deserialized = read(serialized);
for (Person p : deserialized.iter().as<Person>()) {
    // ...
}

```