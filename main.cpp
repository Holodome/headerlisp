#include "headerlisp.h"

#include <iostream>
#include <stdlib.h>

using namespace headerlisp;

struct Person {
    std::string name;
    std::string address;
    int age;
};

template <> struct headerlisp::to_list<Person> {
    auto operator()(const Person &person) { return list(person.name, person.address, person.age); }
};

template <> struct headerlisp::from_list<Person> {
    Person operator()(value lst) {
        Person person;
        person.name = std::string{as_string_view(first(lst))};
        person.address = std::string{as_string_view(second(lst))};
        person.age = as_num_int(third(lst));
        return person;
    }
};

int main() {
    set_context(context{(char *)malloc(1 << 20), 0, 1 << 20});

    Person p1{"John", "Here", 23};
    Person p2{"Adam", "There", 32};
    auto lst = list(p1, p2);
    std::string serialized = print(lst);
    std::cout << serialized << "\n";
    auto deserialized = read(serialized);
    for (Person &p : deserialized.iter().as<Person>()) {
        std::cout << std::format("Person(name={},address={},age={})\n", p.name, p.address, p.age);
    }
}

void other_test() {
    auto x = list(1, 2, list("hello", 2));
    std::cout << print(x) << "\n";
    auto y = map([](auto x) { return is_num(x) ? x * 2 : x; }, x);
    std::cout << print(y) << "\n";
    for (auto it : reverse(y).iter()) {
        std::cout << print(it) << "\n";
    }
}