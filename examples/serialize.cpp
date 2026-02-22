//
// This example showcases usage of headerlisp as a serialization and deserialization tool.
//
#include "headerlisp.h"

namespace hl = headerlisp;

// Define the type we will serialize and deserialize.
struct Person {
    std::string name;
    std::string address;
    int age;
};

// Implementation of `to_list` trait that tells how to serialize Person.
template <> struct hl::to_list<Person> {
    auto operator()(const Person &person) { return list(person.name, person.address, person.age); }
};

// Implementation of `from_list` trait that tells how to deserialize Person.
template <> struct hl::from_list<Person> {
    Person operator()(value lst) {
        auto [a, b, c] = first_3(lst);
        Person person;
        person.name = as_string_view(a);
        person.address = as_string_view(b);
        person.age = as_num_int(c);
        return person;
    }
};

int main() {
    hl::context_guard ctx{};

    Person p1{"John", "Here", 23};
    Person p2{"Adam", "There", 32};
    // hl::list automatically serializes its arguments. This can be done manually using hl::make_value
    auto lst = hl::list(p1, p2);
    std::string serialized = hl::print(lst);
    printf("serialized = %s\n", serialized.c_str()); // (("John" "Here" 23) ("Adam" "There" 32))

    auto deserialized = hl::read(serialized);
    // Iterators are used to deserialize a list of some type.
    for (Person p : deserialized.iter().as<Person>()) {
        printf("Person(name=%s,address=%s,age=%d)\n", p.name.c_str(), p.address.c_str(), p.age);
    }
}