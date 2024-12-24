//
// This example showcases usage of headerlisp as a serialization and deserialization tool.
//
#include "headerlisp.h"

namespace hl = headerlisp;

struct Person {
    std::string name;
    std::string address;
    int age;
};

template <> struct hl::to_list<Person> {
    auto operator()(const Person &person) { return list(person.name, person.address, person.age); }
};

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
    auto lst = hl::list(p1, p2);
    std::string serialized = print(lst);
    printf("serialized = %s\n", serialized.c_str()); // (("John" "Here" 23) ("Adam" "There" 32))

    auto deserialized = hl::read(serialized);
    for (Person &p : deserialized.iter().as<Person>()) {
        printf("Person(name=%s,address=%s,age=%d)\n", p.name.c_str(), p.address.c_str(), p.age);
    }
}