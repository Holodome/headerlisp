//
// This example showcases usage of headerlisp's list processing abilities.
//
#include "headerlisp.h"

#include <stdio.h>

using namespace headerlisp;

int main() {
    context_guard ctx{};

    // map
    value lst = list(1, 2, 3);
    lst = map([](value x) { return x * 2; }, lst);
    printf("%s\n", print(lst).c_str()); // (2 4 6)

    // filter
    lst = list(1, 2, 3);
    lst = filter(is_odd, lst);
    printf("%s\n", print(lst).c_str()); // (1 3)

    // fold
    lst = list(1, 2, 3);
    int sum = foldl([](value x, int acc) { return as_num_int(x) + acc; }, 0, lst);
    printf("sum is %d\n", sum);

    // homogenous list iteration
    lst = list("hello", "world", "!");
    for (std::string_view it : lst.iter().as<std::string_view>()) {
        printf("%.*s\n", (int)it.length(), it.data());
    }

    // heterogenous list
    lst = list(1, 3.5, "hello", list(10, 20), cons(make_value("car"), make_value("cdr")), true, nullptr);
    printf("%s\n", print(lst).c_str()); // (1 3.5 "hello" (10 20) ("car" . "cdr") t ())

    for (auto it : lst.iter()) {
        if (is_cons(it) && is_list(cdr(it))) {
            printf("list length=%zu values=%s\n", length(it), print(it).c_str());
        } else if (is_cons(it)) {
            printf("cons car=%s cdr=%s\n", print(car(it)).c_str(), print(cdr(it)).c_str());
        } else if (is_num(it)) {
            printf("num %g\n", as_num_f64(it));
        } else if (is_true(it)) {
            printf("true\n");
        } else if (is_string(it)) {
            std::string_view sv = as_string_view(it);
            printf("string '%.*s'\n", (int)sv.length(), sv.data());
        } else if (is_nil(it)) {
            printf("nil\n");
        }
    }

    return 0;
}