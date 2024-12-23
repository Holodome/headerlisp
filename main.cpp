#include "headerlisp.h"

#include <iostream>
#include <stdlib.h>

using namespace headerlisp;

int main() {
    set_context(context{(char *)malloc(1 << 20), 0, 1 << 20});

    auto x = read("(1 2 3 \"aboba\")");
    std::cout << print(x) << "\n";
    auto y = map([](auto x) { return is_num(x) ? x * 2 : x; }, x);
    std::cout << print(y) << "\n";
    for (auto it : reverse(y).range()) {
        std::cout << print(it) << "\n";
    }
}