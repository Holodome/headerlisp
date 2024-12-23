#include "headerlisp.h"

#include <iostream>
#include <stdlib.h>

using namespace headerlisp;

int main() {
    set_context(context{(char *)malloc(1 << 20), 0, 1 << 20});

    auto x = read("(1 2 3 4)");
    std::cout << print(x) << "\n";
    auto y = map([](auto x) { return x * 2; }, x);
    std::cout << print(y) << "\n";
}