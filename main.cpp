#define HL_IMPLEMENTATION
#include "headerlisp.h"

using namespace headerlisp;

int main() {
    auto x = read(make_value("(1 2 3 4)"));
    auto y = filter(is_nil, map([](auto x) { return x * 2; }, x));
    print2s(y);
}