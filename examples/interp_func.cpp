//
// This example showcases usage of headerlisp for writing ad-hoc parsers.
//
#include "headerlisp.h"

#include <stdio.h>

namespace hl = headerlisp;

// Define type hierarchy for expressions that will be used inside interpreter
struct Node {
    virtual int eval() = 0;
};
struct Num : Node {
    int x;
    Num(int x) : x(x) {}
    int eval() override { return x; }
};
struct Add : Node {
    Node *left, *right;
    Add(Node *left, Node *right) : left(left), right(right) {}
    int eval() override { return left->eval() + right->eval(); }
};
struct Minus : Node {
    Node *arg;
    Minus(Node *arg) : arg(arg) {}
    int eval() override { return -arg->eval(); }
};

// Recursive s-expression traversal that builds tree of Nodes.
Node *parse_tree(hl::value it) {
    if (hl::is_num(it))
        return new Num{hl::as_num_int(it)};

    std::string_view label = hl::as_string_view(hl::first(it));
    if (label == "-")
        return new Minus(parse_tree(hl::second(it)));
    if (label == "+")
        return new Add(parse_tree(hl::second(it)), parse_tree(hl::third(it)));
    fprintf(stderr, "invalid syntax\n");
    exit(1);
}

// Convert string to interpreter tree.
Node *parse(std::string_view input) {
    hl::context_guard ctx{};
    hl::value ast;
    try {
        // Parse the string into s-expression
        ast = hl::read_function(input);
    } catch (hl::hl_exception &e) {
        fprintf(stderr, "reading error: %s\n", e.what());
        exit(1);
    }
    try {
        // Parse s-expression into interpreter tree.
        return parse_tree(ast);
    } catch (hl::hl_exception &e) {
        fprintf(stderr, "parsing error: %s\n", e.what());
        exit(1);
    }
}

int main() {
    std::string input = "+(+(2, 7), -(5))";
    Node *tree = parse(input);
    printf("result is %d\n", tree->eval());
    return 0;
}