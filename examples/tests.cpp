#include <algorithm>
#include <cassert>
#include <cstdio>
#include <list>
#include <memory>
#include <numeric>
#include <optional>
#include <string>
#include <vector>

#include "headerlisp.h"

using namespace headerlisp;

static context_guard guard{1 << 24};

// ----- Simple test harness -----
static int tests_run = 0;
static int tests_failed = 0;
static std::vector<std::string> failures;

#define TEST(cond, msg)                                                                                                \
    do {                                                                                                               \
        ++tests_run;                                                                                                   \
        try {                                                                                                          \
            if (!(cond)) {                                                                                             \
                throw std::runtime_error("Assertion failed: " #cond);                                                  \
            }                                                                                                          \
        } catch (const std::exception &e) {                                                                            \
            ++tests_failed;                                                                                            \
            failures.push_back(std::string(__FILE__) + ":" + std::to_string(__LINE__) + " " + msg + " - " + e.what()); \
        }                                                                                                              \
    } while (0)

#define TEST_THROWS(expr, msg)                                                                                         \
    do {                                                                                                               \
        ++tests_run;                                                                                                   \
        bool threw = false;                                                                                            \
        try {                                                                                                          \
            expr;                                                                                                      \
        } catch (...) { threw = true; }                                                                                \
        if (!threw) {                                                                                                  \
            ++tests_failed;                                                                                            \
            failures.push_back(std::string(__FILE__) + ":" + std::to_string(__LINE__) + " " + msg +                    \
                               " - expected exception not thrown");                                                    \
        }                                                                                                              \
    } while (0)

// ----------------------------------------------------------------------
// Test suite
// ----------------------------------------------------------------------

void test_type_checkers() {
    printf("--- Type checkers ---\n");
    value int_val = make_value(42);
    value double_val = make_value(3.14);
    value str_val = new_stringz("hello");
    value nil_val = make_value(nullptr);
    value true_val = make_value(true);
    value cons_val = cons(make_value(1), make_value(2));
    value list_val = list(1, 2, 3);

    TEST(is_num(int_val), "int should be numeric");
    TEST(is_num(double_val), "double should be numeric");
    TEST(!is_num(str_val), "string should not be numeric");
    TEST(is_obj(str_val), "string should be object");
    TEST(is_nil(nil_val), "nullptr should be nil");
    TEST(is_true(true_val), "true should be true");
    TEST(!is_true(nil_val), "nil should not be true");
    TEST(is_cons(cons_val), "cons should be cons");
    TEST(!is_cons(int_val), "int should not be cons");
    TEST(is_string(str_val), "string should be string");
    TEST(!is_string(int_val), "int should not be string");
    TEST(is_list(list_val), "list should be list");
    TEST(!is_list(cons_val), "dotted pair should not be list");
}

void test_creation_and_access() {
    printf("--- Creation and access ---\n");
    value lst = list(1, 2, 3, 4, 5);
    TEST(length(lst) == 5, "length should be 5");

    TEST(as_num_int(car(lst)) == 1, "car of list should be 1");
    TEST(as_num_int(cadr(lst)) == 2, "cadr should be 2");
    // Use proper accessors
    TEST(as_num_int(first(lst)) == 1, "first == 1");
    TEST(as_num_int(second(lst)) == 2, "second == 2");
    TEST(as_num_int(third(lst)) == 3, "third == 3");
    TEST(as_num_int(fourth(lst)) == 4, "fourth == 4");
    TEST(as_num_int(fifth(lst)) == 5, "fifth == 5");
    // nth
    TEST(as_num_int(nth(lst, 0)) == 1, "nth 0 == 1");
    TEST(as_num_int(nth(lst, 4)) == 5, "nth 4 == 5");
    TEST_THROWS(as_num_int(nth(lst, 5)), "nth out of bounds should throw");
    // nthcdr
    value rest2 = nthcdr(lst, 2);
    TEST(length(rest2) == 3, "nthcdr 2 should have length 3");
    TEST(as_num_int(car(rest2)) == 3, "car of nthcdr 2 should be 3");

    // first_2, first_3, first_4
    auto [a, b] = first_2(lst);
    TEST(as_num_int(a) == 1 && as_num_int(b) == 2, "first_2");
    auto [c, d, e] = first_3(lst);
    TEST(as_num_int(c) == 1 && as_num_int(d) == 2 && as_num_int(e) == 3, "first_3");
    auto [f, g, h, i] = first_4(lst);
    TEST(as_num_int(f) == 1 && as_num_int(g) == 2 && as_num_int(h) == 3 && as_num_int(i) == 4, "first_4");

    // cons unapply
    value pair = cons(make_value(10), make_value(20));
    auto [car_ref, cdr_ref] = unapply_cons(pair);
    TEST(as_num_int(car_ref) == 10, "unapply_cons car");
    TEST(as_num_int(cdr_ref) == 20, "unapply_cons cdr");
}

void test_optional_accessors() {
    printf("--- Optional accessors ---\n");
    value lst = list(1, 2, 3);
    value empty = nil;

    TEST(car_opt(lst).has_value(), "car_opt on non-empty");
    TEST(as_num_int(*car_opt(lst)) == 1, "car_opt value");
    TEST(!car_opt(empty).has_value(), "car_opt on nil");

    TEST(cdr_opt(lst).has_value(), "cdr_opt on non-empty");
    TEST(length(*cdr_opt(lst)) == 2, "cdr_opt length");
    TEST(!cdr_opt(empty).has_value(), "cdr_opt on nil");

    TEST(cadr_opt(lst).has_value() && as_num_int(*cadr_opt(lst)) == 2, "cadr_opt");
    TEST(!caadr_opt(empty).has_value(), "caadr_opt on nil");

    TEST(nth_opt(lst, 2).has_value() && as_num_int(*nth_opt(lst, 2)) == 3, "nth_opt");
    TEST(!nth_opt(lst, 5).has_value(), "nth_opt out of range");
    TEST(nthcdr_opt(lst, 1).has_value() && length(*nthcdr_opt(lst, 1)) == 2, "nthcdr_opt");
    TEST(!nthcdr_opt(lst, 5).has_value(), "nthcdr_opt out of range");

    TEST(head_opt(lst) && as_num_int(*head_opt(lst)) == 1, "head_opt");
    TEST(!head_opt(empty).has_value(), "head_opt on nil");
    TEST(rest_opt(lst) && length(*rest_opt(lst)) == 2, "rest_opt");
    TEST(!rest_opt(empty).has_value(), "rest_opt on nil");
}

void test_list_manipulation() {
    printf("--- List manipulation ---\n");
    value a = list(1, 2, 3);
    value b = list(4, 5, 6);

    // append
    value ab = append(a, b);
    TEST(length(ab) == 6, "append length");
    TEST(as_num_int(nth(ab, 3)) == 4, "append element");

    // reverse
    value rev = reverse(a);
    TEST(as_num_int(first(rev)) == 3 && as_num_int(third(rev)) == 1, "reverse");

    // add_last (modifies first and last)
    value first_list = a;
    value last_list = nthcdr(a, 2); // last cons cell
    value new_elem = make_value(7);
    add_last(first_list, last_list, new_elem);
    TEST(length(first_list) == 4, "add_last length");
    TEST(as_num_int(nth(first_list, 3)) == 7, "add_last element");

    // cartesian_product
    value nums = list(1, 2);
    value strs = list("a", "b");
    value prod = cartesian_product(nums, strs);
    TEST(length(prod) == 4, "cartesian product length 2x2");

    // range
    value r = range(5);
    TEST(length(r) == 5, "range(5) length");
    TEST(as_num_int(nth(r, 0)) == 0 && as_num_int(nth(r, 4)) == 4, "range values");
    value r2 = range(1.0, 5.0, 1.5);
    TEST(length(r2) == 3, "range with step length");
    // values: 1.0, 2.5, 4.0
    TEST(as_num_f64(nth(r2, 0)) == 1.0 && as_num_f64(nth(r2, 2)) == 4.0, "range step values");
}

void test_higher_order_functions() {
    printf("--- Higher-order functions ---\n");
    value lst = list(1, 2, 3, 4, 5);

    // map
    value doubled = map([](value x) { return make_value(as_num_int(x) * 2); }, lst);
    TEST(as_num_int(first(doubled)) == 2 && as_num_int(fifth(doubled)) == 10, "map double");

    // filter
    value odds = filter(is_odd, lst);
    TEST(length(odds) == 3, "filter odds");
    TEST(as_num_int(first(odds)) == 1 && as_num_int(third(odds)) == 5, "filter values");

    // filter_not
    value evens = filter_not(is_odd, lst);
    TEST(length(evens) == 2, "filter_not evens");

    // remove (by predicate)
    value no_odd = remove(lst, is_odd);
    TEST(length(no_odd) == 2, "remove by predicate");

    // remove (by value)
    value lst2 = list(1, 2, 3, 2, 1);
    value no_2 = remove(make_value(2), lst2);
    TEST(length(no_2) == 3, "remove by value");

    // all
    TEST(all([](value x) { return as_num_int(x) > 0; }, lst), "all positive");
    TEST(!all(is_odd, lst), "not all odd");

    // any
    TEST(any(is_odd, lst), "any odd");
    TEST(!any([](value x) { return as_num_int(x) > 10; }, lst), "any >10 false");

    // foldl
    int sum = foldl([](value x, int acc) { return as_num_int(x) + acc; }, 0, lst);
    TEST(sum == 15, "foldl sum");

    // foldr (string concatenation)
    value strs = list("a", "b", "c");
    std::string concat =
        foldr([](value x, std::string acc) { return std::string{as_string_view(x)} + acc; }, std::string(""), strs);
    TEST(concat == "abc",
         "foldr string concatenation"); // foldr typically processes from right: "a" + ("b" + ("c" + "")) = "abc"

    // build_list
    value squares = build_list(5, [](size_t i) { return make_value(int(i * i)); });
    TEST(length(squares) == 5, "build_list length");
    TEST(as_num_int(nth(squares, 2)) == 4, "build_list value");
}

void test_predicates() {
    printf("--- Predicates ---\n");
    TEST(is_even(make_value(2)), "2 even");
    TEST(!is_even(make_value(3)), "3 odd");
    TEST(is_odd(make_value(3)), "3 odd");
    TEST(is_zero(make_value(0)), "0 zero");
    TEST(is_positive(make_value(5)), "5 positive");
    TEST(!is_positive(make_value(-1)), "-1 not positive");
    TEST(is_negative(make_value(-1)), "-1 negative");
}

void test_assoc_and_member() {
    printf("--- Association lists and member ---\n");
    // alist: ((a . 1) (b . 2) (c . 3))
    value a = cons(make_value("a"), make_value(1));
    value b = cons(make_value("b"), make_value(2));
    value c = cons(make_value("c"), make_value(3));
    value alist = list(a, b, c);

    value found = assoc(make_value("b"), alist);
    TEST(!is_nil(found), "assoc found");
    TEST(as_num_int(cdr(found)) == 2, "assoc value");

    value not_found = assoc(make_value("x"), alist);
    TEST(is_nil(not_found), "assoc not found");

    // member (by value)
    TEST(member(make_value(2), alist, [](value x, value y) { return as_num_int(cdr(x)) == as_num_int(y); }),
         "member by custom predicate");
    TEST(!member(make_value(10), alist, [](value x, value y) { return as_num_int(cdr(x)) == as_num_int(y); }),
         "member not found");

    // member (default equality)
    value lst = list(1, 2, 3);
    TEST(member(make_value(2), lst), "member default");
    TEST(!member(make_value(5), lst), "member default not found");

    // index_of
    auto idx = index_of(lst, make_value(3));
    TEST(idx.has_value() && *idx == 2, "index_of found");
    idx = index_of(lst, make_value(10));
    TEST(!idx.has_value(), "index_of not found");
    idx = index_of(lst, [](value x) { return as_num_int(x) % 2 == 0; });
    TEST(idx.has_value() && *idx == 1, "index_of predicate");
}

void test_unsafe_functions_and_mutators() {
    printf("--- Unsafe functions and mutators ---\n");
    value pair = cons(make_value(10), make_value(20));
    // Actually unwrap_f64 is for numbers; test on number
    value num = make_value(3.14);
    TEST(unwrap_f64(num) == 3.14, "unwrap_f64");

    // unwrap_string_view
    value str = new_stringz("hello");
    TEST(unwrap_string_view(str) == "hello", "unwrap_string_view");

    // unwrap_car/cdr
    TEST(as_num_int(unwrap_car(pair)) == 10, "unwrap_car");
    TEST(as_num_int(unwrap_cdr(pair)) == 20, "unwrap_cdr");

    // mutators
    unwrap_setcar(pair, make_value(99));
    unwrap_setcdr(pair, make_value(100));
    TEST(as_num_int(unwrap_car(pair)) == 99, "unwrap_setcar");
    TEST(as_num_int(unwrap_cdr(pair)) == 100, "unwrap_setcdr");
}

void test_heterogeneous_and_iteration() {
    printf("--- Heterogeneous lists and iteration ---\n");
    value mixed = list(1, 3.14, "hello", list(10, 20), cons(make_value("car"), make_value("cdr")), true, nullptr);

    // Check types
    TEST(is_num(first(mixed)), "first is num");
    TEST(is_num(second(mixed)), "second is num (double)");
    TEST(is_string(third(mixed)), "third is string");
    TEST(is_list(fourth(mixed)), "fourth is list");
    TEST(is_cons(fifth(mixed)), "fifth is cons");
    TEST(is_true(sixth(mixed)), "sixth is true");
    TEST(is_nil(seventh(mixed)), "seventh is nil");

    // Iteration over homogenous list
    value hom = list("apple", "banana", "cherry");
    std::vector<std::string> collected;
    for (std::string_view sv : hom.iter().as<std::string_view>()) {
        collected.push_back(std::string(sv));
    }
    TEST(collected.size() == 3, "iter count");
    TEST(collected[0] == "apple" && collected[2] == "cherry", "iter values");

    TEST_THROWS((void)mixed.iter().as<std::string_view>().begin(),
                "iter on mixed list should throw when accessing as string_view");
}
void test_map_filter_numeric() {
    printf("--- Map/filter numeric calculations ---\n");
    value nums = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

    // Filter even numbers, then square them
    auto even_squares = map(
        [](value x) {
            int n = as_num_int(x);
            return make_value(n * n);
        },
        filter(is_even, nums));

    TEST(length(even_squares) == 5, "even squares count");
    TEST(as_num_int(first(even_squares)) == 4, "first even square (2^2)");
    TEST(as_num_int(fifth(even_squares)) == 100, "last even square (10^2)");

    // Filter numbers divisible by 3, then compute cubes
    auto cubes_of_threes = map(
        [](value x) {
            int n = as_num_int(x);
            return make_value(n * n * n);
        },
        filter([](value x) { return as_num_int(x) % 3 == 0; }, nums));

    TEST(length(cubes_of_threes) == 3, "cubes of multiples of 3 count");
    TEST(as_num_int(first(cubes_of_threes)) == 27, "3^3");
    TEST(as_num_int(second(cubes_of_threes)) == 216, "6^3");
    TEST(as_num_int(third(cubes_of_threes)) == 729, "9^3");

    // Use map with two lists: pairwise sum
    value list_a = list(1, 2, 3);
    value list_b = list(10, 20, 30);
    auto sum_lists = map([](value a, value b) { return make_value(as_num_int(a) + as_num_int(b)); }, list_a, list_b);
    TEST(length(sum_lists) == 3, "pairwise sum length");
    TEST(as_num_int(first(sum_lists)) == 11, "first sum");
    TEST(as_num_int(second(sum_lists)) == 22, "second sum");
    TEST(as_num_int(third(sum_lists)) == 33, "third sum");

    // Filter and fold: sum of squares of odd numbers
    int sum_squares_odds =
        foldl([](value x, int acc) { return acc + as_num_int(x) * as_num_int(x); }, 0, filter(is_odd, nums));
    TEST(sum_squares_odds == 1 + 9 + 25 + 49 + 81, "sum squares of odds"); // 165
}

void test_map_filter_heterogeneous() {
    printf("--- Map/filter on heterogeneous lists ---\n");
    // Mixed list: numbers, strings, booleans, nil, a nested list
    value mixed = list(42, 3.14, "hello", true, nullptr, list(1, 2, 3), "world", 99);

    // Filter only numbers (integers and floats)
    auto numbers_only = filter([](value x) { return is_num(x); }, mixed);
    TEST(length(numbers_only) == 3, "number count"); // 42, 3.14, 99
    TEST(as_num_int(first(numbers_only)) == 42, "first number");
    TEST(as_num_f64(second(numbers_only)) == 3.14, "second number (double)");
    TEST(as_num_int(third(numbers_only)) == 99, "third number");

    // Filter only strings
    auto strings_only = filter(is_string, mixed);
    TEST(length(strings_only) == 2, "string count");
    TEST(as_string_view(first(strings_only)) == "hello", "first string");
    TEST(as_string_view(second(strings_only)) == "world", "second string");

    auto to_string_repr = [](value x) -> std::string {
        if (is_num(x)) {
            double d = as_num_f64(x);
            // simple formatting, could use std::to_string
            char buf[64];
            snprintf(buf, sizeof(buf), "%g", d);
            return buf;
        } else if (is_string(x)) {
            return std::string("\"") + std::string(as_string_view(x)) + "\"";
        } else if (is_true(x)) {
            return "true";
        } else if (is_nil(x)) {
            return "nil";
        } else if (is_list(x)) {
            return "(list)"; // simplified
        } else {
            return "?";
        }
    };

    auto string_reprs = map([&](value x) { return new_stringz(to_string_repr(x).c_str()); }, mixed);

    TEST(length(string_reprs) == 8, "string reprs length");
    // Check first few
    TEST(as_string_view(first(string_reprs)) == "42", "repr of 42");
    TEST(as_string_view(second(string_reprs)) == "3.14", "repr of 3.14");
    TEST(as_string_view(third(string_reprs)) == "\"hello\"", "repr of hello");
    TEST(as_string_view(fourth(string_reprs)) == "true", "repr of true");
    TEST(as_string_view(fifth(string_reprs)) == "nil", "repr of nil");
    TEST(as_string_view(sixth(string_reprs)) == "(list)", "repr of nested list");
    TEST(as_string_view(seventh(string_reprs)) == "\"world\"", "repr of world");
    TEST(as_string_view(eighth(string_reprs)) == "99", "repr of 99");

    // Filter and map together: from mixed list, get numbers, double them, return as list of numbers
    auto doubled_numbers = map([](value x) { return make_value(as_num_f64(x) * 2); }, filter(is_num, mixed));
    TEST(length(doubled_numbers) == 3, "doubled numbers count");
    TEST(as_num_f64(first(doubled_numbers)) == 84.0, "42*2");
    TEST(as_num_f64(second(doubled_numbers)) == 6.28, "3.14*2");
    TEST(as_num_f64(third(doubled_numbers)) == 198.0, "99*2");

    // Filter strings that contain 'o' and map to uppercase (simulate)
    auto filtered_uppercase = map(
        [](value x) {
            std::string s(as_string_view(x));
            for (char &c : s)
                c = toupper(c);
            return new_stringz(s.c_str());
        },
        filter(
            [](value x) {
                std::string_view sv = as_string_view(x);
                return sv.find('o') != std::string_view::npos;
            },
            filter(is_string, mixed)));

    TEST(length(filtered_uppercase) == 2, "strings with 'o' count");
    // "hello" and "world" both contain 'o'
    auto it = filtered_uppercase.iter().as<std::string_view>();
    std::vector<std::string> results;
    for (auto sv : it)
        results.push_back(std::string(sv));
    TEST(results[0] == "HELLO" || results[1] == "HELLO", "HELLO present");
    TEST(results[0] == "WORLD" || results[1] == "WORLD", "WORLD present");
}

void test_invalid_uses() {
    printf("--- Invalid uses (should throw) ---\n");

    // --- Basic values for invalid tests ---
    value num_val = make_value(42);
    value str_val = new_stringz("hello");
    value bool_val = make_value(true);
    value nil_val = nil;
    value cons_val = cons(make_value(1), make_value(2));
    value proper_list = list(1, 2, 3);
    value improper_list = cons(make_value(1), cons(make_value(2), make_value(3))); // (1 2 . 3)

    // --- Type conversion functions ---
    TEST_THROWS(as_string_view(num_val), "as_string_view on number");
    TEST_THROWS(as_string_view(cons_val), "as_string_view on cons");
    TEST_THROWS(as_string_view(nil_val), "as_string_view on nil");
    TEST_THROWS(as_string_view(bool_val), "as_string_view on boolean");

    TEST_THROWS(as_num_f64(str_val), "as_num_f64 on string");
    TEST_THROWS(as_num_f64(cons_val), "as_num_f64 on cons");
    TEST_THROWS(as_num_f64(nil_val), "as_num_f64 on nil");
    TEST_THROWS(as_num_f64(bool_val), "as_num_f64 on boolean");

    TEST_THROWS(as_num_int(str_val), "as_num_int on string");
    TEST_THROWS(as_num_int(cons_val), "as_num_int on cons");
    TEST_THROWS(as_num_int(nil_val), "as_num_int on nil");
    TEST_THROWS(as_num_int(bool_val), "as_num_int on boolean");

    // --- Cons/car/cdr accessors ---
    TEST_THROWS(car(num_val), "car on number");
    TEST_THROWS(car(str_val), "car on string");
    TEST_THROWS(car(bool_val), "car on boolean");
    TEST_THROWS(car(nil_val), "car on nil"); // nil is not a cons

    TEST_THROWS(cdr(num_val), "cdr on number");
    TEST_THROWS(cdr(str_val), "cdr on string");
    TEST_THROWS(cdr(bool_val), "cdr on boolean");
    TEST_THROWS(cdr(nil_val), "cdr on nil");

    // --- caar, cadr, etc. (composed accessors) ---
    TEST_THROWS(caar(num_val), "caar on number");
    TEST_THROWS(caar(proper_list), "caar on proper list (should be car of car, but car is 1 which is not cons)");
    TEST_THROWS(cadr(str_val), "cadr on string");
    TEST_THROWS(caddr(cons_val), "caddr on dotted pair (not long enough)");

    // --- head, rest, first..tenth ---
    TEST_THROWS(head(num_val), "head on number");
    TEST_THROWS(head(str_val), "head on string");
    TEST_THROWS(head(bool_val), "head on boolean");
    TEST_THROWS(head(nil_val), "head on nil");
    // head is same as car, so similar.

    TEST_THROWS(rest(num_val), "rest on number");
    TEST_THROWS(rest(str_val), "rest on string");
    TEST_THROWS(rest(bool_val), "rest on boolean");
    TEST_THROWS(rest(nil_val), "rest on nil");

    TEST_THROWS(first(num_val), "first on number");
    TEST_THROWS(first(str_val), "first on string");
    TEST_THROWS(first(bool_val), "first on boolean");
    TEST_THROWS(first(nil_val), "first on nil");

    TEST_THROWS(second(num_val), "second on number");
    // Need a list with less than 2 elements:
    value one_elem = list(42);
    TEST_THROWS(second(one_elem), "second on list of length 1");
    TEST_THROWS(third(one_elem), "third on list of length 1");
    TEST_THROWS(fourth(one_elem), "fourth on list of length 1");
    // Similarly for others.

    // --- nth, nthcdr ---
    TEST_THROWS(nth(num_val, 0), "nth on number");
    TEST_THROWS(nth(str_val, 0), "nth on string");
    TEST_THROWS(nth(bool_val, 0), "nth on boolean");
    TEST_THROWS(nth(nil_val, 0), "nth on nil");
    // out-of-range nth on proper list should also throw:
    TEST_THROWS(nth(proper_list, 5), "nth index out of range");

    TEST_THROWS(nthcdr(num_val, 1), "nthcdr on number");
    TEST_THROWS(nthcdr(str_val, 1), "nthcdr on string");
    TEST_THROWS(nthcdr(bool_val, 1), "nthcdr on boolean");
    TEST_THROWS(nthcdr(nil_val, 1), "nthcdr on nil");
    TEST_THROWS(nthcdr(proper_list, 5), "nthcdr index out of range");

    // --- first_2, first_3, first_4 ---
    TEST_THROWS(first_2(num_val), "first_2 on number");
    TEST_THROWS(first_2(one_elem), "first_2 on list length 1");
    value two_elem = list(1, 2);
    TEST_THROWS(first_3(two_elem), "first_3 on list length 2");
    TEST_THROWS(first_4(proper_list), "first_4 on list length 3"); // needs at least 4

    // --- unapply_cons ---
    TEST_THROWS(unapply_cons(num_val), "unapply_cons on number");
    TEST_THROWS(unapply_cons(str_val), "unapply_cons on string");
    TEST_THROWS(unapply_cons(bool_val), "unapply_cons on boolean");
    TEST_THROWS(unapply_cons(nil_val), "unapply_cons on nil");

    // --- length ---
    TEST_THROWS(length(num_val), "length on number");
    TEST_THROWS(length(str_val), "length on string");
    TEST_THROWS(length(bool_val), "length on boolean");
    TEST_THROWS(length(improper_list), "length on improper list");

    // --- List manipulation functions that expect lists ---
    // reverse
    TEST_THROWS(reverse(num_val), "reverse on number");
    TEST_THROWS(reverse(str_val), "reverse on string");
    TEST_THROWS(reverse(bool_val), "reverse on boolean");
    TEST_THROWS(reverse(improper_list), "reverse on improper list");

    // append
    TEST_THROWS(append(num_val, proper_list), "append first arg not list");
    TEST_THROWS(append(proper_list, num_val), "append second arg not list");
    TEST_THROWS(append(num_val, str_val), "append both not list");

    // map (single list)
    auto f = [](value x) { return x; };
    TEST_THROWS(map(f, num_val), "map on number");
    TEST_THROWS(map(f, str_val), "map on string");
    TEST_THROWS(map(f, bool_val), "map on boolean");
    TEST_THROWS(map(f, improper_list), "map on improper list");

    // map with two lists
    TEST(is_equal(map([](value a, value) { return a; }, proper_list, num_val), nil), "map two lists, second not list");
    TEST(is_equal(map([](value a, value) { return a; }, num_val, proper_list), nil), "map two lists, first not list");

    // filter
    TEST_THROWS(filter(is_odd, num_val), "filter on number");
    TEST_THROWS(filter(is_odd, str_val), "filter on string");
    TEST_THROWS(filter(is_odd, bool_val), "filter on boolean");
    TEST_THROWS(filter(is_odd, improper_list), "filter on improper list");

    // filter_not
    TEST_THROWS(filter_not(is_odd, num_val), "filter_not on number");

    // remove (by predicate)
    TEST_THROWS(remove(num_val, is_odd), "remove by predicate on non-list");
    TEST_THROWS(remove(improper_list, is_odd), "remove by predicate on improper list");

    // remove (by value)
    TEST_THROWS(remove(make_value(1), num_val), "remove by value on non-list");
    TEST_THROWS(remove(make_value(1), improper_list), "remove by value on improper list");

    // all
    TEST_THROWS(all(is_odd, num_val), "all on non-list");

    // any
    TEST_THROWS(any(is_odd, num_val), "any on non-list");
    TEST(any(is_odd, improper_list), "any on improper list");

    // foldl
    TEST_THROWS(foldl([](value, int acc) { return acc; }, 0, num_val), "foldl on non-list");
    TEST_THROWS(foldl([](value, int acc) { return acc; }, 0, improper_list), "foldl on improper list");

    // foldr
    TEST_THROWS(foldr([](value, int acc) { return acc; }, 0, num_val), "foldr on non-list");
    TEST_THROWS(foldr([](value, int acc) { return acc; }, 0, improper_list), "foldr on improper list");

    // cartesian_product
    TEST_THROWS(cartesian_product(num_val, proper_list), "cartesian_product first not list");
    TEST_THROWS(cartesian_product(proper_list, num_val), "cartesian_product second not list");
    TEST_THROWS(cartesian_product(proper_list, proper_list, num_val), "cartesian_product three args third not list");

    // assoc
    TEST_THROWS(assoc(make_value("a"), num_val), "assoc on non-list");
    TEST_THROWS(assoc(make_value("a"), improper_list), "assoc on improper list");

    // member (by predicate)
    TEST_THROWS(member(make_value(1), num_val, [](value, value) { return true; }), "member predicate on non-list");

    // member (default)
    TEST_THROWS(member(make_value(1), num_val), "member default on non-list");

    // index_of (by predicate)
    TEST_THROWS(index_of(num_val, [](value) { return true; }), "index_of predicate on non-list");

    // index_of (by value)
    TEST_THROWS(index_of(num_val, make_value(1)), "index_of value on non-list");

    // --- Heterogeneous iteration ---
    value mixed = list(1, "hello", true);
    // .iter().as<T>() should throw if any element is not of type T
    TEST_THROWS((void)mixed.iter().as<int>().begin()++ ++, "iter().as<int>() on mixed list");
    TEST_THROWS((void)mixed.iter().as<std::string_view>().begin()++ ++, "iter().as<string_view>() on mixed list");
    // Homogeneous list works:
    value hom = list(1, 2, 3);
    // No throw:
    try {
        for (int i : hom.iter().as<int>()) {
            (void)i;
        }
    } catch (...) { TEST(false, "iter().as<int>() on homogeneous list should not throw"); }
}
void test_arithmetic_operators() {
    printf("--- Arithmetic operators ---\n");

    value i1 = make_value(10);
    value i2 = make_value(3);
    value d1 = make_value(2.5);
    value d2 = make_value(1.5);
    value str = new_stringz("hello");
    value boolv = make_value(true);
    value nilv = nil;

    // --- value + value ---
    value sum1 = i1 + i2;
    TEST(is_num(sum1), "int+int result is num");
    TEST(as_num_int(sum1) == 13, "int+int value");

    value sum2 = d1 + d2;
    TEST(is_num(sum2), "double+double result is num");
    TEST(as_num_f64(sum2) == 4.0, "double+double value");

    value sum3 = i1 + d1;
    TEST(is_num(sum3), "int+double result is num");
    TEST(as_num_f64(sum3) == 12.5, "int+double value");

    // --- value + double ---
    value sum4 = i1 + 5.5;
    TEST(is_num(sum4), "value+double result is num");
    TEST(as_num_f64(sum4) == 15.5, "value+double value");

    // --- value - value ---
    value diff1 = i1 - i2;
    TEST(as_num_int(diff1) == 7, "int-int");

    value diff2 = d1 - d2;
    TEST(as_num_f64(diff2) == 1.0, "double-double");

    value diff3 = i1 - d2;
    TEST(as_num_f64(diff3) == 8.5, "int-double");

    // --- value - double ---
    value diff4 = i1 - 2.5;
    TEST(as_num_f64(diff4) == 7.5, "value-double");

    // --- value * value ---
    value prod1 = i1 * i2;
    TEST(as_num_int(prod1) == 30, "int*int");

    value prod2 = d1 * d2;
    TEST(as_num_f64(prod2) == 3.75, "double*double");

    value prod3 = i1 * d2;
    TEST(as_num_f64(prod3) == 15.0, "int*double");

    // --- value * double ---
    value prod4 = i1 * 1.5;
    TEST(as_num_f64(prod4) == 15.0, "value*double");

    // --- value / value ---
    value quot1 = i1 / i2;
    // integer division? Depending on implementation, could be 3 (if integer division) or 3.333...
    // The API returns value, could be integer or double? Since i1/i2 both ints, maybe integer division? Not specified.
    // We'll test both ints and ensure result is numeric.
    TEST(is_num(quot1), "int/int result num");

    // To avoid ambiguity, test with doubles:
    value quot2 = d1 / d2;
    TEST(as_num_f64(quot2) == 2.5 / 1.5, "double/double"); // approx 1.6666667

    value quot3 = i1 / d2;
    TEST(as_num_f64(quot3) == 10.0 / 1.5, "int/double");

    // --- value / double ---
    value quot4 = i1 / 2.0;
    TEST(as_num_f64(quot4) == 5.0, "value/double");

    // --- Test with negative numbers ---
    value neg = make_value(-5);
    value pos = make_value(3);
    TEST(as_num_int(neg + pos) == -2, "neg+pos");
    TEST(as_num_int(neg - pos) == -8, "neg-pos");
    TEST(as_num_int(neg * pos) == -15, "neg*pos");
    TEST(as_num_f64(neg / make_value(2.0)) == -2.5, "neg/pos double");

    // --- Invalid uses (should throw) ---
    // value + non-number
    TEST_THROWS(i1 + str, "add with string");
    TEST_THROWS(str + i1, "add string+int");
    TEST_THROWS(i1 + boolv, "add with bool");
    TEST_THROWS(i1 + nilv, "add with nil");

    // value - non-number
    TEST_THROWS(i1 - str, "subtract with string");
    TEST_THROWS(str - i1, "subtract string");

    // value * non-number
    TEST_THROWS(i1 * str, "multiply with string");
    TEST_THROWS(str * i1, "multiply string");

    // value / non-number
    TEST_THROWS(i1 / str, "divide with string");
    TEST_THROWS(str / i1, "divide string");

    // operations between two non-numbers
    TEST_THROWS(str + str, "string+string");
    TEST_THROWS(boolv + boolv, "bool+bool");
    TEST_THROWS(nilv + nilv, "nil+nil");

    // Check that original values are unchanged
    TEST(as_num_int(i1) == 10, "original i1 unchanged");
    TEST(as_num_int(i2) == 3, "original i2 unchanged");
    TEST(as_num_f64(d1) == 2.5, "original d1 unchanged");
    TEST(as_num_f64(d2) == 1.5, "original d2 unchanged");
}
#include <optional>
#include <vector>

// ===== Complex types for serialization test =====
struct Person {
    std::string name;
    std::string address;
    int age;
    std::optional<std::string> email;
};

struct Department {
    std::string name;
    Person head;
    std::vector<Person> employees;
};

struct Company {
    std::string name;
    int founded;
    std::vector<Department> departments;
};

// ===== Serialization traits =====
// Person
template <> struct headerlisp::to_list<Person> {
    auto operator()(const Person &p) {
        value email_val = p.email ? headerlisp::make_value(*p.email) : headerlisp::nil;
        return headerlisp::list(p.name, p.address, p.age, email_val);
    }
};

template <> struct headerlisp::from_list<Person> {
    Person operator()(value lst) {
        auto [name, addr, age, email] = headerlisp::first_4(lst);
        Person p;
        p.name = headerlisp::as_string_view(name);
        p.address = headerlisp::as_string_view(addr);
        p.age = headerlisp::as_num_int(age);
        if (!headerlisp::is_nil(email)) {
            p.email = std::string(headerlisp::as_string_view(email));
        }
        return p;
    }
};

// std::vector<T>
namespace headerlisp {
template <typename T> struct to_list<std::vector<T>> {
    auto operator()(const std::vector<T> &vec) {
        value result = nil;
        for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
            result = headerlisp::cons(headerlisp::make_value(*it), result);
        }
        return result;
    }
};

template <typename T> struct from_list<std::vector<T>> {
    std::vector<T> operator()(value lst) {
        std::vector<T> result;
        while (!headerlisp::is_nil(lst)) {
            result.push_back(headerlisp::as<T>(headerlisp::car(lst)));
            lst = headerlisp::cdr(lst);
        }
        return result;
    }
};
} // namespace headerlisp

// Department
template <> struct headerlisp::to_list<Department> {
    auto operator()(const Department &d) { return headerlisp::list(d.name, d.head, d.employees); }
};

template <> struct headerlisp::from_list<Department> {
    Department operator()(value lst) {
        auto [name, head, employees] = headerlisp::first_3(lst);
        Department d;
        d.name = headerlisp::as_string_view(name);
        d.head = headerlisp::as<Person>(head);
        d.employees = headerlisp::as<std::vector<Person>>(employees);
        return d;
    }
};

// Company
template <> struct headerlisp::to_list<Company> {
    auto operator()(const Company &c) { return headerlisp::list(c.name, c.founded, c.departments); }
};

template <> struct headerlisp::from_list<Company> {
    Company operator()(value lst) {
        auto [name, founded, depts] = headerlisp::first_3(lst);
        Company c;
        c.name = headerlisp::as_string_view(name);
        c.founded = headerlisp::as_num_int(founded);
        c.departments = headerlisp::as<std::vector<Department>>(depts);
        return c;
    }
};

// ===== Test function =====
void test_serialization_complex() {
    printf("--- Complex serialization test ---\n");

    // Create sample data
    Person alice{"Alice", "123 Wonderland", 30, "alice@example.com"};
    Person bob{"Bob", "456 Builder St", 25, std::nullopt};
    Person charlie{"Charlie", "789 Chocolate Ave", 35, "charlie@wonka.com"};

    Department engineering{"Engineering", alice, {bob, charlie}};
    Department sales{"Sales", bob, {}}; // empty employees list

    Company company{"TechCorp", 2000, {engineering, sales}};

    // Serialize
    value serialized = headerlisp::make_value(company);
    std::string printed = headerlisp::print(serialized);
    TEST(
        printed ==
            R"FOO(("TechCorp" 2000.000000 (("Engineering" ("Alice" "123 Wonderland" 30.000000 "alice@example.com") (("Bob" "456 Builder St" 25.000000 ()) ("Charlie" "789 Chocolate Ave" 35.000000 "charlie@wonka.com"))) ("Sales" ("Bob" "456 Builder St" 25.000000 ()) ()))))FOO",
        "serialized wrong");

    // Deserialize
    value read_back = headerlisp::read(printed);
    Company deserialized = headerlisp::as<Company>(read_back);

    // Verify
    TEST(deserialized.name == "TechCorp", "company name");
    TEST(deserialized.founded == 2000, "founded year");
    TEST(deserialized.departments.size() == 2, "department count");

    const auto &dept0 = deserialized.departments[0];
    TEST(dept0.name == "Engineering", "first dept name");
    TEST(dept0.head.name == "Alice", "dept head name");
    TEST(dept0.head.email.has_value() && *dept0.head.email == "alice@example.com", "head email");
    TEST(dept0.employees.size() == 2, "engineering employees count");
    TEST(dept0.employees[0].name == "Bob", "first employee name");
    TEST(!dept0.employees[0].email.has_value(), "bob no email");
    TEST(dept0.employees[1].name == "Charlie", "second employee name");
    TEST(dept0.employees[1].email.has_value() && *dept0.employees[1].email == "charlie@wonka.com", "charlie email");

    const auto &dept1 = deserialized.departments[1];
    TEST(dept1.name == "Sales", "second dept name");
    TEST(dept1.head.name == "Bob", "sales head name");
    TEST(dept1.employees.empty(), "sales employees empty");

    // Test round-trip on a single Person with optional
    value person_val = headerlisp::make_value(alice);
    std::string person_str = headerlisp::print(person_val);
    value person_read = headerlisp::read(person_str);
    Person alice2 = headerlisp::as<Person>(person_read);
    TEST(alice2.name == "Alice", "person name");
    TEST(alice2.email.has_value() && *alice2.email == "alice@example.com", "person email");

    // Test Person with missing email (Bob)
    person_val = headerlisp::make_value(bob);
    person_str = headerlisp::print(person_val);
    person_read = headerlisp::read(person_str);
    Person bob2 = headerlisp::as<Person>(person_read);
    TEST(bob2.name == "Bob", "bob name");
    TEST(!bob2.email.has_value(), "bob email missing");

    // Test empty list serialization (e.g., empty vector of Persons)
    std::vector<Person> empty;
    value empty_val = headerlisp::make_value(empty);
    std::string empty_str = headerlisp::print(empty_val);
    TEST(empty_str == "()", "empty list prints as ()");
    auto empty_vec = headerlisp::as<std::vector<Person>>(headerlisp::read(empty_str));
    TEST(empty_vec.empty(), "empty vector deserialized");
}

struct Node {
    virtual ~Node() = default;
    virtual int eval() = 0;
};

struct Num : Node {
    int value;
    Num(int v) : value(v) {}
    int eval() override { return value; }
};

struct Add : Node {
    std::vector<std::unique_ptr<Node>> args;
    int eval() override {
        int sum = 0;
        for (auto &arg : args)
            sum += arg->eval();
        return sum;
    }
};

struct Mul : Node {
    std::vector<std::unique_ptr<Node>> args;
    int eval() override {
        int product = 1;
        for (auto &arg : args)
            product *= arg->eval();
        return product;
    }
};

struct Sub : Node {
    std::vector<std::unique_ptr<Node>> args;
    int eval() override {
        if (args.empty())
            throw std::runtime_error("- requires at least one argument");
        int result = args[0]->eval();
        if (args.size() == 1) {
            return -result;
        }
        for (size_t i = 1; i < args.size(); ++i)
            result -= args[i]->eval();
        return result;
    }
};

struct Div : Node {
    std::vector<std::unique_ptr<Node>> args;
    int eval() override {
        if (args.size() < 2)
            throw std::runtime_error("/ requires at least two arguments");
        int result = args[0]->eval();
        for (size_t i = 1; i < args.size(); ++i) {
            int divisor = args[i]->eval();
            if (divisor == 0)
                throw std::runtime_error("division by zero");
            result /= divisor;
        }
        return result;
    }
};

// Recursive s-expression traversal that builds tree of Nodes.
std::unique_ptr<Node> parse_tree(headerlisp::value it) {
    if (headerlisp::is_num(it)) {
        return std::make_unique<Num>(headerlisp::as_num_int(it));
    }

    // Must be a list
    if (!headerlisp::is_list(it)) {
        throw std::runtime_error("expected list or number");
    }

    // Get operator (first element)
    headerlisp::value op_val = headerlisp::first(it);
    if (!headerlisp::is_string(op_val)) {
        throw std::runtime_error("operator must be a symbol (string)");
    }
    std::string_view op = headerlisp::as_string_view(op_val);

    // Collect arguments (rest of the list)
    headerlisp::value rest = headerlisp::rest(it);
    std::vector<std::unique_ptr<Node>> args;
    while (!headerlisp::is_nil(rest)) {
        args.push_back(parse_tree(headerlisp::first(rest)));
        rest = headerlisp::rest(rest);
    }

    // Create appropriate node
    if (op == "+") {
        auto node = std::make_unique<Add>();
        node->args = std::move(args);
        return node;
    } else if (op == "*") {
        auto node = std::make_unique<Mul>();
        node->args = std::move(args);
        return node;
    } else if (op == "-") {
        auto node = std::make_unique<Sub>();
        node->args = std::move(args);
        return node;
    } else if (op == "/") {
        auto node = std::make_unique<Div>();
        node->args = std::move(args);
        return node;
    } else {
        throw std::runtime_error("unknown operator: " + std::string(op));
    }
}

// Convert string to interpreter tree.
std::unique_ptr<Node> parse(std::string_view input) {
    headerlisp::context_guard ctx{};
    headerlisp::value ast;
    try {
        ast = headerlisp::read(input);
    } catch (headerlisp::hl_exception &e) { throw std::runtime_error("reading error: " + std::string(e.what())); }
    return parse_tree(ast);
}

void test_variable_arity_parser() {
    printf("--- Variable-arity parser test ---\n");

    // Test cases
    struct TestCase {
        std::string input;
        int expected;
    } tests[] = {
        {"(+ 1 2 3 4)", 10},
        {"(* 2 3 4)", 24},
        {"(- 10 3 2)", 5},                       // 10-3-2 =5
        {"(- 5)", -5},                           // unary minus
        {"(/ 100 5 2)", 10},                     // 100/5/2 =10
        {"(+ (* 2 3) (- 10 4) 1)", (6 + 6 + 1)}, // 2*3=6, 10-4=6, +1 =13? Wait 10-4=6, 6+6+1=13? Actually 6+6+1=13.
                                                 // Check: (+ (* 2 3) (- 10 4) 1) = (+ 6 6 1) =13
        {"(* (+ 1 2) (- 5 2) 2)", (3 * 3 * 2)},
        {"(/ 100 (* 2 5) 2)", (100 / 10 / 2)},
        {"(+ 5)", 5}, // single argument addition
        {"(* 5)", 5}, // single argument multiplication
    };

    for (const auto &t : tests) {
        try {
            auto tree = parse(t.input);
            int result = tree->eval();
            TEST(result == t.expected, t.input + " evaluates correctly");
        } catch (const std::exception &e) { TEST(false, t.input + " threw exception"); }
    }

    // Error cases
    TEST_THROWS(parse("(unknown 1 2)"), "unknown operator");
    TEST_THROWS(parse("(+ 1 2 . 3)"), "improper list");
}
// Simple custom type for testing from_list conversion
struct Point {
    int x, y;
    
    bool operator==(Point other) const {
        return x == other.x && y == other.y;
    }
};

template <> struct headerlisp::to_list<Point> {
    auto operator()(const Point &p) { return headerlisp::list(p.x, p.y); }
};

template <> struct headerlisp::from_list<Point> {
    Point operator()(value lst) {
        auto [x, y] = headerlisp::first_2(lst);
        return {headerlisp::as_num_int(x), headerlisp::as_num_int(y)};
    }
};

void test_iterators() {
    printf("--- Iterator tests ---\n");

    // Basic iteration over homogeneous list
    value nums = headerlisp::list(1, 2, 3, 4, 5);
    std::vector<int> collected;
    for (int x : nums.iter().as<int>()) {
        collected.push_back(x);
    }
    TEST(collected == std::vector<int>({1, 2, 3, 4, 5}), "iter().as<int>() collects correctly");

    // Using std::copy with back_inserter
    std::vector<int> vec;
    auto it_begin = nums.iter().as<int>().begin();
    auto it_end = nums.iter().as<int>().end();
    std::copy(it_begin, it_end, std::back_inserter(vec));
    TEST(vec == std::vector<int>({1, 2, 3, 4, 5}), "std::copy works");

    // Using std::transform
    std::vector<int> squares;
    std::transform(it_begin, it_end, std::back_inserter(squares), [](int x) { return x * x; });
    TEST(squares == std::vector<int>({1, 4, 9, 16, 25}), "std::transform");

    // Using std::accumulate
    int sum = std::accumulate(it_begin, it_end, 0);
    TEST(sum == 15, "std::accumulate");

    // Using std::for_each
    int counter = 0;
    std::for_each(it_begin, it_end, [&counter](int) { ++counter; });
    TEST(counter == 5, "std::for_each");

    // Constructing standard containers from iterator range
    std::list<int> lst(it_begin, it_end);
    TEST(lst.size() == 5 && lst.front() == 1 && lst.back() == 5, "list constructor from iterators");

    // Test empty list iteration
    value empty = headerlisp::nil;
    auto empty_range = empty.iter().as<int>();
    TEST(empty_range.begin() == empty_range.end(), "empty list iterators equal");

    // Test heterogeneous list: iter().as<T>() should throw if element not convertible
    value mixed = headerlisp::list(1, "hello", 3.14);
    bool threw = false;
    try {
        for (int x : mixed.iter().as<int>()) {
            (void)x;
        }
    } catch (...) { threw = true; }
    TEST(threw, "heterogeneous list throws in as<int> iteration");

    // But iter() without .as<T>() works (yields value objects)
    int count = 0;
    for (value _ : mixed.iter()) {
        ++count;
    }
    TEST(count == 3, "iter() on heterogeneous list counts elements");

    // Test .as<Point>() with custom type
    value points = headerlisp::list(Point{1, 2}, Point{3, 4}, Point{5, 6});
    std::vector<Point> points_vec;
    for (const Point &p : points.iter().as<Point>()) {
        points_vec.push_back(p);
    }
    TEST(points_vec.size() == 3, "points count");
    TEST(points_vec[0].x == 1 && points_vec[0].y == 2, "first point");
    TEST(points_vec[2].x == 5 && points_vec[2].y == 6, "last point");

    // Using .as<Point>() with algorithms
    std::vector<Point> points2;
    std::copy(points.iter().as<Point>().begin(), points.iter().as<Point>().end(), std::back_inserter(points2));
    TEST(!!std::equal(points2.begin(), points2.end(), points_vec.begin()), "copy points");

    int fold_sum = headerlisp::foldl([](value x, int acc) { return headerlisp::as_num_int(x) + acc; }, 0, nums);
    TEST(fold_sum == 15, "foldl with iter()");
}

int main() {
    printf("Running tests for Lisp list library...\n\n");

    test_type_checkers();
    test_creation_and_access();
    test_optional_accessors();
    test_list_manipulation();
    test_higher_order_functions();
    test_predicates();
    test_assoc_and_member();
    test_unsafe_functions_and_mutators();
    test_heterogeneous_and_iteration();
    test_map_filter_numeric();
    test_map_filter_heterogeneous();
    test_invalid_uses();
    test_arithmetic_operators();
    test_serialization_complex();
    test_variable_arity_parser();
    test_iterators();

    printf("\n=== Test summary ===\n");
    printf("Tests run: %d\n", tests_run);
    printf("Tests passed: %d\n", tests_run - tests_failed);
    printf("Tests failed: %d\n", tests_failed);
    if (!failures.empty()) {
        printf("\nFailures:\n");
        for (const auto &f : failures) {
            printf("  %s\n", f.c_str());
        }
    }
    return tests_failed == 0 ? 0 : 1;
}