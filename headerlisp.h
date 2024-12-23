#ifndef HEADERLISP_H
#define HEADERLISP_H

#include <assert.h>
#include <cctype>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <new>
#include <stddef.h>
#include <stdexcept>
#include <stdint.h>
#include <string_view>
#include <type_traits>

namespace headerlisp {

class value;
class value_range;

class value {
    struct constructor_tag {};

public:
    constexpr value();
    constexpr static value make(uint64_t x) { return value(constructor_tag{}, x); }
    value(double num) {
        uint64_t x;
        memcpy(&x, &num, sizeof(value));
        *this = make(x);
    }

    constexpr uint64_t internal() const { return u64_; }

    double unsafe_f64() const {
        double result;
        memcpy(&result, &u64_, sizeof(value));
        return result;
    }

    value_range range();

private:
    constexpr value(constructor_tag, uint64_t x) : u64_(x) {}

    uint64_t u64_;
};

enum class value_kind : uint8_t {
    // Singleton values. These are not heap-allocated.
    num = 0x0,
    nil = 0x1,
    tru = 0x2,
    // Heap-allocated garbage-collected values
    cons = 0x3,
    string = 0x4,
};

struct obj {
    value_kind kind;
    char _align[7];
    char as[1];
};

struct obj_cons {
    value car;
    value cdr;
};

struct obj_env {
    value vars;
    value up;
};

struct obj_str {
    size_t length;
    uint32_t hash;
    char str[1];
};

class hl_exception : public std::exception {
public:
    hl_exception() = delete;
    explicit hl_exception(const char *reason) { snprintf(msg_, sizeof(msg_), "%s", reason); }
    explicit hl_exception(std::string_view reason) { snprintf(msg_, sizeof(msg_), "%s", reason.data()); }
    template <typename... Args> explicit hl_exception(std::format_string<Args...> fmt, Args &&...args) {
        std::format_to_n(msg_, sizeof(msg_), fmt, std::forward<Args>(args)...);
    }

    const char *what() const noexcept override { return msg_; }

private:
    char msg_[4096];
};

value new_string(const char *symbol, size_t length);
value new_stringz(const char *symbol);
value new_cons(value car, value cdr);

constexpr uint64_t HL_SIGN_BIT = ((uint64_t)1 << 63);
constexpr uint64_t HL_QNAN = (uint64_t)0x7ffc000000000000;

constexpr inline value nan_box_singleton(value_kind kind) {
    return value::make(HL_QNAN | (uint64_t)kind);
}

constexpr inline uint8_t nan_unbox_singleton(value v) {
    return v.internal() & ~(HL_QNAN);
}

inline value nan_box_ptr(void *ptr) {
    return value::make(((uintptr_t)ptr) | (HL_SIGN_BIT | HL_QNAN));
}

inline obj *nan_unbox_ptr(value value) {
    return (obj *)(uintptr_t)(value.internal() & ~(HL_SIGN_BIT | HL_QNAN));
}

constexpr value nil = nan_box_singleton(value_kind::nil);
constexpr value tru = nan_box_singleton(value_kind::tru);

constexpr value::value() {
    *this = nil;
}

constexpr inline bool is_num(value value) {
    return (value.internal() & HL_QNAN) != HL_QNAN;
}
constexpr inline bool is_obj(value value) {
    return ((value.internal() & (HL_QNAN | HL_SIGN_BIT)) == (HL_QNAN | HL_SIGN_BIT));
}
constexpr inline value_kind get_value_kind(value x) {
    return is_obj(x) ? nan_unbox_ptr(x)->kind : (value_kind)nan_unbox_singleton(x);
}

constexpr inline bool is_nil(value x) {
    return x.internal() == nil.internal();
}
constexpr inline bool is_true(value x) {
    return x.internal() == tru.internal();
}

constexpr inline bool is_cons(value value) {
    return is_obj(value) && nan_unbox_ptr(value)->kind == value_kind::cons;
}

constexpr inline bool is_string(value x) {
    return is_obj(x) && nan_unbox_ptr(x)->kind == value_kind::string;
}

constexpr inline bool is_list(value x) {
    return is_cons(x) || is_nil(x);
}

inline struct obj_cons *unwrap_cons(value value) {
    assert(is_obj(value));
    obj *obj = nan_unbox_ptr(value);
    assert(obj->kind == value_kind::cons);
    return (obj_cons *)obj->as;
}

inline obj_str *unwrap_string(value value) {
    assert(is_obj(value));
    obj *obj = nan_unbox_ptr(value);
    assert(obj->kind == value_kind::string);
    return (obj_str *)obj->as;
}
inline const char *unwrap_zstring(value value) {
    return unwrap_string(value)->str;
}
inline std::string_view unwrap_string_view(value value) {
    obj_str *str = unwrap_string(value);
    return {str->str, str->length};
}

inline value &unwrap_cdr(value value) {
    assert(is_obj(value));
    obj *obj = nan_unbox_ptr(value);
    assert(obj->kind == value_kind::cons);
    return ((obj_cons *)obj->as)->cdr;
}
inline value &unwrap_car(value value) {
    assert(is_obj(value));
    obj *obj = nan_unbox_ptr(value);
    assert(obj->kind == value_kind::cons);
    return ((obj_cons *)obj->as)->car;
}

inline double unwrap_num(value value) {
    assert(is_num(value));
    double result;
    memcpy(&result, &value, sizeof(value));
    return result;
}
inline obj *unwrap_obj(value value) {
    assert(is_obj(value));
    return nan_unbox_ptr(value);
}
inline void unwrap_setcar(value cons, value car) {
    unwrap_cons(cons)->car = car;
}
inline void unwrap_setcdr(value cons, value cdr) {
    unwrap_cons(cons)->cdr = cdr;
}

constexpr std::string_view value_kind_str(value_kind kind) {
    switch (kind) {
    case value_kind::num: return "num";
    case value_kind::nil: return "nil";
    case value_kind::tru: return "true";
    case value_kind::cons: return "cons";
    case value_kind::string: return "string";
    }
    __builtin_unreachable();
}
constexpr std::string_view value_kind_str(value value) {
    return value_kind_str(get_value_kind(value));
}

constexpr inline std::string_view string_view(value x) {
    if (!is_string(x))
        throw hl_exception("called 'symb_view()' on non-symbol value {}", value_kind_str(x));
    obj_str *str = unwrap_string(x);
    return std::string_view{str->str, str->length};
}
inline double num_dbl(value x) {
    if (!is_num(x))
        throw hl_exception("called 'num_dbl() on non-symbol value {}", value_kind_str(x));
    return unwrap_num(x);
}
inline int64_t num_i64(value x) {
    return (int64_t)num_dbl(x);
}
inline int num_int(value x) {
    return (int)num_i64(x);
}

inline value &car(value x) {
    if (!is_cons(x))
        throw hl_exception("called 'car()' on non-cons value {}", value_kind_str(x));
    return unwrap_car(x);
}
inline value &cdr(value x) {
    if (!is_cons(x))
        throw hl_exception("called 'car()' on non-cons value {}", value_kind_str(x));
    return unwrap_cdr(x);
}
// clang-format off
inline value caar(value x) { return car(car(x)); }
inline value cadr(value x) { return car(cdr(x)); }
inline value cdar(value x) { return cdr(car(x)); }
inline value cddr(value x) { return cdr(cdr(x)); }
inline value caaar(value x) { return car(car(car(x))); }
inline value caadr(value x) { return car(car(cdr(x))); }
inline value cadar(value x) { return car(cdr(car(x))); }
inline value caddr(value x) { return car(cdr(cdr(x))); }
inline value cdaar(value x) { return cdr(car(car(x))); }
inline value cdadr(value x) { return cdr(car(cdr(x))); }
inline value cddar(value x) { return cdr(cdr(car(x))); }
inline value cdddr(value x) { return cdr(cdr(cdr(x))); }
inline value caaaar(value x) { return car(car(car(car(x)))); }
inline value caaadr(value x) { return car(car(car(cdr(x)))); }
inline value caadar(value x) { return car(car(cdr(car(x)))); }
inline value caaddr(value x) { return car(car(cdr(cdr(x)))); }
inline value cadaar(value x) { return car(cdr(car(car(x)))); }
inline value cadadr(value x) { return car(cdr(car(cdr(x)))); }
inline value caddar(value x) { return car(cdr(cdr(car(x)))); }
inline value cadddr(value x) { return car(cdr(cdr(cdr(x)))); }
inline value cdaaar(value x) { return cdr(car(car(car(x)))); }
inline value cdaadr(value x) { return cdr(car(car(cdr(x)))); }
inline value cdadar(value x) { return cdr(car(cdr(car(x)))); }
inline value cdaddr(value x) { return cdr(car(cdr(cdr(x)))); }
inline value cddaar(value x) { return cdr(cdr(car(car(x)))); }
inline value cddadr(value x) { return cdr(cdr(car(cdr(x)))); }
inline value cdddar(value x) { return cdr(cdr(cdr(car(x)))); }
inline value cddddr(value x) { return cdr(cdr(cdr(cdr(x)))); }

inline value head(value x) { return car(x); }
inline value rest(value x) { return cdr(x); }
inline value first(value x) { return car(x); }
inline value second(value x) { return cadr(x); }
inline value third(value x) { return caddr(x); }
inline value fourth(value x) { return cadddr(x); }
inline value fifth(value x) { return car(cddddr(x)); }
inline value sixth(value x) { return cadr(cddddr(x)); }
inline value seventh(value x) { return caddr(cddddr(x)); }
inline value eighth(value x) { return cadddr(cddddr(x)); }
inline value ninth(value x) { return car(cdddr(cddddr(x))); }
inline value tenth(value x) { return cadr(cdddr(cddddr(x))); }

// clang-format on

inline bool operator==(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'==' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'==' called on non-number {}", value_kind_str(right));
    return left.unsafe_f64() == right.unsafe_f64();
}
inline bool operator==(value left, double x) {
    if (!is_num(left))
        throw hl_exception("'==' called on non-number {}", value_kind_str(left));
    return left.unsafe_f64() == x;
}

inline bool operator!=(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'!=' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'!=' called on non-number {}", value_kind_str(right));
    return left.unsafe_f64() != right.unsafe_f64();
}
inline bool operator!=(value left, double x) {
    if (!is_num(left))
        throw hl_exception("'!=' called on non-number {}", value_kind_str(left));
    return left.unsafe_f64() != x;
}

inline bool is_equal(value left, value right) {
    value_kind kind = get_value_kind(left);
    if (get_value_kind(right) != kind)
        return false;
    switch (kind) {
    case value_kind::num: return left == right;
    case value_kind::nil: return is_nil(left);
    case value_kind::tru: return is_true(left);
    case value_kind::cons:
        return is_equal(unwrap_car(left), unwrap_car(right)) && is_equal(unwrap_cdr(left), unwrap_cdr(right));
    case value_kind::string: {
        obj_str *left_str = unwrap_string(left);
        obj_str *right_str = unwrap_string(right);
        return left_str->length == right_str->length && left_str->hash == right_str->hash &&
               memcmp(left_str->str, right_str->str, left_str->length) == 0;
    }
    }
    __builtin_unreachable();
}

class value_iter {
public:
    using difference_type = ptrdiff_t;
    using value_type = value;
    using pointer = value *;
    using reference = value &;
    using iterator_category = std::forward_iterator_tag;

    value_iter() = delete;
    value_iter(const value_iter &) = default;
    value_iter(value_iter &&) = default;
    value_iter &operator=(const value_iter &) = default;
    value_iter &operator=(value_iter &&) = default;

    explicit value_iter(value x) : current_(x) {}

    reference operator*() { return car(current_); }
    bool operator==(value_iter other) { return current_.internal() == other.current_.internal(); }

    value_iter operator++(int) { return value_iter(cdr(current_)); }
    value_iter &operator++() {
        current_ = cdr(current_);
        return *this;
    }

private:
    value current_;
};

class value_range {
public:
    value_range() = delete;
    value_range(const value_range &) = default;
    value_range(value_range &&) = default;
    value_range &operator=(const value_range &) = default;
    value_range &operator=(value_range &&) = default;

    explicit value_range(value x) : start_(x) {}

    value_iter begin() { return value_iter{start_}; }
    value_iter end() { return value_iter{nil}; }

private:
    value start_;
};

inline value_range value::range() {
    return value_range(*this);
}

inline value nth(value lst, size_t idx) {
    while (idx--) {
        lst = cdr(lst);
    }
    return car(lst);
}

inline size_t length(value lst) {
    size_t len = 0;
    for (auto it : lst.range()) {
        (void)it;
        ++len;
    }
    return len;
}

inline void add_last(value &first, value &last, value x) {
    if (is_nil(last)) {
        first = last = new_cons(x, nil);
    } else {
        value new_last = new_cons(x, nil);
        unwrap_setcdr(last, new_last);
        last = new_last;
    }
}

template <typename T>
    requires std::convertible_to<T, double>
inline value make_value(T x) {
    return value(static_cast<double>(x));
}
inline value make_value(nullptr_t) {
    return nil;
}
inline value make_value(std::string_view s) {
    return new_string(s.begin(), s.length());
}
inline value make_value(value x) {
    return x;
}

template <typename... Args> inline value list(Args &&...args) {
    value first = nil;
    value last = nil;
    (add_last(first, last, make_value(std::forward<Args>(args))), ...);
    return first;
}

inline value append(value a, value b) {
    value result = nil;
    value result_tail = nil;
    for (value x : a.range()) {
        add_last(result, result_tail, x);
    }
    for (value x : b.range()) {
        add_last(result, result_tail, x);
    }
    return result;
}
inline value append(value a, value b, value c) {
    value result = nil;
    value result_tail = nil;
    for (value x : a.range()) {
        add_last(result, result_tail, x);
    }
    for (value x : b.range()) {
        add_last(result, result_tail, x);
    }
    for (value x : c.range()) {
        add_last(result, result_tail, x);
    }
    return result;
}

template <typename F> inline value map(F f, value lst) {
    auto new_lst = nil;
    auto new_lst_end = nil;
    for (auto it : lst.range()) {
        add_last(new_lst, new_lst_end, make_value(f(it)));
    }
    return new_lst;
}
template <typename F> inline value map(F f, value lst1, value lst2) {
    auto new_lst = nil;
    auto new_lst_end = nil;
    for (; is_cons(lst1) && is_cons(lst2); lst1 = cdr(lst1), lst2 = cdr(lst2)) {
        auto it1 = car(lst1);
        auto it2 = car(lst2);
        add_last(new_lst, new_lst_end, make_value(f(it1, it2)));
    }
    return new_lst;
}
template <typename F> inline bool all(F f, value lst) {
    for (auto it : lst.range()) {
        if (!f(it)) {
            return false;
        }
    }
    return true;
}
template <typename F> inline bool any(F f, value lst) {
    for (auto it : lst.range()) {
        if (f(it)) {
            return true;
        }
    }
    return false;
}

template <typename F> inline value foldl(F f, value init, value lst) {
    value result = init;
    for (auto it : lst.range()) {
        result = make_value(f(it, result));
    }
    return result;
}
template <typename F> inline value foldl(F f, value init, value lst1, value lst2) {
    value result = init;
    for (; is_cons(lst1) && is_cons(lst2); lst1 = cdr(lst1), lst2 = cdr(lst2)) {
        auto it1 = car(lst1);
        auto it2 = car(lst2);
        result = f(it1, it2, result);
    }
    return result;
}
template <typename F> inline value foldr(F f, value init, value lst) {
    if (is_nil(lst)) {
        return init;
    }
    return f(car(lst), foldr(f, init, cdr(lst)));
}
template <typename F> inline value foldr(F f, value init, value lst1, value lst2) {
    if (is_nil(lst1) || is_nil(lst2)) {
        return init;
    }
    return f(car(lst1), car(lst2), foldr(f, init, cdr(lst1), cdr(lst2)));
}
inline value reverse(value lst) {
    value result = nil;
    for (auto it : lst.range()) {
        result = new_cons(it, result);
    }
    return result;
}

template <typename F> inline value filter(F f, value lst) {
    value new_lst = nil;
    value new_lst_end = nil;
    for (auto it : lst.range()) {
        if (f(it)) {
            add_last(new_lst, new_lst_end, it);
        }
    }
    return new_lst;
}

template <typename F> inline value filter_not(F f, value lst) {
    return filter([&f](auto x) { return !f(x); }, lst);
}

template <typename F> inline value remove(value x, value lst, F f) {
    value new_lst = nil;
    value new_lst_end = nil;
    for (auto it : lst.range()) {
        if (!f(x, it)) {
            add_last(new_lst, new_lst_end, it);
        }
    }
    return new_lst;
}
inline value remove(value x, value lst) {
    return remove(x, lst, is_equal);
}

inline value cartesian_product(value lst1, value lst2) {
    value head = nil, tail = nil;
    for (auto it1 : lst1.range()) {
        for (auto it2 : lst2.range()) {
            add_last(head, tail, list(it1, it2));
        }
    }
    return head;
}
inline value cartesian_product(value lst1, value lst2, value lst3) {
    value head = nil, tail = nil;
    for (auto it1 : lst1.range()) {
        for (auto it2 : lst2.range()) {
            for (auto it3 : lst3.range()) {
                add_last(head, tail, list(it1, it2, it3));
            }
        }
    }
    return head;
}

inline bool is_even(value x) {
    if (!is_num(x))
        throw hl_exception("'is_even' called on non-number {}", value_kind_str(x));
    return ((int64_t)unwrap_num(x) & 2) == 0;
}
inline bool is_odd(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return ((int64_t)unwrap_num(x) & 2) != 0;
}
inline bool is_zero(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return unwrap_num(x) == 0;
}
inline bool is_positive(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return unwrap_num(x) > 0;
}
inline bool is_negative(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return unwrap_num(x) < 0;
}

inline value operator+(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'+' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'+' called on non-number {}", value_kind_str(right));
    return make_value(left.unsafe_f64() + right.unsafe_f64());
}
inline value operator-(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'-' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'-' called on non-number {}", value_kind_str(right));
    return make_value(left.unsafe_f64() - right.unsafe_f64());
}
inline value operator*(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'*' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'*' called on non-number {}", value_kind_str(right));
    return make_value(left.unsafe_f64() * right.unsafe_f64());
}
inline value operator/(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'/' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'/' called on non-number {}", value_kind_str(right));
    return make_value(left.unsafe_f64() / right.unsafe_f64());
}

inline value assoc(value v, value lst) {
    for (auto it : lst.range()) {
        if (is_equal(v, car(it))) {
            return it;
        }
    }
    return nil;
}

template <typename F> inline std::optional<size_t> index_of(value lst, value v, F f) {
    size_t idx = 0;
    for (auto it : lst.range()) {
        if (f(v, it)) {
            return idx;
        }
        ++idx;
    }
    return std::nullopt;
}
inline std::optional<size_t> index_of(value lst, value v) {
    return index_of(lst, v, is_equal);
}
template <typename F> inline bool member(value v, value lst, F f) {
    return index_of(lst, v, f).has_value();
}
inline bool member(value v, value lst) {
    return index_of(lst, v).has_value();
}

inline value range(size_t end) {
    value head = nil, tail = nil;
    for (size_t i = 0; i < end; ++i) {
        add_last(head, tail, make_value(i));
    }
    return head;
}
inline value range(double start, double end, double step = 1.0) {
    value head = nil, tail = nil;
    for (double it = start; it < end; it += step) {
        add_last(head, tail, make_value(it));
    }
    return head;
}
template <typename F> inline value build_list(size_t n, F f) {
    value head = nil, tail = nil;
    for (size_t i = 0; i < n; ++i) {
        add_last(head, tail, make_value(f(i)));
    }
    return head;
}

struct context {
    char *memory;
    size_t memory_used;
    size_t memory_reserved;
};

namespace internal {

inline thread_local context g_ctx{nullptr, 0, 0};

inline uint32_t djb2(const char *src, const char *dst) {
    uint32_t hash = 5381;
    do {
        int c = *src++;
        hash = ((hash << 5) + hash) + c;
    } while (src != dst);
    return hash;
}

inline void *alloc(context *ctx, size_t size) {
    size_t size_aligned = (size + 15) / 16 * 16;
    assert(!(ctx->memory_used & 15));
    if (ctx->memory_used + size_aligned > ctx->memory_reserved)
        throw std::bad_alloc{};

    void *result = ctx->memory + ctx->memory_used;
    ctx->memory_used += size_aligned;
    return result;
}

inline value new_string(context *ctx, const char *value, size_t length) {
    assert(value != NULL);
    assert(length != 0);
    assert(length < UINT32_MAX);

    void *memory = alloc(ctx, offsetof(obj, as) + offsetof(obj_str, str) + length + 1);
    obj *header = (obj *)memory;
    header->kind = value_kind::string;

    obj_str *str = (obj_str *)(void *)(header->as);
    str->length = length;
    str->hash = djb2(value, value + length);
    memcpy(str->str, value, length);
    str->str[length] = '\0';

    return nan_box_ptr(header);
}

inline value new_cons(context *ctx, value car, value cdr) {
    void *memory = alloc(ctx, offsetof(obj, as) + sizeof(obj_cons));
    obj *header = (obj *)memory;
    header->kind = value_kind::cons;
    obj_cons *cons = (obj_cons *)(void *)(header->as);
    cons->car = car;
    cons->cdr = cdr;
    return nan_box_ptr(header);
}

enum class token_kind {
    end,
    num,
    symb,
    string,
    dot,
    tru,
    lparen,
    rparen,
    unexpected
};

struct token {
    token_kind kind;
    size_t offset;
    size_t length;
    double f64;
};

inline bool is_symbol_breaker(int c) {
    return isspace(c) || c == ';' || c == '(' || c == ')' || !isprint(c) || c == '"';
}

struct lexer {
    const char *input;
    const char *end;
    const char *cursor;
    token next;

    void advance() {
        const char *token_start = cursor;
        next.offset = token_start - input;
        for (;;) {
            if (cursor >= end) {
                next.kind = token_kind::end;
                return;
            }
            int c = *cursor++;

            // Spaces
            if (is_symbol_breaker(c)) {
                if (isspace(c)) {
                    while (isspace(*cursor)) {
                        ++cursor;
                    }
                    token_start = cursor;
                    next.offset = token_start - input;
                    continue;
                }

                // Parens
                if (c == '(') {
                    next.kind = token_kind::lparen;
                    return;
                } else if (c == ')') {
                    next.kind = token_kind::rparen;
                    return;
                }

                if (c == '"') {
                    for (;;) {
                        if (cursor >= end) {
                            throw hl_exception("unterminated string literal");
                        }
                        if (*cursor == '"') {
                            ++cursor;
                            break;
                        }
                        ++cursor;
                    }
                    next.kind = token_kind::string;
                    next.length = cursor - token_start;
                    return;
                }

                // Comment
                if (c == ';') {
                    while (cursor < end && *cursor != '\n') {
                        ++cursor;
                    }
                    if (*cursor == '\n') {
                        ++cursor;
                    }
                    token_start = cursor;
                    next.offset = token_start - input;
                    continue;
                }

                // other bogus stuff
                for (;;) {
                    c = *cursor++;
                    if (isprint(c)) {
                        --cursor;
                        break;
                    }
                }
                next.kind = token_kind::unexpected;
                next.length = cursor - token_start;
                return;
            }

            for (;;) {
                c = *cursor++;
                if (is_symbol_breaker(c)) {
                    --cursor;
                    break;
                }
            }
            size_t length = cursor - token_start;
            if (length == 1 && *token_start == '.') {
                next.kind = token_kind::dot;
                return;
            } else if (length == 1 && *token_start == 't') {
                next.kind = token_kind::tru;
                return;
            }

            char *strtod_end = nullptr;
            double f64 = strtod(token_start, &strtod_end);
            if (strtod_end == cursor) {
                next.kind = token_kind::num;
                next.f64 = f64;
                return;
            }

            next.kind = token_kind::symb;
            next.length = cursor - token_start;
            return;
        }
    }
};

struct reader {
    lexer &lex;
    const token &tok;
    bool should_return_old_token = false;

    reader(lexer &lex) : lex(lex), tok(lex.next) {}

    void peek_token() {
        if (should_return_old_token) {
            return;
        }
        for (;;) {
            lex.advance();
            if (tok.kind == token_kind::unexpected) {
                throw hl_exception("unexpected token");
            }
            break;
        }
        should_return_old_token = true;
    }

    void eat_token() { should_return_old_token = false; }

    value read_list() {
        peek_token();
        // This should be guaranteed by caller.
        assert(tok.kind == token_kind::lparen);
        eat_token();

        peek_token();
        // Handle nil
        if (tok.kind == token_kind::rparen) {
            eat_token();
            return nil;
        }

        value list_head, list_tail;
        list_head = list_tail = new_cons(read_expr(), nil);
        // Now enter the loop of parsing other list elements.
        for (;;) {
            peek_token();
            if (tok.kind == token_kind::end) {
                throw hl_exception("Missing closing paren when reading list (eof encountered)");
            }
            if (tok.kind == token_kind::rparen) {
                eat_token();
                return list_head;
            }
            if (tok.kind == token_kind::dot) {
                eat_token();
                unwrap_setcdr(list_tail, read_expr());
                peek_token();
                if (tok.kind != token_kind::rparen) {
                    throw hl_exception("Missing closing paren after dot when reading list");
                }
                return list_head;
            }

            value ast = read_expr();
            value cons = new_cons(ast, nil);
            unwrap_setcdr(list_tail, cons);
            list_tail = cons;
        }
    }

    value read_expr() {
        peek_token();
        switch (tok.kind) {
        case token_kind::end: break;
        case token_kind::num: eat_token(); return make_value(tok.f64);
        case token_kind::tru: eat_token(); return tru;
        case token_kind::symb: eat_token(); return new_string(&g_ctx, lex.input + tok.offset, tok.length);
        case token_kind::string:
            eat_token();
            assert(lex.input[tok.offset] == '"');
            assert(lex.input[tok.offset + tok.length - 1] == '"');
            assert(tok.length >= 2);
            return new_string(&g_ctx, lex.input + tok.offset + 1, tok.length - 2);
        case token_kind::lparen: return read_list(); break;
        case token_kind::dot: throw hl_exception("unexpected ."); break;
        case token_kind::rparen: throw hl_exception("stray )"); break;
        case token_kind::unexpected: break;
        }
        __builtin_unreachable();
    }
};

} // namespace internal

inline void set_context(context ctx) {
    internal::g_ctx = ctx;
}

inline value new_cons(value car, value cdr) {
    return internal::new_cons(&internal::g_ctx, car, cdr);
}
inline value new_string(const char *value, size_t length) {
    return internal::new_string(&internal::g_ctx, value, length);
}

inline value read(const char *s, size_t length) {
    internal::lexer lexer{s, s + length, s, {}};
    internal::reader reader{lexer};

    return reader.read_expr();
}
inline value read(std::string_view s) {
    return read(s.data(), s.length());
}

inline std::string print(value x) {
    switch (get_value_kind(x)) {
    case value_kind::num: return std::format("{:g}", x.unsafe_f64());
    case value_kind::nil: return "()";
    case value_kind::tru: return "t";
    case value_kind::cons: {
        std::string result = "(";
        while (is_cons(x)) {
            result += print(unwrap_car(x));

            value cdr = unwrap_cdr(x);
            if (!is_list(cdr)) {
                result += " . ";
                result += print(cdr);
                break;
            }
            if (!is_nil(cdr)) {
                result += " ";
            }
            x = cdr;
        }
        return result + ")";
    }
    case value_kind::string: return std::format("\"{}\"", unwrap_string_view(x));
    }
}

} // namespace headerlisp

#endif