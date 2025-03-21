//
// headerlisp.h
// https://github.com/holodome/headerlisp
//
// MIT License
//
// Copyright (c) 2024 Ilya Vinogradov
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
#ifndef HEADERLISP_H
#define HEADERLISP_H

#include <assert.h>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <format>
#include <iterator>
#include <new>
#include <optional>
#include <stddef.h>
#include <stdint.h>
#include <string_view>
#include <type_traits>

namespace headerlisp {

//
// Support stuff
//

struct context {
    char *memory = nullptr;
    size_t memory_used = 0;
    size_t memory_reserved = 0;
};
inline void set_context(context ctx);

class context_guard {
public:
    context_guard(const context_guard &) = delete;
    context_guard(context_guard &&) = default;
    context_guard &operator=(const context_guard &) = delete;
    context_guard &operator=(context_guard &&) = default;

    context_guard() : context_guard(1 << 20) {}
    explicit context_guard(size_t size) : memory_(static_cast<char *>(malloc(size))), memory_size_(size) {
        if (memory_ == nullptr)
            throw std::bad_alloc{};
        set_context(context{memory_, 0, memory_size_});
    }
    ~context_guard() { set_context(context{}); }

private:
    char *memory_ = nullptr;
    size_t memory_size_ = 0;
};

//
// Base types
//

class value_range;

enum class value_kind : uint8_t {
    // Singleton values. These are not heap-allocated.
    num = 0x0,
    nil = 0x1,
    tru = 0x2,
    // Heap-allocated values
    cons = 0x3,
    string = 0x4,
};

// Disallow calling functions without namespace due to namespace lookup rules
namespace value_ns {

class value {
    struct constructor_tag {};

public:
    constexpr value();
    constexpr value(const value &) noexcept = default;
    constexpr value(value &&) noexcept = default;
    constexpr value &operator=(const value &) noexcept = default;
    constexpr value &operator=(value &&) noexcept = default;

    constexpr static value make(uint64_t x) noexcept { return value(constructor_tag{}, x); }
    constexpr uint64_t internal() const noexcept { return u64_; }

    value_range iter() const noexcept;

private:
    constexpr value(constructor_tag, uint64_t x) noexcept : u64_(x) {}

    uint64_t u64_;
};

} // namespace value_ns

using value = value_ns::value;

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

template <typename T> struct to_list {};
template <typename T> struct from_list {};
template <typename T> struct list_tag {};

template <typename T>
concept has_to_list = requires(T a) {
    { to_list<std::remove_cvref_t<T>>{}(a) } -> std::same_as<value>;
};
template <typename T>
concept has_from_list = requires {
    { from_list<std::remove_cvref_t<T>>{}(value{}) } -> std::same_as<T>;
};
template <typename T>
concept has_list_tag = requires {
    { list_tag<std::remove_cvref_t<T>>::tag } -> std::same_as<const std::string_view &>;
};

template <typename T>
    requires(std::constructible_from<std::string_view, T> && !std::same_as<T, nullptr_t>)
struct list_tag<T> {
    constexpr static inline std::string_view tag = "string";
};
template <> struct list_tag<double> {
    constexpr static inline std::string_view tag = "num";
};
template <> struct list_tag<int> {
    constexpr static inline std::string_view tag = "num";
};

template <typename F, typename U>
concept fold_function = std::is_invocable_r_v<U, F, value, U>;

template <typename F, typename U>
concept fold2_function = std::is_invocable_r_v<U, F, value, value, U>;

template <typename F>
concept map_function = std::is_invocable_r_v<value, F, value>;

template <typename F>
concept map2_function = std::is_invocable_r_v<value, F, value, value>;

template <typename F>
concept predicate_function = std::is_invocable_r_v<bool, F, value>;

//
// Value constructors
//

// There are more overloads for these functions below
inline value make_value(value x) noexcept;
template <has_list_tag T> value make_tagged_value(T &&t);

inline value new_string(const char *str, size_t length);
inline value new_stringz(const char *str);
inline value cons(value car, value cdr);

template <typename... Args> value list(Args &&...args);
template <has_list_tag... Args> value tagged_list(Args &&...args);

//
// Type checkers
//
inline std::string_view value_kind_str(value_kind kind) noexcept;
inline std::string_view value_kind_str(value x) noexcept;
inline value_kind get_value_kind(value x) noexcept;
inline bool is_num(value x) noexcept;
inline bool is_obj(value x) noexcept;
inline bool is_nil(value x) noexcept;
inline bool is_true(value x) noexcept;
inline bool is_cons(value x) noexcept;
inline bool is_string(value x) noexcept;
inline bool is_list(value x) noexcept;

//
// Unsafe functions that unwrap inner storage with assumption that type is correct
//
inline std::string_view unwrap_string_view(value x) noexcept;
inline value &unwrap_car(value x) noexcept;
inline value &unwrap_cdr(value x) noexcept;
inline double unwrap_f64(value x) noexcept;
// Unsafe cons mutators
inline void unwrap_setcar(value c, value car) noexcept;
inline void unwrap_setcdr(value c, value cdr) noexcept;

//
// Checked data access
//
inline std::string_view as_string_view(value x);
inline double as_num_f64(value x);
inline int as_num_int(value x);
inline value &car(value x);
inline value &cdr(value x);

struct cons_unapply_result {
    value &car, &cdr;
};
inline cons_unapply_result unapply_cons(value x);

//
// Library list accessors
//
inline size_t length(value lst);

inline value caar(value x);
inline value cadr(value x);
inline value cdar(value x);
inline value cddr(value x);
inline value caaar(value x);
inline value caadr(value x);
inline value cadar(value x);
inline value caddr(value x);
inline value cdaar(value x);
inline value cdadr(value x);
inline value cddar(value x);
inline value cdddr(value x);
inline value caaaar(value x);
inline value caaadr(value x);
inline value caadar(value x);
inline value caaddr(value x);
inline value cadaar(value x);
inline value cadadr(value x);
inline value caddar(value x);
inline value cadddr(value x);
inline value cdaaar(value x);
inline value cdaadr(value x);
inline value cdadar(value x);
inline value cdaddr(value x);
inline value cddaar(value x);
inline value cddadr(value x);
inline value cdddar(value x);
inline value cddddr(value x);

inline value head(value x);
inline value rest(value x);
inline value first(value x);
inline value second(value x);
inline value third(value x);
inline value fourth(value x);
inline value fifth(value x);
inline value sixth(value x);
inline value seventh(value x);
inline value eighth(value x);
inline value ninth(value x);
inline value tenth(value x);

inline value nth(value lst, size_t idx);
inline value nthcdr(value lst, size_t idx);

struct list_bind_2 {
    value a, b;
};
struct list_bind_3 {
    value a, b, c;
};
struct list_bind_4 {
    value a, b, c, d;
};

inline list_bind_2 first_2(value x);
inline list_bind_3 first_3(value x);
inline list_bind_4 first_4(value x);

//
// Optional list accessors
//
inline std::optional<value> car_opt(value x);
inline std::optional<value> cdr_opt(value x);
inline std::optional<value> caar_opt(value x);
inline std::optional<value> cadr_opt(value x);
inline std::optional<value> cdar_opt(value x);
inline std::optional<value> cddr_opt(value x);
inline std::optional<value> caaar_opt(value x);
inline std::optional<value> caadr_opt(value x);
inline std::optional<value> cadar_opt(value x);
inline std::optional<value> caddr_opt(value x);
inline std::optional<value> cdaar_opt(value x);
inline std::optional<value> cdadr_opt(value x);
inline std::optional<value> cddar_opt(value x);
inline std::optional<value> cdddr_opt(value x);
inline std::optional<value> caaaar_opt(value x);
inline std::optional<value> caaadr_opt(value x);
inline std::optional<value> caadar_opt(value x);
inline std::optional<value> caaddr_opt(value x);
inline std::optional<value> cadaar_opt(value x);
inline std::optional<value> cadadr_opt(value x);
inline std::optional<value> caddar_opt(value x);
inline std::optional<value> cadddr_opt(value x);
inline std::optional<value> cdaaar_opt(value x);
inline std::optional<value> cdaadr_opt(value x);
inline std::optional<value> cdadar_opt(value x);
inline std::optional<value> cdaddr_opt(value x);
inline std::optional<value> cddaar_opt(value x);
inline std::optional<value> cddadr_opt(value x);
inline std::optional<value> cdddar_opt(value x);
inline std::optional<value> cddddr_opt(value x);

inline std::optional<value> head_opt(value x);
inline std::optional<value> rest_opt(value x);
inline std::optional<value> first_opt(value x);
inline std::optional<value> second_opt(value x);
inline std::optional<value> third_opt(value x);
inline std::optional<value> fourth_opt(value x);
inline std::optional<value> fifth_opt(value x);
inline std::optional<value> sixth_opt(value x);
inline std::optional<value> seventh_opt(value x);
inline std::optional<value> eighth_opt(value x);
inline std::optional<value> ninth_opt(value x);
inline std::optional<value> tenth_opt(value x);

inline std::optional<value> nth_opt(value lst, size_t idx);
inline std::optional<value> nthcdr_opt(value lst, size_t idx);

//
// Library list manipulation
//
inline void add_last(value &first, value &last, value x);
inline value reverse(value lst);

inline value append(value a, value b);
inline value append(value a, value b, value c);
value map(map_function auto f, value lst);
value map(map2_function auto f, value lst1, value lst2);
bool all(predicate_function auto f, value lst);
bool any(predicate_function auto f, value lst);
template <typename U> U foldl(fold_function<U> auto f, U init, value lst);
template <typename U> U foldl(fold2_function<U> auto f, U init, value lst1, value lst2);
template <typename U> U foldr(fold_function<U> auto f, U init, value lst);
template <typename U> U foldr(fold2_function<U> auto f, U init, value lst1, value lst2);
value filter(predicate_function auto f, value lst);
value filter_not(predicate_function auto f, value lst);
value remove(value lst, predicate_function auto f);
inline value remove(value x, value lst);
inline value cartesian_product(value lst1, value lst2);
inline value cartesian_product(value lst1, value lst2, value lst3);

inline value assoc(value v, value lst);
std::optional<size_t> index_of(value lst, predicate_function auto f);
inline std::optional<size_t> index_of(value lst, value v);
bool member(value v, value lst, predicate_function auto f);
inline bool member(value v, value lst);
inline value range(size_t end);
inline value range(double start, double end, double step = 1.0);
template <typename F>
    requires std::is_invocable_r_v<value, F, size_t>
value build_list(size_t n, F f);

//
// Predicates
//
inline bool is_even(value x);
inline bool is_odd(value x);
inline bool is_zero(value x);
inline bool is_positive(value x);
inline bool is_negative(value x);

//
// Equality and operator overloading
//
inline bool is_equal(value left, value right) noexcept;

inline bool operator==(value left, value right);
inline bool operator==(value left, double right);
inline bool operator==(value left, std::string_view right);

inline bool operator!=(value left, value right);
inline bool operator!=(value left, double right);
inline bool operator!=(value left, std::string_view right);

inline value operator+(value left, value right);
inline value operator+(value left, double right);
inline value operator-(value left, value right);
inline value operator-(value left, double right);
inline value operator*(value left, value right);
inline value operator*(value left, double right);
inline value operator/(value left, value right);
inline value operator/(value left, double right);

//
// IO
//

inline value read(const char *s, size_t length, const char **end = nullptr);
inline value read(std::string_view s, const char **end = nullptr);
inline std::string print(value x);

namespace internal {

template <typename U, typename T, typename F>
    requires std::is_invocable_r_v<std::optional<U>, F, T>
std::optional<U> flat_map(std::optional<T> x, F f) {
    if (x.has_value()) {
        return f(*x);
    }
    return x;
}

inline thread_local context g_ctx{nullptr, 0, 0};

constexpr uint64_t HL_SIGN_BIT = ((uint64_t)1 << 63);
constexpr uint64_t HL_QNAN = (uint64_t)0x7ffc000000000000;

constexpr value nan_box_singleton(value_kind kind) noexcept { return value::make(HL_QNAN | (uint64_t)kind); }
inline uint8_t nan_unbox_singleton(value v) noexcept { return v.internal() & ~(HL_QNAN); }
inline value nan_box_ptr(void *ptr) noexcept { return value::make(((uintptr_t)ptr) | (HL_SIGN_BIT | HL_QNAN)); }

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

inline obj *nan_unbox_ptr(value x) noexcept { return (obj *)(uintptr_t)(x.internal() & ~(HL_SIGN_BIT | HL_QNAN)); }

inline struct obj_cons *unwrap_cons(value x) noexcept {
    assert(is_obj(x));
    obj *obj = nan_unbox_ptr(x);
    assert(obj->kind == value_kind::cons);
    return (obj_cons *)obj->as;
}

inline obj_str *unwrap_string(value x) noexcept {
    assert(is_obj(x));
    obj *obj = nan_unbox_ptr(x);
    assert(obj->kind == value_kind::string);
    return (obj_str *)obj->as;
}

inline uint32_t djb2(const char *src, const char *dst) noexcept {
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

inline bool hex_symbol_to_int(int x, int *value) noexcept {
    if ('0' <= x && x <= '9') {
        *value = x - '0';
        return true;
    }
    if ('A' <= x && x <= 'F') {
        *value = x - 'A' + 0xA;
        return true;
    }
    if ('a' <= x && x <= 'f') {
        *value = x - 'a' + 0xA;
        return true;
    }
    return false;
}

inline value new_string(context *ctx, const char *value, size_t length) {
    assert(value != NULL);
    assert(length != 0);
    assert(length < UINT32_MAX);

    void *memory = alloc(ctx, offsetof(obj, as) + offsetof(obj_str, str) + length + 1);
    obj *header = static_cast<obj *>(memory);
    header->kind = value_kind::string;

    obj_str *str = reinterpret_cast<obj_str *>(header->as);
    size_t actual_length = 0;
    char *write_cursor = str->str;
    const char *end = value + length;
    for (const char *cursor = value; cursor < end;) {
        if (*cursor == '\\') {
            ++cursor;
            if (cursor >= end)
                throw hl_exception("invalid escape sequence");
            switch (*cursor++) {
            case 'a': *write_cursor++ = '\a'; break;
            case 'b': *write_cursor++ = '\b'; break;
            case 'e': *write_cursor++ = '\x1b'; break;
            case 'f': *write_cursor++ = '\f'; break;
            case 'n': *write_cursor++ = '\n'; break;
            case 'r': *write_cursor++ = '\r'; break;
            case 't': *write_cursor++ = '\t'; break;
            case 'v': *write_cursor++ = '\v'; break;
            case '\\': *write_cursor++ = '\\'; break;
            case '\'': *write_cursor++ = '\''; break;
            case '\"': *write_cursor++ = '\"'; break;
            case '\?': *write_cursor++ = '\x3f'; break;
            case 'x': {
                if (cursor + 1 >= end)
                    throw hl_exception("too short hex escape sequence");
                int a = *cursor++;
                int b = *cursor++;
                if (!hex_symbol_to_int(a, &a) || !hex_symbol_to_int(b, &b))
                    throw hl_exception("invalid hex escape sequence");
                *write_cursor++ = (a << 4) | b;
                break;
            }
            default: throw hl_exception("invalid escape secuence");
            }
            ++actual_length;
            continue;
        }
        *write_cursor++ = *cursor++;
        ++actual_length;
    }
    assert((size_t)(write_cursor - str->str) <= length + 1);
    *write_cursor = '\0';
    str->length = actual_length;
    str->hash = djb2(str->str, str->str + actual_length);

    return nan_box_ptr(header);
}

inline value new_cons(context *ctx, value car, value cdr) {
    void *memory = alloc(ctx, offsetof(obj, as) + sizeof(obj_cons));
    obj *header = static_cast<obj *>(memory);
    header->kind = value_kind::cons;
    obj_cons *c = reinterpret_cast<obj_cons *>(header->as);
    c->car = car;
    c->cdr = cdr;
    return nan_box_ptr(header);
}

} // namespace internal

inline void set_context(context ctx) { internal::g_ctx = std::move(ctx); }

//
// Value constructors
//

constexpr value nil = internal::nan_box_singleton(value_kind::nil);
constexpr value tru = internal::nan_box_singleton(value_kind::tru);

// Use concepts with same_as<T> here instead of concrete type arguments to avoid implicit conversions.
// They are very hard to work around.
inline value make_value(value x) noexcept { return x; }
inline value make_value(std::same_as<bool> auto x) noexcept { return x ? tru : nil; }
inline value make_value(std::same_as<double> auto x) noexcept {
    uint64_t u64;
    memcpy(&u64, &x, sizeof(u64));
    return value::make(u64);
}
inline value make_value(std::same_as<int> auto x) noexcept { return make_value((double)x); }
inline value make_value(std::same_as<size_t> auto x) noexcept { return make_value((double)x); }
inline value make_value(std::same_as<nullptr_t> auto) noexcept { return nil; }
template <typename T>
    requires(std::constructible_from<std::string_view, T> && !std::is_same_v<T, nullptr_t>)
value make_value(T s) {
    std::string_view sv{s};
    return new_string(sv.begin(), sv.length());
}
template <has_to_list T> value make_value(T x) { return to_list<T>{}(x); }
template <typename T> value make_value(T) {
    static_assert(!sizeof(T),
                  "'make_value' not implemented for this type. Please use existing overloads to avoid ambiguity.");
}

template <typename... Args> value list(Args &&...args) {
    value h = nil;
    value t = nil;
    (add_last(h, t, make_value(std::forward<Args>(args))), ...);
    return h;
}

template <has_list_tag T> value make_tagged_value(T &&t) {
    std::string_view tag = list_tag<std::remove_cvref_t<T>>::tag;
    value v = make_value(std::forward<T>(t));
    return cons(make_value(tag), v);
}

template <has_list_tag... Args> value tagged_list(Args &&...args) {
    value h = nil;
    value t = nil;
    (add_last(h, t, make_tagged_value(std::forward<Args>(args))), ...);
    return h;
}

constexpr value::value() { this->u64_ = nil.u64_; }

inline value cons(value car, value cdr) { return internal::new_cons(&internal::g_ctx, car, cdr); }
inline value new_string(const char *value, size_t length) {
    return internal::new_string(&internal::g_ctx, value, length);
}

//
// Iterators
//

class value_iter {
public:
    using difference_type = ptrdiff_t;
    using value_type = value;
    using pointer = value *;
    using reference = value &;
    using iterator_category = std::forward_iterator_tag;

    value_iter() = delete;
    value_iter(const value_iter &) noexcept = default;
    value_iter(value_iter &&) noexcept = default;
    value_iter &operator=(const value_iter &) noexcept = default;
    value_iter &operator=(value_iter &&) noexcept = default;

    explicit value_iter(value x) noexcept : current_(x) {}

    reference operator*() { return car(current_); }
    bool operator==(value_iter other) noexcept { return current_.internal() == other.current_.internal(); }

    value_iter operator++(int) const {
        value_iter tmp = *this;
        ++tmp;
        return tmp;
    }
    value_iter &operator++() {
        current_ = cdr(current_);
        return *this;
    }

private:
    value current_;
};

template <has_from_list T> class deserializing_value_iter {
public:
    using difference_type = ptrdiff_t;
    using value_type = T;
    using pointer = T *;
    using reference = T &;
    using iterator_category = std::forward_iterator_tag;

    deserializing_value_iter() = delete;
    deserializing_value_iter(const deserializing_value_iter &) noexcept = default;
    deserializing_value_iter(deserializing_value_iter &&) noexcept = default;
    deserializing_value_iter &operator=(const deserializing_value_iter &) noexcept = default;
    deserializing_value_iter &operator=(deserializing_value_iter &&) noexcept = default;

    explicit deserializing_value_iter(value x) : current_(x) { update_current_value(); }

    reference operator*() noexcept { return current_value_; }
    bool operator==(deserializing_value_iter other) noexcept {
        return current_.internal() == other.current_.internal();
    }

    deserializing_value_iter operator++(int) const {
        deserializing_value_iter tmp = *this;
        ++tmp;
        return tmp;
    }
    deserializing_value_iter &operator++() {
        current_ = cdr(current_);
        update_current_value();
        return *this;
    }

private:
    void update_current_value() {
        if (!is_nil(current_))
            current_value_ = from_list<T>{}(car(current_));
    }

    value current_;
    T current_value_;
};

class tagged_value {
public:
    tagged_value() = default;
    tagged_value(const tagged_value &) noexcept = default;
    tagged_value(tagged_value &&) noexcept = default;
    tagged_value &operator=(const tagged_value &) noexcept = default;
    tagged_value &operator=(tagged_value &&) noexcept = default;

    explicit tagged_value(value data) : tag_(as_string_view(car(data))), data_(cdr(data)) {}
    tagged_value(std::string_view tag, value data) noexcept : tag_(tag), data_(data) {}

    template <has_from_list T> T get() const {
        assert(is<T>());
        return from_list<T>{}(data_);
    }

    template <has_list_tag T> bool is() const noexcept { return tag_ == list_tag<T>::tag; }
    bool is_num() const noexcept { return is<double>(); }
    bool is_string() const noexcept { return is<std::string_view>(); }

    std::string_view tag() const noexcept { return tag_; }

private:
    std::string_view tag_;
    value data_;
};

class deserializing_tagged_value_iter {
public:
    using difference_type = ptrdiff_t;
    using value_type = tagged_value;
    using pointer = value_type *;
    using reference = value_type &;
    using iterator_category = std::forward_iterator_tag;

    deserializing_tagged_value_iter() = delete;
    deserializing_tagged_value_iter(const deserializing_tagged_value_iter &) noexcept = default;
    deserializing_tagged_value_iter(deserializing_tagged_value_iter &&) noexcept = default;
    deserializing_tagged_value_iter &operator=(const deserializing_tagged_value_iter &) noexcept = default;
    deserializing_tagged_value_iter &operator=(deserializing_tagged_value_iter &&) noexcept = default;

    explicit deserializing_tagged_value_iter(value x) : current_(x) { update_current_value(); }

    reference operator*() noexcept { return current_element_; }
    bool operator==(const deserializing_tagged_value_iter &other) const noexcept {
        return current_.internal() == other.current_.internal();
    }

    deserializing_tagged_value_iter operator++(int) const {
        deserializing_tagged_value_iter tmp = *this;
        ++tmp;
        return tmp;
    }
    deserializing_tagged_value_iter &operator++() {
        current_ = cdr(current_);
        update_current_value();
        return *this;
    }

private:
    void update_current_value() {
        if (!is_nil(current_)) {
            value item = car(current_);
            auto [tag, data] = unapply_cons(item);
            current_element_ = value_type(as_string_view(tag), data);
        }
    }

    value current_;
    value_type current_element_;
};

template <has_from_list T> class deserializing_value_range {
public:
    deserializing_value_range() = delete;
    deserializing_value_range(const deserializing_value_range &) noexcept = default;
    deserializing_value_range(deserializing_value_range &&) noexcept = default;
    deserializing_value_range &operator=(const deserializing_value_range &) noexcept = default;
    deserializing_value_range &operator=(deserializing_value_range &&) noexcept = default;

    explicit deserializing_value_range(value x) noexcept : start_(x) {}

    deserializing_value_iter<T> begin() const noexcept { return deserializing_value_iter<T>{start_}; }
    deserializing_value_iter<T> end() const noexcept { return deserializing_value_iter<T>{nil}; }

private:
    value start_;
};

class deserializing_tagged_value_range {
public:
    deserializing_tagged_value_range() = delete;
    deserializing_tagged_value_range(const deserializing_tagged_value_range &) noexcept = default;
    deserializing_tagged_value_range(deserializing_tagged_value_range &&) noexcept = default;
    deserializing_tagged_value_range &operator=(const deserializing_tagged_value_range &) noexcept = default;
    deserializing_tagged_value_range &operator=(deserializing_tagged_value_range &&) noexcept = default;

    explicit deserializing_tagged_value_range(value x) noexcept : start_(x) {}

    deserializing_tagged_value_iter begin() const noexcept { return deserializing_tagged_value_iter{start_}; }
    deserializing_tagged_value_iter end() const noexcept { return deserializing_tagged_value_iter{nil}; }

private:
    value start_;
};

class value_range {
public:
    value_range() = delete;
    value_range(const value_range &) noexcept = default;
    value_range(value_range &&) noexcept = default;
    value_range &operator=(const value_range &) noexcept = default;
    value_range &operator=(value_range &&) noexcept = default;

    explicit value_range(value x) noexcept : start_(x) {}

    value_iter begin() const noexcept { return value_iter{start_}; }
    value_iter end() const noexcept { return value_iter{nil}; }

    template <has_from_list T> deserializing_value_range<T> as() const noexcept {
        return deserializing_value_range<T>(start_);
    }
    deserializing_tagged_value_range tagged() const noexcept { return deserializing_tagged_value_range(start_); }

private:
    value start_;
};

inline value_range value::iter() const noexcept { return value_range(*this); }

//
// Type checkers
//

inline std::string_view value_kind_str(value_kind kind) noexcept {
    switch (kind) {
    case value_kind::num: return "num";
    case value_kind::nil: return "nil";
    case value_kind::tru: return "true";
    case value_kind::cons: return "cons";
    case value_kind::string: return "string";
    }
    __builtin_unreachable();
}
inline std::string_view value_kind_str(value x) noexcept { return value_kind_str(get_value_kind(x)); }

inline value_kind get_value_kind(value x) noexcept {
    return is_obj(x) ? internal::nan_unbox_ptr(x)->kind : (value_kind)internal::nan_unbox_singleton(x);
}

inline bool is_num(value x) noexcept { return (x.internal() & internal::HL_QNAN) != internal::HL_QNAN; }
inline bool is_obj(value x) noexcept {
    return ((x.internal() & (internal::HL_QNAN | internal::HL_SIGN_BIT)) ==
            (internal::HL_QNAN | internal::HL_SIGN_BIT));
}

inline bool is_nil(value x) noexcept { return x.internal() == nil.internal(); }
inline bool is_true(value x) noexcept { return x.internal() == tru.internal(); }
inline bool is_cons(value x) noexcept { return is_obj(x) && internal::nan_unbox_ptr(x)->kind == value_kind::cons; }
inline bool is_string(value x) noexcept { return is_obj(x) && internal::nan_unbox_ptr(x)->kind == value_kind::string; }
inline bool is_list(value x) noexcept { return is_cons(x) || is_nil(x); }

//
// Unsafe accessors
//

inline std::string_view unwrap_string_view(value x) noexcept {
    internal::obj_str *str = internal::unwrap_string(x);
    return {str->str, str->length};
}

inline value &unwrap_car(value x) noexcept {
    internal::obj_cons *obj = internal::unwrap_cons(x);
    return obj->car;
}
inline value &unwrap_cdr(value x) noexcept {
    internal::obj_cons *obj = internal::unwrap_cons(x);
    return obj->cdr;
}
inline double unwrap_f64(value x) noexcept {
    assert(is_num(x));
    double result = 0;
    memcpy(&result, &x, sizeof(value));
    return result;
}
inline void unwrap_setcar(value c, value car) noexcept { internal::unwrap_cons(c)->car = car; }
inline void unwrap_setcdr(value c, value cdr) noexcept { internal::unwrap_cons(c)->cdr = cdr; }

//
// Checked data access
//

inline std::string_view as_string_view(value x) {
    if (!is_string(x))
        throw hl_exception("called 'as_string_view()' on non-string {}", value_kind_str(x));
    return unwrap_string_view(x);
}
inline double as_num_f64(value x) {
    if (!is_num(x))
        throw hl_exception("called 'num_dbl() on non-number value {}", value_kind_str(x));
    return unwrap_f64(x);
}
inline int as_num_int(value x) { return (int)as_num_f64(x); }
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
inline cons_unapply_result unapply_cons(value x) {
    if (!is_cons(x))
        throw hl_exception("called 'car()' on non-cons value {}", value_kind_str(x));
    return {unwrap_car(x), unwrap_cdr(x)};
}

//
// Library list accessors
//

inline size_t length(value lst) {
    size_t len = 0;
    for (auto it : lst.iter()) {
        (void)it;
        ++len;
    }
    return len;
}

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

inline value nth(value lst, size_t idx) { return car(nthcdr(lst, idx)); }
inline value nthcdr(value lst, size_t idx) {
#ifdef __GNUC__
    if (__builtin_constant_p(idx) && idx <= 10) {
        switch (idx) {
        case 0: return lst;
        case 1: return cdr(lst);
        case 2: return cddr(lst);
        case 3: return cdddr(lst);
        case 4: return cddddr(lst);
        case 5: return cdr(cddddr(lst));
        case 6: return cddr(cddddr(lst));
        case 7: return cdddr(cddddr(lst));
        case 8: return cddddr(cddddr(lst));
        case 9: return cdr(cddddr(cddddr(lst)));
        case 10: return cddr(cddddr(cddddr(lst)));
        }
    }
#endif
    while (idx--) {
        lst = cdr(lst);
    }
    return lst;
}

inline list_bind_2 first_2(value x) { return {first(x), second(x)}; }
inline list_bind_3 first_3(value x) { return {first(x), second(x), third(x)}; }
inline list_bind_4 first_4(value x) { return {first(x), second(x), third(x), fourth(x)}; }

//
// Optional list accessors
//
inline std::optional<value> car_opt(value x) {
    if (is_nil(x)) {
        return std::nullopt;
    }
    return car(x);
}
inline std::optional<value> cdr_opt(value x) {
    if (is_nil(x)) {
        return std::nullopt;
    }
    return cdr(x);
}
inline std::optional<value> caar_opt(value x) { return internal::flat_map<value>(car_opt(x), car_opt); }
inline std::optional<value> cadr_opt(value x) { return internal::flat_map<value>(cdr_opt(x), car_opt); }
inline std::optional<value> cdar_opt(value x) { return internal::flat_map<value>(car_opt(x), cdr_opt); }
inline std::optional<value> cddr_opt(value x) { return internal::flat_map<value>(cdr_opt(x), cdr_opt); }
inline std::optional<value> caaar_opt(value x) { return internal::flat_map<value>(caar_opt(x), car_opt); }
inline std::optional<value> caadr_opt(value x) { return internal::flat_map<value>(cadr_opt(x), car_opt); }
inline std::optional<value> cadar_opt(value x) { return internal::flat_map<value>(cdar_opt(x), car_opt); }
inline std::optional<value> caddr_opt(value x) { return internal::flat_map<value>(cddr_opt(x), car_opt); }
inline std::optional<value> cdaar_opt(value x) { return internal::flat_map<value>(caar_opt(x), cdr_opt); }
inline std::optional<value> cdadr_opt(value x) { return internal::flat_map<value>(cadr_opt(x), cdr_opt); }
inline std::optional<value> cddar_opt(value x) { return internal::flat_map<value>(cdar_opt(x), cdr_opt); }
inline std::optional<value> cdddr_opt(value x) { return internal::flat_map<value>(cddr_opt(x), cdr_opt); }
inline std::optional<value> caaaar_opt(value x) { return internal::flat_map<value>(caaar_opt(x), car_opt); }
inline std::optional<value> caaadr_opt(value x) { return internal::flat_map<value>(caadr_opt(x), car_opt); }
inline std::optional<value> caadar_opt(value x) { return internal::flat_map<value>(cadar_opt(x), car_opt); }
inline std::optional<value> caaddr_opt(value x) { return internal::flat_map<value>(caddr_opt(x), car_opt); }
inline std::optional<value> cadaar_opt(value x) { return internal::flat_map<value>(cdaar_opt(x), car_opt); }
inline std::optional<value> cadadr_opt(value x) { return internal::flat_map<value>(cdadr_opt(x), car_opt); }
inline std::optional<value> caddar_opt(value x) { return internal::flat_map<value>(cddar_opt(x), car_opt); }
inline std::optional<value> cadddr_opt(value x) { return internal::flat_map<value>(cdddr_opt(x), car_opt); }
inline std::optional<value> cdaaar_opt(value x) { return internal::flat_map<value>(caaar_opt(x), cdr_opt); }
inline std::optional<value> cdaadr_opt(value x) { return internal::flat_map<value>(caadr_opt(x), cdr_opt); }
inline std::optional<value> cdadar_opt(value x) { return internal::flat_map<value>(cadar_opt(x), cdr_opt); }
inline std::optional<value> cdaddr_opt(value x) { return internal::flat_map<value>(caddr_opt(x), cdr_opt); }
inline std::optional<value> cddaar_opt(value x) { return internal::flat_map<value>(cdaar_opt(x), cdr_opt); }
inline std::optional<value> cddadr_opt(value x) { return internal::flat_map<value>(cdadr_opt(x), cdr_opt); }
inline std::optional<value> cdddar_opt(value x) { return internal::flat_map<value>(cddar_opt(x), cdr_opt); }
inline std::optional<value> cddddr_opt(value x) { return internal::flat_map<value>(cdddr_opt(x), cdr_opt); }

inline std::optional<value> head_opt(value x) { return car_opt(x); }
inline std::optional<value> rest_opt(value x) { return cdr_opt(x); }
inline std::optional<value> first_opt(value x) { return car_opt(x); }
inline std::optional<value> second_opt(value x) { return cadr_opt(x); }
inline std::optional<value> third_opt(value x) { return caddr_opt(x); }
inline std::optional<value> fourth_opt(value x) { return cadddr_opt(x); }
inline std::optional<value> fifth_opt(value x) { return internal::flat_map<value>(cddddr_opt(x), car_opt); }
inline std::optional<value> sixth_opt(value x) { return internal::flat_map<value>(cddddr_opt(x), cadr_opt); }
inline std::optional<value> seventh_opt(value x) { return internal::flat_map<value>(cddddr_opt(x), caddr_opt); }
inline std::optional<value> eighth_opt(value x) { return internal::flat_map<value>(cddddr_opt(x), cadddr_opt); }
inline std::optional<value> ninth_opt(value x) {
    return internal::flat_map<value>(internal::flat_map<value>(cddddr_opt(x), cadddr_opt), car_opt);
}
inline std::optional<value> tenth_opt(value x) {
    return internal::flat_map<value>(internal::flat_map<value>(cddddr_opt(x), cadddr_opt), cadr_opt);
}

inline std::optional<value> nth_opt(value lst, size_t idx) {
    return internal::flat_map<value>(nthcdr_opt(lst, idx), car);
}
inline std::optional<value> nthcdr_opt(value lst, size_t idx) {
    if (is_nil(lst)) {
        return std::nullopt;
    }
    while (idx--) {
        lst = cdr(lst);
        if (is_nil(lst)) {
            return std::nullopt;
        }
    }
    return lst;
}

//
// Library list manipulation
//

inline void add_last(value &first, value &last, value x) {
    if (is_nil(last)) {
        first = last = cons(x, nil);
    } else {
        value new_last = cons(x, nil);
        unwrap_setcdr(last, new_last);
        last = new_last;
    }
}
inline value reverse(value lst) {
    value result = nil;
    for (auto it : lst.iter()) {
        result = cons(it, result);
    }
    return result;
}

inline value append(value a, value b) {
    value result = nil;
    value result_tail = nil;
    for (value x : a.iter()) {
        add_last(result, result_tail, x);
    }
    for (value x : b.iter()) {
        add_last(result, result_tail, x);
    }
    return result;
}
inline value append(value a, value b, value c) {
    value result = nil;
    value result_tail = nil;
    for (value x : a.iter()) {
        add_last(result, result_tail, x);
    }
    for (value x : b.iter()) {
        add_last(result, result_tail, x);
    }
    for (value x : c.iter()) {
        add_last(result, result_tail, x);
    }
    return result;
}
value map(map_function auto f, value lst) {
    auto new_lst = nil;
    auto new_lst_end = nil;
    for (auto it : lst.iter()) {
        add_last(new_lst, new_lst_end, make_value(f(it)));
    }
    return new_lst;
}
value map(map2_function auto f, value lst1, value lst2) {
    auto new_lst = nil;
    auto new_lst_end = nil;
    for (; is_cons(lst1) && is_cons(lst2); lst1 = cdr(lst1), lst2 = cdr(lst2)) {
        auto it1 = car(lst1);
        auto it2 = car(lst2);
        add_last(new_lst, new_lst_end, make_value(f(it1, it2)));
    }
    return new_lst;
}
bool all(predicate_function auto f, value lst) {
    for (auto it : lst.iter()) {
        if (!f(it)) {
            return false;
        }
    }
    return true;
}
bool any(predicate_function auto f, value lst) {
    for (auto it : lst.iter()) {
        if (f(it)) {
            return true;
        }
    }
    return false;
}

template <typename U> U foldl(fold_function<U> auto f, U init, value lst) {
    U result{std::move(init)};
    for (auto it : lst.iter()) {
        result = f(it, result);
    }
    return result;
}
template <typename U> U foldl(fold2_function<U> auto f, U init, value lst1, value lst2) {
    U result{std::move(init)};
    for (; is_cons(lst1) && is_cons(lst2); lst1 = cdr(lst1), lst2 = cdr(lst2)) {
        auto it1 = car(lst1);
        auto it2 = car(lst2);
        result = f(it1, it2, result);
    }
    return result;
}
template <typename U> U foldr(fold_function<U> auto f, U init, value lst) {
    if (is_nil(lst)) {
        return init;
    }
    auto [a, b] = unapply_cons(lst);
    return f(a, foldr(f, init, b));
}
template <typename U> U foldr(fold2_function<U> auto f, U init, value lst1, value lst2) {
    if (is_nil(lst1) || is_nil(lst2)) {
        return init;
    }
    return f(car(lst1), car(lst2), foldr(f, init, cdr(lst1), cdr(lst2)));
}

value filter(predicate_function auto f, value lst) {
    value new_lst = nil;
    value new_lst_end = nil;
    for (auto it : lst.iter()) {
        if (f(it)) {
            add_last(new_lst, new_lst_end, it);
        }
    }
    return new_lst;
}

value filter_not(predicate_function auto f, value lst) {
    return filter([&f](auto x) { return !f(x); }, lst);
}

value remove(value lst, predicate_function auto f) {
    value new_lst = nil;
    value new_lst_end = nil;
    for (auto it : lst.iter()) {
        if (!f(it)) {
            add_last(new_lst, new_lst_end, it);
        }
    }
    return new_lst;
}

inline value remove(value x, value lst) {
    return remove(lst, [x](auto it) { return it == x; });
}

inline value cartesian_product(value lst1, value lst2) {
    value h = nil, t = nil;
    for (auto it1 : lst1.iter()) {
        for (auto it2 : lst2.iter()) {
            add_last(h, t, list(it1, it2));
        }
    }
    return h;
}
inline value cartesian_product(value lst1, value lst2, value lst3) {
    value h = nil, t = nil;
    for (auto it1 : lst1.iter()) {
        for (auto it2 : lst2.iter()) {
            for (auto it3 : lst3.iter()) {
                add_last(h, t, list(it1, it2, it3));
            }
        }
    }
    return h;
}

inline value assoc(value v, value lst) {
    for (auto it : lst.iter()) {
        if (is_equal(v, car(it))) {
            return it;
        }
    }
    return nil;
}

std::optional<size_t> index_of(value lst, predicate_function auto f) {
    size_t idx = 0;
    for (auto it : lst.iter()) {
        if (f(it)) {
            return idx;
        }
        ++idx;
    }
    return std::nullopt;
}
inline std::optional<size_t> index_of(value lst, value x) {
    return index_of(lst, [x](auto it) { return it == x; });
}
template <typename F> bool member(value v, value lst, F f) { return index_of(lst, v, f).has_value(); }
inline bool member(value v, value lst) { return index_of(lst, v).has_value(); }

inline value range(size_t end) {
    value h = nil, t = nil;
    for (size_t i = 0; i < end; ++i) {
        add_last(h, t, make_value(i));
    }
    return h;
}
inline value range(double start, double end, double step) {
    value h = nil, t = nil;
    for (double it = start; it < end; it += step) {
        add_last(h, t, make_value(it));
    }
    return h;
}
template <typename F>
    requires std::is_invocable_r_v<value, F, size_t>
value build_list(size_t n, F f) {
    value h = nil, t = nil;
    for (size_t i = 0; i < n; ++i) {
        add_last(h, t, make_value(f(i)));
    }
    return h;
}

//
// Predicates
//

inline bool is_even(value x) {
    if (!is_num(x))
        throw hl_exception("'is_even' called on non-number {}", value_kind_str(x));
    double d = unwrap_f64(x);
    if ((double)(int)d != d)
        return false;
    return ((int)d % 2) == 0;
}
inline bool is_odd(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    double d = unwrap_f64(x);
    if ((double)(int)d != d)
        return false;
    return ((int)d % 2) != 0;
}
inline bool is_zero(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return unwrap_f64(x) == 0;
}
inline bool is_positive(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return unwrap_f64(x) > 0;
}
inline bool is_negative(value x) {
    if (!is_num(x))
        throw hl_exception("'is_odd' called on non-number {}", value_kind_str(x));
    return unwrap_f64(x) < 0;
}

//
// Equality and operator overloading
//

inline bool is_equal(value left, value right) noexcept {
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
        const internal::obj_str *left_str = internal::unwrap_string(left);
        const internal::obj_str *right_str = internal::unwrap_string(right);
        return left_str->length == right_str->length && left_str->hash == right_str->hash &&
               memcmp(left_str->str, right_str->str, left_str->length) == 0;
    }
    }
    __builtin_unreachable();
}

inline bool operator==(value left, value right) {
    if (is_num(left) || is_num(right)) {
        if (!is_num(left))
            throw hl_exception("'==' called on non-number {}", value_kind_str(left));
        if (!is_num(right))
            throw hl_exception("'==' called on non-number {}", value_kind_str(right));
        return unwrap_f64(left) == unwrap_f64(right);
    }
    if (is_string(left) || is_string(right)) {
        if (!is_string(left))
            throw hl_exception("'==' called on non-string {}", value_kind_str(left));
        if (!is_string(right))
            throw hl_exception("'==' called on non-string {}", value_kind_str(right));
        return unwrap_string_view(left) == unwrap_string_view(right);
    }
    throw hl_exception("unexpected types for '==' {} {}", value_kind_str(left), value_kind_str(right));
}
inline bool operator==(value left, double right) {
    if (!is_num(left))
        throw hl_exception("'==' called on non-number {}", value_kind_str(left));
    return unwrap_f64(left) == right;
}
inline bool operator==(value left, std::string_view right) {
    if (!is_string(left))
        throw hl_exception("'==' called on non-number {}", value_kind_str(left));
    return unwrap_string_view(left) == right;
}

inline bool operator!=(value left, value right) {
    if (is_num(left) || is_num(right)) {
        if (!is_num(left))
            throw hl_exception("'!=' called on non-number {}", value_kind_str(left));
        if (!is_num(right))
            throw hl_exception("'!=' called on non-number {}", value_kind_str(right));
        return unwrap_f64(left) != unwrap_f64(right);
    }
    if (is_string(left) || is_string(right)) {
        if (!is_string(left))
            throw hl_exception("'!=' called on non-string {}", value_kind_str(left));
        if (!is_string(right))
            throw hl_exception("'!=' called on non-string {}", value_kind_str(right));
        return unwrap_string_view(left) != unwrap_string_view(right);
    }
    throw hl_exception("unexpected types for '!=' {} {}", value_kind_str(left), value_kind_str(right));
}
inline bool operator!=(value left, double x) {
    if (!is_num(left))
        throw hl_exception("'!=' called on non-number {}", value_kind_str(left));
    return unwrap_f64(left) != x;
}
inline bool operator!=(value left, std::string_view right) {
    if (!is_num(left))
        throw hl_exception("'==' called on non-number {}", value_kind_str(left));
    return unwrap_string_view(left) != right;
}

inline value operator+(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'+' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'+' called on non-number {}", value_kind_str(right));
    return make_value(unwrap_f64(left) + unwrap_f64(right));
}
inline value operator-(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'-' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'-' called on non-number {}", value_kind_str(right));
    return make_value(unwrap_f64(left) - unwrap_f64(right));
}
inline value operator*(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'*' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'*' called on non-number {}", value_kind_str(right));
    return make_value(unwrap_f64(left) * unwrap_f64(right));
}
inline value operator*(value left, double right) {
    if (!is_num(left))
        throw hl_exception("'*' called on non-number {}", value_kind_str(left));
    return make_value(unwrap_f64(left) * right);
}
inline value operator/(value left, value right) {
    if (!is_num(left))
        throw hl_exception("'/' called on non-number {}", value_kind_str(left));
    if (!is_num(right))
        throw hl_exception("'/' called on non-number {}", value_kind_str(right));
    return make_value(unwrap_f64(left) / unwrap_f64(right));
}

//
// IO
//

namespace internal::reader {

enum class token_kind {
    end,
    num,
    symb,
    string,
    dot,
    tru,
    lparen,
    rparen,
    unexpected,
    comma
};

struct token {
    token_kind kind;
    size_t offset;
    size_t length;
    double f64;
};

struct lexer {
    const char *input;
    const char *end;
    const char *cursor;
    bool comma_symbol = false;
    token next{};

    bool is_symbol_breaker(int c) noexcept {
        if (comma_symbol && c == ',')
            return true;
        return isspace(c) || c == ';' || c == '(' || c == ')' || !isprint(c) || c == '"';
    }

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

                if (comma_symbol && c == ',') {
                    next.kind = token_kind::comma;
                    return;
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
                        if (*cursor == '\\' && cursor + 1 < end && cursor[1] == '\"') {
                            ++cursor;
                            ++cursor;
                            continue;
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
            size_t len = cursor - token_start;
            if (len == 1 && *token_start == '.') {
                next.kind = token_kind::dot;
                return;
            } else if (len == 1 && *token_start == 't') {
                next.kind = token_kind::tru;
                return;
            }

            char *strtod_end = nullptr;
            long long i64 = strtoll(token_start, &strtod_end, 10);
            if (strtod_end == cursor) {
                next.kind = token_kind::num;
                next.f64 = (double)i64;
                return;
            }
            strtod_end = nullptr;
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

template <typename T> struct base_reader {
    lexer &lex;
    const token &tok;
    bool should_return_old_token = false;

    explicit base_reader(lexer &lex) noexcept : lex(lex), tok(lex.next) {}

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

    void eat_token() noexcept { should_return_old_token = false; }

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
        list_head = list_tail = cons(((T *)this)->read_expr(), nil);
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
                unwrap_setcdr(list_tail, ((T *)this)->read_expr());
                peek_token();
                if (tok.kind != token_kind::rparen) {
                    throw hl_exception("Missing closing paren after dot when reading list");
                }
                eat_token();
                return list_head;
            }
            if (tok.kind == token_kind::comma) {
                eat_token();
                continue;
            }

            value ast = ((T *)this)->read_expr();
            value tail = cons(ast, nil);
            unwrap_setcdr(list_tail, tail);
            list_tail = tail;
        }
    }
};

struct function_reader : base_reader<function_reader> {

    using base_reader<function_reader>::base_reader;

    value read_expr() {
        peek_token();
        switch (tok.kind) {
        case token_kind::end: break;
        case token_kind::num: eat_token(); return make_value(tok.f64);
        case token_kind::tru: eat_token(); return tru;
        case token_kind::symb: return read_symbol();
        case token_kind::string:
            eat_token();
            assert(lex.input[tok.offset] == '"');
            assert(lex.input[tok.offset + tok.length - 1] == '"');
            assert(tok.length >= 2);
            return new_string(&g_ctx, lex.input + tok.offset + 1, tok.length - 2);
        case token_kind::lparen: return read_list();
        case token_kind::dot: throw hl_exception("unexpected .");
        case token_kind::rparen: throw hl_exception("stray )");
        case token_kind::comma: throw hl_exception("stary ,");
        case token_kind::unexpected: break;
        }
        __builtin_unreachable();
    }

    value read_symbol() {
        peek_token();
        if (tok.kind != token_kind::symb)
            throw hl_exception("function name token should be a symbol");

        eat_token();
        value sy = new_string(&g_ctx, lex.input + tok.offset, tok.length);
        peek_token();
        if (tok.kind == token_kind::lparen) {
            value lst = read_list();
            return cons(sy, lst);
        }
        return sy;
    }

    value read() { return read_symbol(); }
};

struct sexpr_reader : base_reader<sexpr_reader> {

    using base_reader<sexpr_reader>::base_reader;

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
        case token_kind::comma: throw hl_exception("unexpected ,"); break;
        case token_kind::unexpected: break;
        }
        __builtin_unreachable();
    }

    value read() { return read_expr(); }
};

} // namespace internal::reader

inline value read(const char *s, size_t length, const char **end) {
    internal::reader::lexer lexer{s, s + length, s};
    internal::reader::sexpr_reader reader{lexer};

    value result = reader.read();
    if (end) {
        *end = reader.lex.cursor;
    }
    return result;
}
inline value read(std::string_view s, const char **end) { return read(s.data(), s.length(), end); }

inline value read_function(const char *s, size_t length, const char **end) {
    internal::reader::lexer lexer{s, s + length, s, true};
    internal::reader::function_reader reader{lexer};

    value result = reader.read();
    if (end) {
        *end = reader.lex.cursor;
    }
    return result;
}
inline value read_function(std::string_view s, const char **end = nullptr) {
    return read_function(s.data(), s.length(), end);
}

inline std::string print(value x) {
    switch (get_value_kind(x)) {
    case value_kind::num: return std::format("{:g}", unwrap_f64(x));
    case value_kind::nil: return "()";
    case value_kind::tru: return "t";
    case value_kind::cons: {
        std::string result = "(";
        while (is_cons(x)) {
            result += print(unwrap_car(x));

            value next = unwrap_cdr(x);
            if (!is_list(next)) {
                result += " . ";
                result += print(next);
                break;
            }
            if (!is_nil(next)) {
                result += " ";
            }
            x = next;
        }
        return result + ")";
    }
    case value_kind::string: {
        std::string_view sv = unwrap_string_view(x);
        std::string result = "\"";
        result.reserve(sv.length() + 2);
        for (auto c : sv) {
            if (isprint(c) && c != '\"') {
                result += c;
                continue;
            }
            switch (c) {
            case '\a': result += "\\a"; break;
            case '\b': result += "\\b"; break;
            case '\x1b': result += "\\x1b"; break;
            case '\f': result += "\\f"; break;
            case '\n': result += "\\n"; break;
            case '\r': result += "\\r"; break;
            case '\t': result += "\\t"; break;
            case '\v': result += "\\v"; break;
            case '\\': result += "\\\\"; break;
            case '\'': result += "\\'"; break;
            case '\"': result += "\\\""; break;
            case '\?': result += "\\x3f"; break;
            default: result += std::format("\\x{:x}{:x}", (uint8_t)c >> 4, ((uint8_t)c & 0x0f));
            }
        }
        return result + "\"";
    }
    }
}

//
// Trait definitions
//

template <> struct from_list<double> {
    double operator()(value x) { return as_num_f64(x); }
};
template <> struct from_list<int> {
    int operator()(value x) { return as_num_int(x); }
};
template <> struct from_list<std::string_view> {
    std::string_view operator()(value x) { return as_string_view(x); }
};

} // namespace headerlisp

#endif