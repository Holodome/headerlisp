#ifndef HEADERLISP_H
#define HEADERLISP_H

#include <assert.h>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <new>
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
    explicit context_guard(size_t size) : memory_((uint8_t *)malloc(size)), memory_size_(size) {
        if (memory_ == nullptr)
            throw std::bad_alloc{};
        set_context(context{(char *)memory_, 0, memory_size_});
    }
    ~context_guard() { set_context(context{}); }

private:
    uint8_t *memory_ = nullptr;
    size_t memory_size_ = 0;
};

//
// Base types
//

class value;
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

class value {
    struct constructor_tag {};

public:
    constexpr value();
    constexpr value(const value &) = default;
    constexpr value(value &&) = default;
    constexpr value &operator=(const value &) = default;
    constexpr value &operator=(value &&) = default;

    constexpr static value make(uint64_t x) { return value(constructor_tag{}, x); }

    constexpr uint64_t internal() const { return u64_; }

    value_range iter();

private:
    constexpr value(constructor_tag, uint64_t x) : u64_(x) {}

    uint64_t u64_;
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

//
// Value constructors
//

// There are more overloads for these functions below
inline value make_value(value x);
template <has_list_tag T> inline value make_tagged_value(T &&t);

inline value new_string(const char *str, size_t length);
inline value new_stringz(const char *str);
inline value cons(value car, value cdr);

template <typename... Args> inline value list(Args &&...args);
template <has_list_tag... Args> inline value tagged_list(Args &&...args);

//
// Type checkers
//
constexpr std::string_view value_kind_str(value_kind kind);
constexpr std::string_view value_kind_str(value x);
constexpr value_kind get_value_kind(value x);
constexpr bool is_num(value x);
constexpr bool is_obj(value x);
constexpr bool is_nil(value x);
constexpr bool is_true(value x);
constexpr bool is_cons(value x);
constexpr bool is_list(value x);

//
// Unsafe functions that unwrap inner storage with assumption that type is correct
//
inline std::string_view unwrap_string_view(value x);
inline value &unwrap_car(value x);
inline value &unwrap_cdr(value x);
inline double unwrap_f64(value x);
// Unsafe cons mutators
inline void unwrap_setcar(value cons, value car);
inline void unwrap_setcdr(value cons, value cdr);

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
// Library list manipulation
//
inline void add_last(value &first, value &last, value x);
inline value reverse(value lst);

inline value append(value a, value b);
inline value append(value a, value b, value c);
template <typename F> inline value map(F f, value lst);
template <typename F> inline value map(F f, value lst1, value lst2);
template <typename F> inline bool all(F f, value lst);
template <typename F> inline bool any(F f, value lst);
template <typename U, typename F> inline U foldl(F f, U init, value lst);
template <typename U, typename F> inline U foldl(F f, U init, value lst1, value lst2);
template <typename U, typename F> inline U foldr(F f, U init, value lst);
template <typename U, typename F> inline U foldr(F f, U init, value lst1, value lst2);
template <typename F> inline value filter(F f, value lst);
template <typename F> inline value filter_not(F f, value lst);
template <typename U, typename F> inline value remove(U x, value lst, F f);
inline value remove(value x, value lst);
inline value cartesian_product(value lst1, value lst2);
inline value cartesian_product(value lst1, value lst2, value lst3);

inline value assoc(value v, value lst);
inline value nth(value lst, size_t idx);
template <typename F> inline std::optional<size_t> index_of(value lst, value v, F f);
inline std::optional<size_t> index_of(value lst, value v);
template <typename F> inline bool member(value v, value lst, F f);
inline bool member(value v, value lst);
inline value range(size_t end);
inline value range(double start, double end, double step = 1.0);
template <typename F> inline value build_list(size_t n, F f);

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
inline bool is_equal(value left, value right);

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
inline value read(std::string_view s);
inline value read(std::string_view s, const char **end);
inline std::string print(value x);

namespace internal {

inline thread_local context g_ctx{nullptr, 0, 0};

constexpr uint64_t HL_SIGN_BIT = ((uint64_t)1 << 63);
constexpr uint64_t HL_QNAN = (uint64_t)0x7ffc000000000000;

constexpr value nan_box_singleton(value_kind kind) { return value::make(HL_QNAN | (uint64_t)kind); }
constexpr uint8_t nan_unbox_singleton(value v) { return v.internal() & ~(HL_QNAN); }
inline value nan_box_ptr(void *ptr) { return value::make(((uintptr_t)ptr) | (HL_SIGN_BIT | HL_QNAN)); }

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

inline obj *nan_unbox_ptr(value x) { return (obj *)(uintptr_t)(x.internal() & ~(HL_SIGN_BIT | HL_QNAN)); }

inline struct obj_cons *unwrap_cons(value x) {
    assert(is_obj(x));
    obj *obj = nan_unbox_ptr(x);
    assert(obj->kind == value_kind::cons);
    return (obj_cons *)obj->as;
}

inline obj_str *unwrap_string(value x) {
    assert(is_obj(x));
    obj *obj = nan_unbox_ptr(x);
    assert(obj->kind == value_kind::string);
    return (obj_str *)obj->as;
}

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

} // namespace internal

inline void set_context(context ctx) { internal::g_ctx = ctx; }

//
// Value constructors
//

constexpr value nil = internal::nan_box_singleton(value_kind::nil);
constexpr value tru = internal::nan_box_singleton(value_kind::tru);

// Use concepts with same_as<T> here instead of concrete type arguments to avoid implicit conversions.
// They are very hard to work around.
inline value make_value(value x) { return x; }
inline value make_value(std::same_as<bool> auto x) { return x ? tru : nil; }
inline value make_value(std::same_as<double> auto x) {
    uint64_t u64;
    memcpy(&u64, &x, sizeof(u64));
    return value::make(u64);
}
inline value make_value(std::same_as<int> auto x) { return make_value((double)x); }
inline value make_value(std::same_as<size_t> auto x) { return make_value((double)x); }
inline value make_value(std::same_as<nullptr_t> auto) { return nil; }
template <typename T>
    requires(std::constructible_from<std::string_view, T> && !std::is_same_v<T, nullptr_t>)
inline value make_value(T s) {
    std::string_view sv{s};
    return new_string(sv.begin(), sv.length());
}
template <has_to_list T> inline value make_value(T x) { return to_list<T>{}(x); }
template <typename T> inline value make_value(T) {
    static_assert(!sizeof(T),
                  "'make_value' not implemented for this type. Please use existing overloads to avoid ambiguity.");
}

template <typename... Args> inline value list(Args &&...args) {
    value first = nil;
    value last = nil;
    (add_last(first, last, make_value(std::forward<Args>(args))), ...);
    return first;
}

template <has_list_tag T> inline value make_tagged_value(T &&t) {
    std::string_view tag = list_tag<std::remove_cvref_t<T>>::tag;
    value v = make_value(std::forward<T>(t));
    return cons(make_value(tag), v);
}

template <has_list_tag... Args> inline value tagged_list(Args &&...args) {
    value first = nil;
    value last = nil;
    (add_last(first, last, make_tagged_value(std::forward<Args>(args))), ...);
    return first;
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
    value_iter(const value_iter &) = default;
    value_iter(value_iter &&) = default;
    value_iter &operator=(const value_iter &) = default;
    value_iter &operator=(value_iter &&) = default;

    explicit value_iter(value x) : current_(x) {}

    reference operator*() { return car(current_); }
    bool operator==(value_iter other) { return current_.internal() == other.current_.internal(); }

    value_iter operator++(int) {
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

    constexpr deserializing_value_iter() = delete;
    constexpr deserializing_value_iter(const deserializing_value_iter &) = default;
    constexpr deserializing_value_iter(deserializing_value_iter &&) = default;
    constexpr deserializing_value_iter &operator=(const deserializing_value_iter &) = default;
    constexpr deserializing_value_iter &operator=(deserializing_value_iter &&) = default;

    explicit constexpr deserializing_value_iter(value x) : current_(x) { update_current_value(); }

    constexpr reference operator*() { return current_value_; }
    constexpr bool operator==(deserializing_value_iter other) {
        return current_.internal() == other.current_.internal();
    }

    constexpr deserializing_value_iter operator++(int) {
        deserializing_value_iter tmp = *this;
        ++tmp;
        return tmp;
    }
    constexpr deserializing_value_iter &operator++() {
        current_ = cdr(current_);
        update_current_value();
        return *this;
    }

private:
    constexpr void update_current_value() {
        if (!is_nil(current_))
            current_value_ = from_list<T>{}(car(current_));
    }

    value current_;
    T current_value_;
};

template <typename... Args> class tagged_value {
public:
    constexpr tagged_value() = default;
    constexpr tagged_value(const tagged_value &) = default;
    constexpr tagged_value(tagged_value &&) = default;
    constexpr tagged_value &operator=(const tagged_value &) = default;
    constexpr tagged_value &operator=(tagged_value &&) = default;

    constexpr tagged_value(value data) : tag_(as_string_view(car(data))), data_(cdr(data)) {}
    constexpr tagged_value(std::string_view tag, value data) : tag_(tag), data_(data) {}

    template <has_from_list T> T get() const {
        assert(is<T>());
        return from_list<T>{}(data_);
    }

    template <typename T> constexpr bool is() const { return tag_ == list_tag<T>::tag; }
    constexpr bool is_num() const { return is<double>(); }
    constexpr bool is_string() const { return is<std::string_view>(); }

    constexpr std::string_view tag() const { return tag_; }

private:
    std::string_view tag_;
    value data_;
};

template <typename... Args> class deserializing_sum_value_iter {
public:
    using difference_type = ptrdiff_t;
    using value_type = tagged_value<Args...>;
    using pointer = value_type *;
    using reference = value_type &;
    using iterator_category = std::forward_iterator_tag;

    deserializing_sum_value_iter() = delete;
    deserializing_sum_value_iter(const deserializing_sum_value_iter &) = default;
    deserializing_sum_value_iter(deserializing_sum_value_iter &&) = default;
    deserializing_sum_value_iter &operator=(const deserializing_sum_value_iter &) = default;
    deserializing_sum_value_iter &operator=(deserializing_sum_value_iter &&) = default;

    explicit deserializing_sum_value_iter(value x) : current_(x) { update_current_value(); }

    reference operator*() { return current_element_; }
    bool operator==(deserializing_sum_value_iter other) { return current_.internal() == other.current_.internal(); }

    deserializing_sum_value_iter operator++(int) {
        deserializing_value_iter tmp = *this;
        ++tmp;
        return tmp;
    }
    deserializing_sum_value_iter &operator++() {
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
    deserializing_value_range(const deserializing_value_range &) = default;
    deserializing_value_range(deserializing_value_range &&) = default;
    deserializing_value_range &operator=(const deserializing_value_range &) = default;
    deserializing_value_range &operator=(deserializing_value_range &&) = default;

    explicit deserializing_value_range(value x) : start_(x) {}

    deserializing_value_iter<T> begin() { return deserializing_value_iter<T>{start_}; }
    deserializing_value_iter<T> end() { return deserializing_value_iter<T>{nil}; }

private:
    value start_;
};

template <has_list_tag... Args> class deserializing_sum_value_range {
public:
    deserializing_sum_value_range() = delete;
    deserializing_sum_value_range(const deserializing_sum_value_range &) = default;
    deserializing_sum_value_range(deserializing_sum_value_range &&) = default;
    deserializing_sum_value_range &operator=(const deserializing_sum_value_range &) = default;
    deserializing_sum_value_range &operator=(deserializing_sum_value_range &&) = default;

    explicit deserializing_sum_value_range(value x) : start_(x) {}

    deserializing_sum_value_iter<Args...> begin() { return deserializing_sum_value_iter<Args...>{start_}; }
    deserializing_sum_value_iter<Args...> end() { return deserializing_sum_value_iter<Args...>{nil}; }

private:
    value start_;
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

    template <has_from_list T> deserializing_value_range<T> as() { return deserializing_value_range<T>(start_); }
    template <has_list_tag... Args> deserializing_sum_value_range<Args...> any_of() {
        return deserializing_sum_value_range<Args...>(start_);
    }

private:
    value start_;
};

inline value_range value::iter() { return value_range(*this); }

//
// Type checkers
//

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
constexpr std::string_view value_kind_str(value x) { return value_kind_str(get_value_kind(x)); }

constexpr value_kind get_value_kind(value x) {
    return is_obj(x) ? internal::nan_unbox_ptr(x)->kind : (value_kind)internal::nan_unbox_singleton(x);
}

constexpr bool is_num(value x) { return (x.internal() & internal::HL_QNAN) != internal::HL_QNAN; }
constexpr bool is_obj(value x) {
    return ((x.internal() & (internal::HL_QNAN | internal::HL_SIGN_BIT)) ==
            (internal::HL_QNAN | internal::HL_SIGN_BIT));
}

constexpr bool is_nil(value x) { return x.internal() == nil.internal(); }
constexpr bool is_true(value x) { return x.internal() == tru.internal(); }
constexpr bool is_cons(value x) { return is_obj(x) && internal::nan_unbox_ptr(x)->kind == value_kind::cons; }
constexpr bool is_string(value x) { return is_obj(x) && internal::nan_unbox_ptr(x)->kind == value_kind::string; }
constexpr bool is_list(value x) { return is_cons(x) || is_nil(x); }

//
// Unsafe accessors
//

inline std::string_view unwrap_string_view(value x) {
    internal::obj_str *str = internal::unwrap_string(x);
    return {str->str, str->length};
}

inline value &unwrap_car(value x) {
    internal::obj_cons *obj = internal::unwrap_cons(x);
    return obj->car;
}
inline value &unwrap_cdr(value x) {
    internal::obj_cons *obj = internal::unwrap_cons(x);
    return obj->cdr;
}
inline double unwrap_f64(value x) {
    assert(is_num(x));
    double result;
    memcpy(&result, &x, sizeof(value));
    return result;
}
inline void unwrap_setcar(value cons, value car) { internal::unwrap_cons(cons)->car = car; }
inline void unwrap_setcdr(value cons, value cdr) { internal::unwrap_cons(cons)->cdr = cdr; }

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

inline list_bind_2 first_2(value x) { return {first(x), second(x)}; }
inline list_bind_3 first_3(value x) { return {first(x), second(x), third(x)}; }
inline list_bind_4 first_4(value x) { return {first(x), second(x), third(x), fourth(x)}; }

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
template <typename F> inline value map(F f, value lst) {
    auto new_lst = nil;
    auto new_lst_end = nil;
    for (auto it : lst.iter()) {
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
    for (auto it : lst.iter()) {
        if (!f(it)) {
            return false;
        }
    }
    return true;
}
template <typename F> inline bool any(F f, value lst) {
    for (auto it : lst.iter()) {
        if (f(it)) {
            return true;
        }
    }
    return false;
}

template <typename U, typename F> inline U foldl(F f, U init, value lst) {
    U result = init;
    for (auto it : lst.iter()) {
        result = f(it, result);
    }
    return result;
}
template <typename U, typename F> inline U foldl(F f, U init, value lst1, value lst2) {
    U result = init;
    for (; is_cons(lst1) && is_cons(lst2); lst1 = cdr(lst1), lst2 = cdr(lst2)) {
        auto it1 = car(lst1);
        auto it2 = car(lst2);
        result = f(it1, it2, result);
    }
    return result;
}
template <typename U, typename F> inline value foldr(F f, U init, value lst) {
    if (is_nil(lst)) {
        return init;
    }
    return f(car(lst), foldr(f, init, cdr(lst)));
}
template <typename U, typename F> inline value foldr(F f, U init, value lst1, value lst2) {
    if (is_nil(lst1) || is_nil(lst2)) {
        return init;
    }
    return f(car(lst1), car(lst2), foldr(f, init, cdr(lst1), cdr(lst2)));
}

template <typename F> inline value filter(F f, value lst) {
    value new_lst = nil;
    value new_lst_end = nil;
    for (auto it : lst.iter()) {
        if (f(it)) {

            add_last(new_lst, new_lst_end, it);
        }
    }
    return new_lst;
}

template <typename F> inline value filter_not(F f, value lst) {
    return filter([&f](auto x) { return !f(x); }, lst);
}

template <typename U, typename F> inline value remove(U x, value lst, F f) {
    value new_lst = nil;
    value new_lst_end = nil;
    for (auto it : lst.iter()) {
        if (!f(x, it)) {
            add_last(new_lst, new_lst_end, it);
        }
    }
    return new_lst;
}
inline value remove(value x, value lst) { return remove(x, lst, is_equal); }

inline value cartesian_product(value lst1, value lst2) {
    value head = nil, tail = nil;
    for (auto it1 : lst1.iter()) {
        for (auto it2 : lst2.iter()) {
            add_last(head, tail, list(it1, it2));
        }
    }
    return head;
}
inline value cartesian_product(value lst1, value lst2, value lst3) {
    value head = nil, tail = nil;
    for (auto it1 : lst1.iter()) {
        for (auto it2 : lst2.iter()) {
            for (auto it3 : lst3.iter()) {
                add_last(head, tail, list(it1, it2, it3));
            }
        }
    }
    return head;
}

inline value assoc(value v, value lst) {
    for (auto it : lst.iter()) {
        if (is_equal(v, car(it))) {
            return it;
        }
    }
    return nil;
}

inline value nth(value lst, size_t idx) {
    while (idx--) {
        lst = cdr(lst);
    }
    return car(lst);
}
template <typename F> inline std::optional<size_t> index_of(value lst, value v, F f) {
    size_t idx = 0;
    for (auto it : lst.iter()) {
        if (f(v, it)) {
            return idx;
        }
        ++idx;
    }
    return std::nullopt;
}
inline std::optional<size_t> index_of(value lst, value v) { return index_of(lst, v, is_equal); }
template <typename F> inline bool member(value v, value lst, F f) { return index_of(lst, v, f).has_value(); }
inline bool member(value v, value lst) { return index_of(lst, v).has_value(); }

inline value range(size_t end) {
    value head = nil, tail = nil;
    for (size_t i = 0; i < end; ++i) {
        add_last(head, tail, make_value(i));
    }
    return head;
}
inline value range(double start, double end, double step) {
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
        internal::obj_str *left_str = internal::unwrap_string(left);
        internal::obj_str *right_str = internal::unwrap_string(right);
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
        list_head = list_tail = cons(read_expr(), nil);
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
                eat_token();
                return list_head;
            }

            value ast = read_expr();
            value tail = cons(ast, nil);
            unwrap_setcdr(list_tail, tail);
            list_tail = tail;
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

} // namespace internal::reader

inline value read(const char *s, size_t length, const char **end) {
    internal::reader::lexer lexer{s, s + length, s, {}};
    internal::reader::reader reader{lexer};

    value result = reader.read_expr();
    if (end) {
        *end = reader.lex.cursor;
    }
    return result;
}
inline value read(std::string_view s) { return read(s.data(), s.length()); }
inline value read(std::string_view s, const char **end) { return read(s.data(), s.length(), end); }

inline std::string print(value x) {
    switch (get_value_kind(x)) {
    case value_kind::num: return std::format("{:g}", unwrap_f64(x));
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

//
// Trait definitions
//

template <> struct list_tag<double> {
    constexpr static inline std::string_view tag = "num";
};
template <> struct list_tag<int> {
    constexpr static inline std::string_view tag = "num";
};

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