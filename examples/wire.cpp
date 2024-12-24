//
// This is more complex example where hololisp is used as a wire format for communcation.
// It uses hololisp to serialize and deserialize structures that are passed between communicating threads.
//
#include "headerlisp.h"

#include <cstdio>
#include <thread>
#include <unistd.h>

namespace hl = headerlisp;

// We define two possible kinds of commands.

// This command should print to terminal my_name specified number of times
struct SayMyName {
    int how_many_times;
    std::string my_name;
};

// This command should calculate arithmetic expression inside expr
struct Calculate {
    std::string expr;
};

// Tags are used when serializing using headerlisp tagged mode. They are used to differentiate between different
// structures.
template <> struct hl::list_tag<SayMyName> {
    constexpr static inline std::string_view tag = "say-my-name";
};
template <> struct hl::list_tag<Calculate> {
    constexpr static inline std::string_view tag = "calculate";
};

template <> struct hl::to_list<SayMyName> {
    auto operator()(const SayMyName &d) { return list(d.how_many_times, d.my_name); }
};
template <> struct hl::to_list<Calculate> {
    auto operator()(const Calculate &d) { return list(d.expr); }
};

template <> struct hl::from_list<SayMyName> {
    SayMyName operator()(auto lst) {
        auto [a, b] = first_2(lst);
        return SayMyName{as_num_int(a), std::string{as_string_view(b)}};
    }
};
template <> struct hl::from_list<Calculate> {
    Calculate operator()(auto lst) { return Calculate{std::string{as_string_view(car(lst))}}; }
};

// Recursive expression evaluation function
int calculate(hl::value x) {
    if (hl::is_num(x))
        return hl::as_num_int(x);
    std::string_view head = hl::as_string_view(hl::first(x));
    if (head == "abs") {
        int tmp = calculate(hl::second(x));
        return tmp < 0 ? -tmp : tmp;
    }
    if (head == "+")
        return calculate(hl::second(x)) + calculate(hl::third(x));
    if (head == "-")
        return calculate(hl::second(x)) - calculate(hl::third(x));
    if (head == "*")
        return calculate(hl::second(x)) * calculate(hl::third(x));
    if (head == "/")
        return calculate(hl::second(x)) / calculate(hl::third(x));
    fprintf(stderr, "invalid expression\n");
    return 0;
}

int calculate(std::string_view expr) {
    hl::value ast = hl::read(expr);
    return calculate(ast);
}

void processing_thread(int pipe_fd) {
    char buffer[4096];

    hl::context_guard ctx{};
    for (;;) {
        ssize_t nread = read(pipe_fd, buffer, sizeof(buffer));
        if (nread < 0) {
            perror("read");
            exit(1);
        }
        if (nread == 0) {
            break;
        }
        buffer[nread] = 0;

        printf("received %s\n", buffer);
        const char *cursor = buffer;
        while (cursor < buffer + nread) {
            const char *end;
            // 'pipe' does not split messages at arbitrary boundaries, but it can read multiple messages at once.
            // hl::read has second parameter that stores location at which it has finished parsing the first value,
            // so we can process the remaining part later.
            hl::value ast = hl::read(cursor, &end);
            cursor = end;
            // Use hl::tagged_value or list(ast).iter().any_of<SayMyName, Calculate> to create std::variant-like
            // accessor
            auto msg = hl::tagged_value<SayMyName, Calculate>(ast);
            if (msg.is<SayMyName>()) {
                SayMyName d = msg.get<SayMyName>();
                for (int i = 0; i < d.how_many_times; ++i) {
                    printf("%d: your name is %s\n", i + 1, d.my_name.c_str());
                }
            } else if (msg.is<Calculate>()) {
                Calculate d = msg.get<Calculate>();
                printf("calculate result %d\n", calculate(d.expr));
            } else {
                fprintf(stderr, "invalid message\n");
            }
        }
    }
    fflush(stdout);
}

int main() {
    int pipefd[2];
    if (pipe(pipefd) < 0) {
        perror("pipe");
        return 1;
    }

    // Spawn other thread that does processing
    std::thread thr{processing_thread, pipefd[0]};

    // In main thread send 3 messages
    {
        hl::context_guard ctx{};
        auto msg_lst = hl::make_tagged_value(SayMyName{1, "Walter"});
        std::string serialized = hl::print(msg_lst); // ("say-my-name" 1 "Walter")
        printf("sending %s\n", serialized.c_str());
        write(pipefd[1], serialized.data(), serialized.size());
        msg_lst = hl::make_tagged_value(SayMyName{2, "White"});
        serialized = hl::print(msg_lst); // ("say-my-name" 2 "White")
        printf("sending %s\n", serialized.c_str());
        write(pipefd[1], serialized.data(), serialized.size());
        msg_lst = hl::make_tagged_value(Calculate{"(abs (* 2 (+ 1 3)))"});
        serialized = hl::print(msg_lst); // ("calculate" "(abs (* 2 (+ 1 3)))")
        printf("sending %s\n", serialized.c_str());
        write(pipefd[1], serialized.data(), serialized.size());
        close(pipefd[1]);
    }

    thr.join();
    return 0;
}