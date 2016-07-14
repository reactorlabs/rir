#ifndef H_SEXP_MATCH
#define H_SEXP_MATCH

#include <sstream>
#include <iostream>
#include <cassert>

#include "r.h"

namespace rir {

class MatchStatement {
    const char* file;
    int line;

  public:
    const SEXP subject;

    MatchStatement(const char* file, int line, SEXP subject)
        : file(file), line(line), subject(subject) {}
    ~MatchStatement() {}

    void fallThroughFail();
};
}

#define Match(__M__e)                                                          \
    {                                                                          \
        MatchStatement __M__match_statement__(__FILE__, __LINE__, (__M__e));   \
        SEXP $ __attribute__((unused)) = (__M__e);                             \
        switch (TYPEOF((__M__e))) {

#define Else(stmnts)                                                           \
    break;                                                                     \
    }                                                                          \
    default: {                                                                 \
        stmnts;                                                                \
        break;                                                                 \
    }                                                                          \
        }

#define EndCases()                                                             \
    break;                                                                     \
    }                                                                          \
    default: { __M__match_statement__.fallThroughFail(); }                     \
        }

#define __M__endCase                                                           \
    break;                                                                     \
    }

#define __ARG6(_0, _1, _2, _3, _4, _5, ...) _5
#define __VA_NUM_ARGS(...) __ARG6(__VA_ARGS__, 5, 4, 3, 2, 1, _EOL)

#define __DISPATCH(fun, ...) __DISPATCH_(fun, __VA_NUM_ARGS(__VA_ARGS__))
#define __DISPATCH_(fun, nargs) __DISPATCH__(fun, nargs)
#define __DISPATCH__(fun, nargs) fun##nargs

#define Case(...) __M__endCase __DISPATCH(Case, __VA_ARGS__)(__VA_ARGS__)

#define Case1(__M__type) case (__M__type): {

#define Case2(__M__type, __M__car_name)                                        \
    case (__M__type): {                                                        \
        SEXP(__M__car_name) = CAR(__M__match_statement__.subject);

#define Case3(__M__type, __M__car_name, __M__cdr_name)                         \
    case (__M__type): {                                                        \
        SEXP(__M__car_name)                                                    \
        __attribute__((unused)) = CAR(__M__match_statement__.subject);         \
        SEXP(__M__cdr_name) = CDR(__M__match_statement__.subject);

#define Case4(__M__type, __M__car_name, __M__cdr_name, __M__tag_name)          \
    case (__M__type): {                                                        \
        SEXP(__M__car_name)                                                    \
        __attribute__((unused)) = CAR(__M__match_statement__.subject);         \
        SEXP(__M__cdr_name)                                                    \
        __attribute__((unused)) = CDR(__M__match_statement__.subject);         \
        SEXP(__M__tag_name) = TAG(__M__match_statement__.subject);


#endif
