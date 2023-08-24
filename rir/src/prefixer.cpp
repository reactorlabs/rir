#include <iostream>

/**
 * Output stream transformer that prefixes each line with a string
 */
class Prefixer : private std::streambuf, public std::ostream {
  public:
    Prefixer(std::ostream& base, const char* prefix)
        : std::ostream(this), base(base), prefix(prefix) {
        base.flush();
    }

    inline virtual int sync(void) {
        base.flush();
        return 0;
    }

  private:
    std::ostream& base;
    const char* prefix;

    bool needsPrefix = true;

    int overflow(int c) override {
        if (needsPrefix) {
            base << prefix;
            needsPrefix = false;
        }

        if (c == '\n') {
            needsPrefix = true;
        }

        base.put(c);
        return 0;
    }
};
