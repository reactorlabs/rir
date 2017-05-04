#pragma once

#include <algorithm>
#include <cstdlib>
#include <deque>
#include <map>

#include "R/r.h"
#include "ir/CodeEditor.h"

namespace rir {

/** Abstract state prototype.

  In its minimal implementation, the abstract state must be capable of cloning
  itself (deep copy) and provide mergeWith() method that updates it with
  information from other state and returns true if own information changes.
 */
class State {
  public:
    virtual ~State() {}

    /** Creates a deep copy of the state and returns it.
     */
    virtual State* clone() const = 0;

    /** Merges the information from the other state, returns true if changed.
     */
    virtual bool mergeWith(State const* other) = 0;
};

/** An ugly SFINAE template that statically checks that values used in states
  have mergeWith method.

  This is not necessary but produces nice non-templatish messages if you forget
  to create the method.
   */
template <typename T>
class has_mergeWith {
    typedef char one;
    typedef long two;

    template <typename C>
    static one test(char (*)[sizeof(&C::mergeWith)]);
    template <typename C>
    static two test(...);

  public:
    enum { value = sizeof(test<T>(0)) == sizeof(char) };
};

/** Stack model.

  Model of an abstract stack is relatively easy - since for correct code, stack
  depth at any mergepoints must be constant, so stack merging is only a merge of
  the stack's values.
 */
template <typename AVALUE>
class AbstractStack : public State {
    static_assert(std::is_copy_constructible<AVALUE>::value,
                  "Abstract values must be copy constructible");
    static_assert(has_mergeWith<AVALUE>::value,
                  "Abstract values must have mergeWith method");
    /*    static_assert(std::is_const<decltype(AVALUE::top)>::value,
                      "Must have const top");
        static_assert(std::is_const<decltype(AVALUE::top)>::value,
                      "Must have const bottom"); */

  public:
    typedef AVALUE Value;

    AbstractStack() = default;
    AbstractStack(AbstractStack const&) = default;

    /** Clone is just a copy constructor.
     */
    AbstractStack* clone() const override { return new AbstractStack(*this); }

    /** Merge with delegates to properly typed function.
     */
    bool mergeWith(State const* other) override {
        assert(dynamic_cast<AbstractStack const*>(other) != nullptr);
        return mergeWith(*dynamic_cast<AbstractStack const*>(other));
    }

    /** Merges the other stack into itself.

      Returns true if any of stack values changed during the process.
     */
    bool mergeWith(AbstractStack const& other) {
        assert(depth() == other.depth() and
               "At merge stacks must have the same height");
        bool result = false;
        for (size_t i = 0, e = stack_.size(); i != e; ++i)
            result = stack_[i].mergeWith(other.stack_[i]) or result;
        return result;
    }

    AVALUE& top() { return stack_.front(); }

    AVALUE const& top() const { return stack_.front(); }

    AVALUE pop() {
        AVALUE result = stack_.front();
        stack_.pop_front();
        return result;
    }

    void pop(size_t num) {
        assert(stack_.size() >= num);
        for (size_t i = 0; i != num; ++i)
            stack_.pop_front();
    }

    void push(AVALUE value) { stack_.push_front(value); }

    size_t depth() const { return stack_.size(); }

    bool empty() const { return stack_.size() == 0; }

    AVALUE const& operator[](size_t idx) const {
        assert(idx < stack_.size());
        return stack_[idx];
    }

    AVALUE& operator[](size_t idx) {
        assert(idx < stack_.size());
        return stack_[idx];
    }

    /** Prettyprints the stack.
     */
    void print() const {
        Rprintf("Stack depth: %i\n", stack_.size());
        for (size_t i = 0; i < stack_.size(); ++i) {
            Rprintf("  %i : ", i);
            stack_[i].print();
            Rprintf("\n");
        }
    }

    typename std::deque<AVALUE>::const_iterator const begin() const {
        return stack_.begin();
    }

    typename std::deque<AVALUE>::const_iterator const end() const {
        return stack_.end();
    }

  protected:
    std::deque<AVALUE> stack_;
};

template <typename KEY, typename AVALUE>
class AbstractEnvironment : public State {
    static_assert(std::is_copy_constructible<AVALUE>::value,
                  "Abstract values must be copy constructible");
    static_assert(has_mergeWith<AVALUE>::value,
                  "Abstract values must have mergeWith method");

    // TODO change this to static methods
    /*static_assert(std::is_const<decltype(AVALUE::top)>::value,
                  "Must have const top");
    static_assert(std::is_const<decltype(AVALUE::top)>::value,
                  "Must have const bottom");
    */

  public:
    typedef AVALUE Value;

    AbstractEnvironment() : parent_(nullptr) {}

    AbstractEnvironment(AbstractEnvironment const& from)
        : parent_(from.parent_ == nullptr ? nullptr : from.parent_->clone()),
          env_(from.env_) {}

    AbstractEnvironment* clone() const override {
        return new AbstractEnvironment(*this);
    }

    bool mergeWith(State const* other) override {
        assert(dynamic_cast<AbstractEnvironment const*>(other) != nullptr);
        return mergeWith(*dynamic_cast<AbstractEnvironment const*>(other));
    }

    /** Merges the other environment into this one.
     *
     * Note: For Environments, Missing Values cannot be assumed to be Bottom!
     *       Eg. if we merge two control flows where one has a variable
     *       defined and the other not, we have to deal with this. We have to
     *       assume that in one branch there exists a local binding and in the
     *       other one not. The specific analysis needs to know what should
     *       happen. Therefore we merge those values with AVALUE::Absent(). If
     *       the analysis really does not care, it can define bottom == absent.
     *
     * TODO Note that merge also merges parent environments
     *      - this might require more thinking.
     */
    bool mergeWith(AbstractEnvironment const& other) {
        bool result = false;

        for (auto i = other.env_.begin(), e = other.env_.end(); i != e; ++i) {
            auto own = env_.find(i->first);
            if (own == env_.end()) {
                // if there is a variable in other that does not exist here, we
                // merge it with the Absent value.
                AVALUE missing = i->second;
                missing.mergeWith(AVALUE::Absent());
                env_.emplace(i->first, missing);
                result = true;
            } else {
                // otherwise try merging it with our variable
                result = own->second.mergeWith(i->second) or result;
            }
        }
        for (auto i = env_.begin(), e = env_.end(); i != e; ++i) {
            auto them = other.env_.find(i->first);
            if (them == other.env_.end()) {
                // The other env has is missing this value, we must treat this
                // as absent
                result = i->second.mergeWith(AVALUE::Absent()) or result;
            }
        }

        // merge parents

        if (parent_ == nullptr) {
            if (other.parent_ != nullptr) {
                parent_ = new AbstractEnvironment(*other.parent_);
                result = true;
            }
        } else if (other.parent_ != nullptr) {
            result = parent_->mergeWith(*(other.parent_)) or result;
        }
        return result;
    }

    // TODO is this what we want wrt parents?
    bool empty() const { return env_.empty(); }

    bool has(KEY name) const { return env_.find(name) != env_.end(); }

    /** Simulates looking for a variable.

      If the variable is found in current environment, it is returned. If not,
      then parent environments are searched and only if the variable is not
      found anywhere in them, top value is returned.
     */
    AVALUE const& find(KEY name) const {
        auto i = env_.find(name);
        if (i == env_.end())
            if (parent_ != nullptr)
                return parent_->find(name);
            else
                return AVALUE::top();
        else
            return i->second;
    }

    AVALUE const& operator[](KEY name) const {
        auto i = env_.find(name);
        if (i == env_.end())
            return AVALUE::top();
        else
            return i->second;
    }

    AVALUE& operator[](KEY name) {
        auto i = env_.find(name);
        if (i == env_.end()) {
            // so that we do not demand default constructor on values
            env_.insert(std::pair<KEY, AVALUE>(name, AVALUE::top()));
            i = env_.find(name);
            return i->second;
        } else {
            return i->second;
        }
    }

    bool hasParent() const { return parent_ != nullptr; }

    AbstractEnvironment& parent() {
        assert(parent_ != nullptr);
        return *parent_;
    }

    void print() const {
        Rprintf("Environment: ");
        for (auto i : env_) {
            Rprintf("    %s : ", CHAR(PRINTNAME(i.first)));
            i.second.print();
            Rprintf("\n");
        }
        if (parent_ != nullptr) {
            Rprintf("Parent :\n");
            parent_->print();
        } else {
            Rprintf("No parent");
        }
    }

    void mergeAll(AVALUE v) {
        for (auto& e : env_)
            e.second.mergeWith(v);
    }

    typename std::map<KEY, AVALUE>::iterator begin() { return env_.begin(); }

    typename std::map<KEY, AVALUE>::iterator end() { return env_.end(); }

  protected:
    AbstractEnvironment* parent_ = nullptr;

    std::map<KEY, AVALUE> env_;
};

class DummyState {
  public:
    bool mergeWith(DummyState const* other) { return false; }
    void print() {}
};

/** This could be done with multiple virtual inheritance, but the composition is
 * simpler, and perhaps even cleaner, albeit more lengthy.
 */
template <typename KEY, typename AVALUE, typename GLOBAL = DummyState>
class AbstractState : public State {
  public:
    typedef AVALUE Value;

    AbstractState() = default;
    AbstractState(AbstractState const&) = default;

    AbstractState* clone() const override { return new AbstractState(*this); }

    bool mergeWith(State const* other) override {
        assert(dynamic_cast<AbstractState const*>(other) != nullptr);
        return mergeWith(*dynamic_cast<AbstractState const*>(other));
    }

    GLOBAL const& global() const { return global_; }
    GLOBAL& global() { return global_; }

    AbstractStack<AVALUE> const& stack() const { return stack_; }

    AbstractStack<AVALUE>& stack() { return stack_; }

    AbstractEnvironment<KEY, AVALUE> const& env() const { return env_; }

    AbstractEnvironment<KEY, AVALUE>& env() { return env_; }

    bool mergeWith(AbstractState const& other) {
        bool result = false;
        result = global_.mergeWith(&other.global_) or result;
        result = stack_.mergeWith(other.stack_) or result;
        result = env_.mergeWith(other.env_) or result;
        return result;
    }

    AVALUE pop() { return stack_.pop(); }

    AVALUE& top() { return stack_.top(); }

    AVALUE const& top() const { return stack_.top(); }

    void pop(size_t num) { stack_.pop(num); }

    void push(AVALUE value) { stack_.push(value); }

    AVALUE const& operator[](size_t index) const { return stack_[index]; }

    AVALUE& operator[](size_t index) { return stack_[index]; }

    AVALUE const& operator[](KEY name) const { return env_[name]; }

    AVALUE& operator[](KEY name) { return env_[name]; }

    void print() const {
        global_.print();
        stack_.print();
        env_.print();
    }

    void mergeAllEnv(AVALUE v) { env_.mergeAll(v); }

  protected:
    AbstractStack<AVALUE> stack_;
    AbstractEnvironment<KEY, AVALUE> env_;
    GLOBAL global_;
};
}
