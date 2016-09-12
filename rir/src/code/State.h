#pragma once

namespace rir {

/** Abstract state prototype.

  In its minimal implementation, the abstract state must be capable of cloning itself (deep copy) and provide mergeWith() method that updates it with information from other state and returns true if own information changes.
 */
class State {
public:
    virtual ~State() {
    }

    /** Creates a deep copy of the state and returns it.
     */
    virtual State * clone() const = 0;

    /** Merges the information from the other state, returns true if changed.
     */
    virtual bool mergeWith(State const * other) = 0;

};





}
