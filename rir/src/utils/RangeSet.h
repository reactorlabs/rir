//! Source: https://github.com/hl037/rangeset.hpp/blob/master/rangeset.hpp
#pragma once

#include <iterator>
#include <set>
#include <utility>

/**
 * Range set ot type T.
 *
 * A range set is a set comprising zero or more nonempty, disconnected ranges of type T.
 *
 * This class supports adding and removing ranges from the set, and testing if a given object or range is contained in the set.
 *
 * @tparam T type of the contained range end points (anything with an absolute order defined)
 *
 * @tparam MERGE_TOUCHING if true (default) inserting [10, 20) then [20, 30) will merge both the range to [10;30). If set to false, both will live in the range set. To merge then, one would have to insert [19, 21)
 */
template <typename T, bool MERGE_TOUCHING=true>
class RangeSet{
  private:
    /** \internal
   *  Representation of an end_point (lower/upper bound) of a range.
   *  RangeSet should alternate lower and upper bounds.
     */
    struct end_point_t{
        T v;
        enum {
            BEFORE=0,
            LOWER=MERGE_TOUCHING ? 1 : 2,
            UPPER=3-LOWER,
            AFTER=2
        } dir;
        bool operator<(const end_point_t & oth) const{
            return v == oth.v ? dir < oth.dir : v < oth.v;
        }
        bool operator ==(const end_point_t & oth) const{
            return v == oth.v && dir == oth.dir;
        }
    };

    std::set<end_point_t> data;

  public:
    /**
   *  The iterator is bidirectionnal. Its dereferenced value is a std::pair<T, T>.
     */
    struct const_iterator{
        using difference_type = long;
        using value_type = std::pair<T, T>;
        using pointer = const value_type *;
        using reference = const value_type &;
        using iterator_category = std::bidirectional_iterator_tag;

        using _sub = typename std::set<end_point_t>::const_iterator;
    
        value_type val;
        _sub lower;
        _sub end;
      protected:
        inline void update(){
            if(lower != end){
                val = {lower->v, std::next(lower)->v};
            }
        }
      public:
        inline const_iterator() : lower{} {}
        inline const_iterator(const _sub & lower, const _sub & end) : lower{lower}, end{end}{ update(); }

        inline reference operator*() const { return val; }
        inline pointer operator->() const { return &val; }
        inline const_iterator & operator++() { ++++lower; update(); return *this; }
        inline const_iterator operator++(int) { const_iterator res{*this}; ++*this; return res; }
        inline const_iterator & operator--() { ----lower; update(); return *this; }
        inline const_iterator operator--(int) { const_iterator res{*this}; --*this; return res; }

        inline bool operator==(const const_iterator & oth) const { return lower == oth.lower; }
        inline bool operator!=(const const_iterator & oth) const { return !(*this == oth); }
    };

    /**
   *  Add the range [start, end) (or "[start; end[" in other notation) to the set.
   *  If overlap occurs, the ranges are merged. if STRICT_OVERLAP is False, [start, mid) and [mid, end) will be merged to [start, end). Else, they will coeexist.
     */
    void insert(T start, T end){
        if(end <= start){
            return;
        }
        auto && upper = data.upper_bound({end, end_point_t::UPPER}); // end) < upper OR upper == end()
        // At the container begining
        if(upper == data.begin()){  //    [start , end) < [ upper=begin(), end() )
            data.insert(data.begin(), {end, end_point_t::UPPER});
            data.insert(data.begin(), {start, end_point_t::LOWER});
            return;
        }

        if(upper == data.end() or upper->dir == end_point_t::LOWER){  // ')' < end < '['
            if(std::prev(upper)->v != end){ // if not same value, insert, else just skip and take upper's precedent
                data.insert(upper, {end, end_point_t::UPPER});
            }
            --upper;
        }
    
        auto && lower = data.upper_bound({start, end_point_t::LOWER});//    [start < lower

        if((lower == upper || lower->dir == end_point_t::LOWER) && lower->v != start
            && (lower == data.begin() || std::prev(lower)->dir == end_point_t::UPPER)){
            data.insert(lower, {start, end_point_t::LOWER});
        }
    
        if(lower != upper){
            data.erase(lower, upper);
        }
    }
  
    inline void insert(const std::pair<T,T> & range){
        insert(range.first, range.second);
    }

    void insert_all(const RangeSet<T> & oth){
        for(const auto& r : oth){
            insert(r.first, r.second);
        }
    }
  
    /**
   * Remove the interval [start, end) (or "[start; end[" in other notation) from the set.
     */
    void remove(const T & start, const T & end){
        auto && lower = data.lower_bound({start, end_point_t::LOWER});
        // At the container end
        if(lower == data.end()){
            return; //nothing to do...
        }

        bool lower_inserted = false;
        if(lower->dir == end_point_t::UPPER){
            if(lower->v == start){
                ++lower;
            }
            else{
                data.insert(lower, {start, end_point_t::UPPER});
                --lower;
                lower_inserted = true;
            }
        }
    
        auto && upper = data.lower_bound({end, end_point_t::LOWER});

        if(upper != data.end() && upper->dir == end_point_t::UPPER){
            if(upper->v == end){
                ++upper;
            }
            else{
                data.insert(upper, {end, end_point_t::LOWER});
                --upper;
            }
        }
    
        if(lower_inserted){
            ++lower;
        }
        if(lower != upper){
            data.erase(lower, upper);
        }
    }
  
    inline void remove(const std::pair<T,T> & range){
        remove(range.first, range.second);
    }

    /**
   * Remove unit ranges from the set (could be faster than remove)
     */
    inline void erase(const_iterator it_begin, const_iterator it_end){
        if(it_begin == end()){
            return;
        }
        data.erase(it_begin.lower, it_end.lower);
    }

    inline void erase(const_iterator it){
        if(it == end()){
            return;
        }
        auto it2 = it.lower;
        ++++it2;
        data.erase(it.lower, it2);
    }

    /**
   * Find the unit range that contains a specific value.
   * Returns end() if not v is not in the set.
     */
    const_iterator find(const T & v) const {
        auto && upper = data.upper_bound({v, end_point_t::AFTER}); // v < lower
        if(upper == data.begin() || upper == data.end() || upper->dir == end_point_t::LOWER){
            return end();
        }
        else {
            return const_iterator(--upper, data.end());
        }
    }
  
    /**
   * Find the unit range that contains the sub range [start, end) (or [start; end[ )
     */
    const_iterator find(const T & start, const T & end) const {
        auto && upper = data.upper_bound({start, end_point_t::AFTER}); // v < lower
        if(upper == data.begin() || upper == data.end() || upper->dir == end_point_t::LOWER || upper->v < end){
            return end();
        }
        else {
            return const_iterator(--upper, data.end());
        }
    }
    inline const_iterator find(const std::pair<T,T> & range) const {
        return find(range.first, range.second);
    }

    /**
   * Return the number of unit range in the set (The number of iterator beetwin begin() and end())
     */
    inline size_t size() const { return data.size() / 2; }

    /**
   * Return an iterator to the first unit range. When dereferencing an iterator, the value is a std::pair<T,T> describing the interval [ res.first, res.end )
     */
    inline const_iterator begin() const { return const_iterator{data.begin(), data.end()}; }
    /**
   * Return a past-the-end iterator of this set.
     */
    inline const_iterator end() const { return const_iterator{data.end(), data.end()}; }

  public:
    RangeSet()=default;
    ~RangeSet()=default;
  
};