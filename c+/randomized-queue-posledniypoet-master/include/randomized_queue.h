#pragma once

#include <vector>
#include <memory>
#include <random>
#include <algorithm>
template <class T>
class randomized_queue {

private:
    inline static std::random_device rd = std::random_device();
    inline static std::mt19937 gen = std::mt19937(rd());
    mutable std::uniform_int_distribution<> rand = std::uniform_int_distribution<>(0, 0);
    std::vector<T> array;

    static std::vector<int> get_permutation(size_t n) {
        std::vector<int> ret(n);
        for (size_t i = 0; i < n; ++i) {
            ret[i] = i;
        }
        std::shuffle(ret.begin(),ret.end(),gen);
        ret.emplace_back(-1);
        return ret;
    }

public:
    class iterator
    {
        class const_iterator;
    public:
        typedef T value_type;
        typedef T &reference;
        typedef T *pointer;
        typedef int difference_type;
        typedef std::forward_iterator_tag iterator_category;

        explicit iterator(randomized_queue& queue_,int size)
                : queue(&queue_),
                  permutation(get_permutation(queue_.size())),
                  pos(size)
        { }
        iterator& operator=(const iterator& other) = default;

        iterator& operator++() { // ++i
            ++pos;
            return *this;
        }

        iterator operator++(int) { // i++
            iterator ret = *this;
            ++(*this);
            return ret;
        }

        reference operator*() const { return queue->array[permutation[pos]]; }
        pointer operator->() const { return queue->array + permutation[pos]; }
        bool operator==(const iterator &other) const { return queue == other.queue && permutation[pos] == other.permutation[other.pos]; }
        bool operator!=(const iterator &other) const { return !(*this == other); }

    private:
        randomized_queue* const queue;
        std::vector<int> permutation;
        int pos;
    };

    class const_iterator
    {
    public:
        typedef const T value_type;
        typedef const T &reference;
        typedef const T *pointer;
        typedef int difference_type;
        typedef std::forward_iterator_tag iterator_category;

        explicit const_iterator(const randomized_queue& queue_, int size)
                : queue(&queue_),
                  permutation(get_permutation(queue_.size())),
                  pos(size)

        { }


        const_iterator(const iterator& it)
                : queue(it.queue),
                  permutation(it.permutation),
                  pos(it.pos)
        { }

        const_iterator& operator=(const const_iterator& other) = default;

        const_iterator operator++() { // ++i
            ++pos;
            return *this;
        }

        const_iterator& operator++(int) { // i++
            ++pos;
            return --(*this);
        };

        reference operator*() const { return queue->array[permutation[pos]]; }
        pointer operator->() const { return queue->array + permutation[pos]; }
        bool operator==(const const_iterator &other) const { return queue == other.queue && permutation[pos] == other.permutation[other.pos]; }
        bool operator!=(const const_iterator &other) const { return !(*this == other); }

    private:
        const randomized_queue* const queue;
        std::vector<int> permutation;
        int pos;
    };

    iterator begin() {
        return iterator(*this,0);
    }

    const_iterator begin() const {
        return const_iterator(*this,0);
    }

    const_iterator cbegin() const {
        return const_iterator(*this,0);
    }

    iterator end() {
        return iterator (*this,size());
    }

    const_iterator end() const {
        return const_iterator(*this,size());
    }

    const_iterator cend() const {
        return const_iterator(*this,size());
    }

    void enqueue(const T& item) {
        array.emplace_back(item);
        rand = std::uniform_int_distribution<>(0, array.size() - 1);
    }

    void enqueue(T&& item) {
        array.emplace_back(std::move(item));
        rand = std::uniform_int_distribution<>(0, size() - 1);
    }

    const T& sample() const {
        int pos = rand(gen);
        return array[pos];
    }

    T dequeue() {
        int pos = rand(gen);
        T ret = std::exchange(array[pos], std::move(array[array.size() - 1]));
        array.pop_back();
        rand = std::uniform_int_distribution<>(0, size() - 1);
        return ret;
    }

    size_t size() const {
        return array.size();
    }

    bool empty() const {
        return array.empty();
    }
};
