#include "randomized_queue.h"
#include "subset.h"

void subset(unsigned long k, std::istream & in, std::ostream & out)
{
    std::string line;
    randomized_queue<std::string> queue;
    while (std::getline(in, line)) {
        queue.enqueue(line);
    }
    for (auto it = queue.begin(); k > 0 && it != queue.end(); --k, ++it) {
        out << *it << std::endl;
    }
}
