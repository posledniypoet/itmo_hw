#include <algorithm>
#include <cstring>
#include <fstream>
#include <iterator>
#include <iostream>
#include <string>
#include <vector>
namespace {
    std::locale def_loc("en_US.UTF-8");
    int string_toInt(const char * c){
        return(std::atoi(c));
    }

    bool numeric_sort_comparator(std::string str1,std::string str2){
        int num1=string_toInt(str1.c_str());
        int num2=string_toInt(str2.c_str());
        if (num1 == num2) {
            return def_loc(str1, str2);
        }
        if (num1 > num2) {
            return false;
        }
        else{
            return true;
        }
    }
    bool ignore_case_comparator(std::string str1,std::string str2){
        transform(str1.begin(), str1.end(), str1.begin(), ::tolower);
        transform(str2.begin(), str2.end(), str2.begin(), ::tolower);
        return def_loc(str1, str2);
    }

    class Vector
    {
        using Impl = std::vector<std::string>;
        Impl m_data;
    public:
        Vector(const std::size_t size_hint)
        {
            m_data.reserve(size_hint / 100);
        }

        template <typename Comparator>
        void sort(Comparator comparator)
        { std::sort(m_data.begin(), m_data.end(), comparator); }
        using const_iterator = Impl::const_iterator;
        using value_type = Impl::value_type;

        const_iterator begin() const
        { return m_data.begin(); }
        const_iterator end() const
        { return m_data.end(); }

        void add(const std::string & line)
        {
            m_data.emplace_back(line);
        }
    };
    using Lines = Vector;

    template <class C>
    void print_out(std::ostream & strm, const C & c)
    {
        std::ostream_iterator<typename C::value_type> out(strm, "\n");
            std::copy(c.begin(), c.end(), out);
    }

    void sort_stream(std::istream & input, const bool ignore_case, const bool numeric_sort, const std::size_t size_hint = 1024)
    {
        Lines lines(size_hint);

        // read lines
        std::string line;
        while (std::getline(input, line)) {
            lines.add(line);
        }
        if (numeric_sort) {
            lines.sort(numeric_sort_comparator);
        } else if (ignore_case) {
            lines.sort(ignore_case_comparator);
        } else {
            lines.sort(def_loc);
        }

        print_out(std::cout, lines);

    }

    std::size_t calculate_size(std::istream & input)
    {
        input.seekg(0, std::ios_base::end);
        const auto end_pos = input.tellg();
        input.seekg(0);
        return end_pos;
    }

} // anonymous namespace

int main(int argc, char ** argv)
{
    bool ignore_case = false;
    bool numeric_sort = false;
    const char * input_name = nullptr;
    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-') {
            if (argv[i][1] != '-') {
                const size_t len = std::strlen(argv[i]);
                for (size_t j = 1; j < len; ++j) {
                    switch (argv[i][j]) {
                        case 'f':
                            ignore_case = true;
                            break;
                        case 'n':
                            numeric_sort = true;
                            break;
                    }
                }
            }
            else {
                if (std::strcmp(argv[i], "--ignore-case") == 0) {
                    ignore_case = true;
                }
                else if (std::strcmp(argv[i], "--numeric-sort") == 0) {
                    numeric_sort = true;
                }
            }
        }
        else {
            input_name = argv[i];
        }
    }

    if (input_name != nullptr) {
        std::ifstream f(input_name);
        sort_stream(f, ignore_case, numeric_sort, calculate_size(f));
    }
    else {
        sort_stream(std::cin, ignore_case, numeric_sort);
    }
}