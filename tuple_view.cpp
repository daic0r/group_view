#include <cstddef>
#include <vector>
#include <ranges>
#include <tuple>
#include <iostream>
#include <utility>
#include <bit>

namespace rg = std::ranges;

namespace detail
{
    template<typename T, typename Dest>
    struct type_appender {};

    template<typename T, typename... Ts, template<typename...> typename Prev>
    struct type_appender<T, Prev<Ts...>> {
        using type = Prev<T, Ts...>;
    };

    template<typename T, std::size_t N, template<typename...> typename Prev>
    struct repeat_type {
        using type = typename type_appender<T, typename repeat_type<T, N-1, Prev>::type>::type;
    };

    template<typename T, template<typename...> typename Prev>
    struct repeat_type<T, 0, Prev> {
        using type =  Prev<>;
    };

} // namespace detail


template<std::size_t N, rg::input_range V>
class group_view {
    using container_t = V;
    using T = typename V::value_type;
    
    const container_t& m_cont;

public:
    struct sentinel_t {};
    static inline constexpr sentinel_t sentinel{};

    class iterator {
        using difference_type = std::ptrdiff_t;
        using value_type = typename detail::repeat_type<std::add_lvalue_reference_t<std::add_const_t<T>>, N, std::tuple>::type;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category	= std::input_iterator_tag;

        const container_t* m_cont{};
        typename container_t::const_iterator m_iter{};

        template<std::size_t>
        static decltype(auto) getIterValue(typename container_t::const_iterator& where) noexcept(noexcept(std::declval<typename container_t::const_iterator>()++)) {
            return *where++;
        }

        template<std::size_t... Is>
        auto fill_tuple(std::index_sequence<Is...>) const noexcept {
            auto iter = m_iter;
            return value_type{ getIterValue<Is>(iter)... };
        }

    public:
        iterator() = default;
        iterator(const container_t* cont, typename container_t::const_iterator iter) : m_cont{ cont }, m_iter{ iter } {}
        iterator(const container_t* cont, sentinel_t) : m_cont{ cont }, m_iter{ std::cend(*m_cont) } {}

        value_type operator*() { 
            return fill_tuple(std::make_index_sequence<N>{});
        }

        iterator& operator++() {
            std::advance(m_iter, N);
            return *this;
        }

        iterator operator++(int) const {
            auto ret = *this;
            ++(*this);
            return ret;
        }

        friend bool operator==(iterator const& lhs, iterator const& rhs) noexcept {
            return lhs.m_iter == rhs.m_iter;
        }

        friend bool operator!=(iterator const& lhs, iterator const& rhs) noexcept {
            return not(lhs.m_iter == rhs.m_iter);
        }

        bool operator==(sentinel_t) const noexcept {
            return m_iter == std::cend(m_cont);
        }

        bool operator!=(sentinel_t) const noexcept {
            return not(m_iter == sentinel);
        }
    };

    group_view(const container_t& cont) : m_cont{ cont } {}
    auto begin() const { return iterator{ &m_cont, std::cbegin(m_cont) }; }
    auto end() const { return iterator{ &m_cont, sentinel }; }
};

int main() {
    std::vector<int> v{ 10, 20, 30, 100, 200, 300, 120, 140, 920 };
    group_view<3, std::vector<int>> gv{ v };

    for (auto tup : gv) {
        std::cout << std::get<0>(tup) << ", " << std::get<1>(tup) << ", " << std::get<2>(tup) << "\n";
    }
}