#include <cstddef>
#include <vector>
#include <ranges>
#include <tuple>
#include <iostream>
#include <utility>

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


template<std::size_t, typename Container>
class group_view;

template<std::size_t N, template<typename,typename> typename Container, typename T, typename Allocator>
class group_view<N, Container<T, Allocator>> : public rg::view_interface<group_view<N, Container<T, Allocator>>> {
    using container_t = Container<T, Allocator>;
    
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
        iterator(const container_t* cont, sentinel_t) : m_cont{ cont }, m_iter{ m_cont->cend() } {}

        value_type operator*() { 
            return fill_tuple(std::make_index_sequence<N>{});
        }

        iterator& operator++() {
            std::advance(m_iter, N);
            return *this;
        }

        iterator operator++(int) const {
            const auto ret = *this;
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
            return m_iter == m_cont->end();
        }

        bool operator!=(sentinel_t) const noexcept {
            return not(m_iter == sentinel);
        }
    };

    group_view(const container_t& cont) : m_cont{ cont } {}
    auto begin() const { return iterator{ &m_cont, m_cont.cbegin() }; }
    auto end() const { return iterator{ &m_cont, sentinel }; }
};


//emplate<std::size_t N, template<typename,typename> typename Container, typename T, typename Allocator>
//group_view(const Container<T, Allocator>&) -> group_view<N, Container<T, Allocator>>;


int main() {
    //static_assert(std::is_same_v<detail::repeat_type<int, 5, std::tuple>::type, std::tuple<int,int,int,int,int>>);td::
    std::vector<int> v{ 10, 20, 30 };
    group_view<3, std::vector<int>> gv{ v };

    int j = 10;

    const auto t = v | group_view<3, std::vector<int>>;

    for (auto tup : gv) {
        std::cout << std::get<0>(tup) << ", " << std::get<1>(tup) << ", " << std::get<2>(tup) << "\n";
        //break;
    }
}