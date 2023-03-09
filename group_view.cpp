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
requires rg::view<V>
class group_view : public rg::view_interface<group_view<N, V>> {
    using container_t = V;
    using T = rg::iterator_t<V>::value_type;

public:
    using difference_type = std::iter_difference_t<std::ranges::iterator_t<V>>;

    struct sentinel_t {};
    static inline constexpr sentinel_t sentinel{};

    class iterator {
    public: // <-- important
        using difference_type = std::iter_difference_t<std::ranges::iterator_t<V>>;
        using value_type = typename detail::repeat_type<std::add_lvalue_reference_t<std::add_const_t<T>>, N, std::tuple>::type;
        using pointer = value_type*;
        using reference = value_type&&;
        using iterator_category	= std::input_iterator_tag;

    private:
        const V* m_cont{};
        rg::iterator_t<V> m_iter{};

        template<std::size_t>
        static decltype(auto) getIterValue(rg::iterator_t<V>& where) noexcept(noexcept(std::declval<rg::iterator_t<V>>()++)) {
            return *where++;
        }

        template<std::size_t... Is>
        auto fill_tuple(std::index_sequence<Is...>) const noexcept {
            auto iter = m_iter;
            return value_type{ getIterValue<Is>(iter)... };
        }

    public:
        iterator() = default;
        iterator(const V* cont, rg::iterator_t<V> iter) : m_cont{ cont }, m_iter{ iter } {}
        iterator(const V* cont, sentinel_t) : m_cont{ cont }, m_iter{ std::cend(*m_cont) } {}

        value_type operator*() const {  // <-- const important!
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

        bool operator==(rg::iterator_t<V> rhs) const {
            return m_iter == rhs;
        }
    };

    group_view(V cont) : m_base{ std::move(cont) }, begin_{ iterator{ &m_base, std::cbegin(cont) } }, end_{ iterator{ &m_base, std::cend(cont) } } {
        static_assert(std::input_iterator<iterator>);
        static_assert(std::sentinel_for<rg::iterator_t<V>, iterator>);
    }
    auto begin() const { return begin_; }
    auto end() const { return end_; }

private:
    V m_base;
    iterator begin_{};
    iterator end_{};
};

template<std::size_t N>
struct group_view_fn {
    template<rg::viewable_range R>
    constexpr auto operator()(R&& range) const 
        -> group_view<N, rg::views::all_t<R>>
    {
        return group_view<N, rg::views::all_t<R>>{ rg::views::all(std::forward<R>(range)) };
    }
};

template<std::size_t N>
static inline constexpr auto group = group_view_fn<N>{};



int main() {
    std::vector<int> v{ 10, 20, 30, 100, 200, 300, 120, 140, 920 };
    auto gv = group<3>(v);

    static_assert(rg::view<decltype(gv)>);

    for (auto tup : gv) {
        std::cout << std::get<0>(tup) << ", " << std::get<1>(tup) << ", " << std::get<2>(tup) << "\n";
    }
}