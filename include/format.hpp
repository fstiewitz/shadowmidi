/*
 * Copyright 2020 Fabian Stiewitz <fabian@stiewitz.pw>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef FORMAT_HPP
#define FORMAT_HPP

#include <any>
#include <iostream>
#include <stdexcept>
#include <functional>
#include <cstring>
#include <memory>
#include <utility>
#include <variant>

#define FORMAT_HPP_TYPE(x) \
    template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., x>;

#define FORMAT_HPP_INTERFACE_DECL \
    template<typename S, bool InternalOverwrite = false, bool NewInternal = false > \
    struct Interface

#define FORMAT_HPP_INTERFACE \
    static constexpr bool Internal = InternalOverwrite ? NewInternal : false; \
    using Stack = std::remove_reference_t<S>; \
    stream_type_t<Stack> &processor; \
    Stack &stack;


namespace format {

    struct binary_eof : public std::exception {
    };
    struct binary_write_error : public std::exception {
    };
    struct binary_read_error : public std::exception {
    };
    struct [[maybe_unused]] constant_mismatch : public std::exception {
    };
    struct [[maybe_unused]] get_mismatch : public std::exception {
    };
    struct [[maybe_unused]] no_matching_case : public std::exception {
        no_matching_case(std::any val): std::exception(), _val(std::move(val)) {}
        std::any _val;
    };
    struct [[maybe_unused]] bitfield_range_error : public std::exception {
    };

    template<typename From, typename To, bool FA, bool FT>
    struct is_promoted_cast_arithmetic_check {
        static constexpr bool value = false;
    };

    template<typename From, typename To>
    struct is_promoted_cast_arithmetic_check<From, To, true, true> {
        static constexpr bool value = !std::numeric_limits<To>::is_integer
                                      || (std::numeric_limits<From>::is_integer
                                          && std::numeric_limits<From>::min() >= std::numeric_limits<To>::min()
                                          && std::numeric_limits<From>::max() <= std::numeric_limits<To>::max());
    };
    template<typename From, typename To>
    struct is_promoted_cast {
        static constexpr bool value = is_promoted_cast_arithmetic_check<From, To, std::is_arithmetic_v<From>, std::is_arithmetic_v<To>>::value;
    };

    template<typename From, typename To> static constexpr bool is_promoted_cast_v = is_promoted_cast<From, To>::value;

    template<template<typename, typename...> typename P, typename OP, typename R, typename ...A>
    struct [[maybe_unused]] default_process {
        using V = P<A...>;
    };

    enum process_op_t {
        UNPACKGLOBALS [[maybe_unused]],
        GETTYPE
    };

    using OpUnpackGlobals = std::integral_constant<int, UNPACKGLOBALS>;
    using OpGetType = std::integral_constant<int, GETTYPE>;

    template<typename S, typename T=void, typename ...Rest>
    struct unpack_globals {
        using TStack = typename T::template Process<format::unpack_globals, OpUnpackGlobals, std::bool_constant<S::IsReadStack>, S>::V::Stack;
        using Stack = typename unpack_globals<TStack, Rest...>::Stack;
    };

    template<typename S>
    struct unpack_globals<S, void> {
        using Stack = S;
    };

    template<typename T, bool O>
    struct base_type {
        using Type = T;
    };

    template<typename T>
    struct base_type<T, true> {
        using Type = typename T::value_type;
    };

    template<typename V, typename T = void, typename ...Rest>
    struct get_type {
        /*
         * Requires qualified name
         */
        static constexpr bool HasVariable = T::template Process<format::get_type, OpGetType, std::bool_constant<true>, V>::V::HasVariable;
        using LocalTypeResult = typename T::template Process<format::get_type, OpGetType, std::bool_constant<true>, V>::V;
        using VariableType = typename std::conditional_t<LocalTypeResult::HasVariable,
                LocalTypeResult,
                get_type<V, Rest...>>::VariableType;
    };

    template<typename V>
    struct get_type<V, void> {
        static constexpr bool HasVariable = false;
        using VariableType = void;
    };

    template<typename T>
    struct get_result {
        static constexpr bool HasVariable = true;
        using VariableType = T;
    };

    template<typename S, typename T = void, typename ...Rest>
    struct has_next_external {
        using Value = std::conditional_t<!T::template Interface<S>::Internal, std::bool_constant<true>, typename has_next_external<S, Rest...>::Value>;
    };

    template<typename S>
    struct has_next_external<S, void> {
        using Value = std::bool_constant<false>;
    };

    template<typename S, typename T = void, typename T2 = void, typename ...Rest>
    struct unpack_types {

        template<bool SelfInternal, bool NextExternal, bool _ = false>
        struct _unpack_types {
            using type = std::tuple<>;
        };

        template<bool _>
        struct [[maybe_unused]] _unpack_types<false, true, _> {
            using type = decltype(std::tuple_cat(std::declval<std::tuple<typename T::template Interface<S>::Type>>(),
                                                 std::declval<typename unpack_types<S, T2, Rest...>::type>()));
        };

        template<bool _>
        struct [[maybe_unused]] _unpack_types<true, true, _> {
            using type = typename unpack_types<S, T2, Rest...>::type;
        };

        template<bool _>
        struct [[maybe_unused]] _unpack_types<false, false, _> {
            using type = std::tuple<typename T::template Interface<S>::Type>;
        };


        using type = typename _unpack_types<T::template Interface<S>::Internal, has_next_external<S, T2, Rest...>::Value::value>::type;
    };

    template<bool R, typename V, typename N>
    struct InternalStack {
        static constexpr bool IsReadStack = R;
        static constexpr bool IsWriteStack = !R;
        static constexpr bool IsStack = true;
        static constexpr bool IsStackEnd = false;
        using Next = N;
        using Val = V;
        template<int NO> using Has = std::conditional_t<
                NO == V::ID, std::bool_constant<true>, typename Next::template Has<NO>>;
        V v;
        Next n;

        InternalStack() = default;

        InternalStack(const InternalStack &other) = delete;

        InternalStack(InternalStack &&other) noexcept = default;

        template<typename V1, typename N1>
        explicit InternalStack(const InternalStack<!R, V1, N1> &other): v(other.template get<V::ID>().val), n(other) {}

        template<typename V1, typename N1>
        explicit InternalStack(InternalStack<!R, V1, N1> &&other): v(std::move(other.template get<V::ID>())),
                                                                   n(std::move(other)) {}

        explicit InternalStack(V &&i) : v{std::move(i)}, n{} {}

        InternalStack(V &&i, Next &&j) : v{std::move(i)}, n{std::move(j)} {}

        explicit InternalStack(Next &&j) : v{}, n(std::move(j)) {}

        InternalStack &operator=(InternalStack &&other) noexcept = default;

        template<int G, typename std::enable_if_t<G == V::ID, int> = 0>
        auto &get() {
            return v;
        }

        template<int G, typename std::enable_if_t<G != V::ID, int> = 0>
        auto &get() {
            return n.template get<G>();
        }

        template<int G, typename std::enable_if_t<G == V::ID, int> = 0>
        auto &get() const {
            return v;
        }

        template<int G, typename std::enable_if_t<G != V::ID, int> = 0>
        auto &get() const {
            return n.template get<G>();
        }

        void debug() {
            v.debug();
            n.debug();
        }

    };

    template<int N, typename T, typename F, bool O = false, bool V = false, bool R = false>
    struct InternalVariable {
        static constexpr int ID = N;
        static constexpr bool Optional = O;
        static constexpr bool Variant = V;
        using FormatType = F;
        using BaseType = typename base_type<T, O>::Type;
        using Type = T;
        Type val;

        InternalVariable() = default;

        InternalVariable(const InternalVariable &) = delete;

        InternalVariable(InternalVariable &&) noexcept = default;

        explicit InternalVariable(const Type &o) : val(o) {}

        explicit InternalVariable(Type &&v) : val(std::move(v)) {}

        InternalVariable &operator=(InternalVariable &&other) noexcept = default;

        operator Type() {
            return val;
        }

        template<bool O1 = O, std::enable_if_t<!O1, int> = 0>
        auto &currentValue() {
            return val;
        }

        template<bool O1 = O, bool R1 = R, std::enable_if_t<O1 && !R1, int> = 0>
        auto &currentValue() {
            return val.value();
        }

        template<bool O1 = O, bool R1 = R, std::enable_if_t<O1 && R1, int> = 0>
        auto &currentValue() {
            return val.value().get();
        }

        auto &value() const {
            return val;
        }

        template<typename T1=T, typename std::enable_if_t<std::is_integral_v<T1>, int> = 0>
        void debug() {
            std::cerr << std::to_string(N) << "\t-> " << std::to_string(val) << std::endl;
        }

        template<bool O1 = O, std::enable_if_t<!O1, int> = 0>
        void debug() {
            std::cerr << std::to_string(N) << "\t-> " << typeid(T).name() << std::endl;
        }

        template<bool O1 = O, std::enable_if_t<O1, int> = 0>
        void debug() {
            if (val) {
                std::cerr << std::to_string(N) << "\t-> std::optional " << val.value() << std::endl;
            } else {
                std::cerr << std::to_string(N) << "\t-> std::optional unset" << std::endl;
            }
        }

    };

    template<bool R, typename I=void, typename J=void>
    struct _merge_stacks {
        template<bool ignore, bool _ = false>
        struct remove_duplicate {
            using Stack = typename _merge_stacks<R, typename I::Next, J>::Stack;
        };

        template<bool _>
        struct remove_duplicate<false, _> {
            using Stack = InternalStack<R, typename I::Val, typename _merge_stacks<R, typename I::Next, J>::Stack>;
        };

        using Stack = typename remove_duplicate<J::template Has<I::Val::ID>::value>::Stack;
    };

    template<bool R, typename J>
    struct _merge_stacks<R, InternalStack<R, void, void>, J> {
        using Stack = InternalStack<J::IsReadStack, typename J::Val, typename _merge_stacks<J::IsReadStack, typename J::Next, InternalStack<R, void, void>>::Stack>;
    };

    template<bool R>
    struct _merge_stacks<R, InternalStack<R, void, void>, InternalStack<R, void, void>> {
        using Stack = InternalStack<R, void, void>;
    };

    template<typename I, typename J> using merge_stacks = _merge_stacks<I::IsReadStack, I, J>;

    template<typename T, typename V1=void, typename ...Rest>
    struct pack_has_type {
        template<bool O, bool X = false>
        struct _pack_has_type {
            static constexpr bool value = true;
        };

        template<bool X>
        struct _pack_has_type<false, X> {
            static constexpr bool value = pack_has_type<T, Rest...>::value;
        };

        static constexpr bool value = _pack_has_type<std::is_same_v<T, V1>>::value;
    };

    template<typename T>
    struct pack_has_type<T, void> {
        static constexpr bool value = false;
    };

    template<typename ...Args>
    struct variant {

        template<template<typename...> typename A> using Call = A<Args...>;

        template<typename S>
        struct Interface {

            template<typename T>
            struct Add {
                template<bool _S, bool X = false>
                struct _Add {
                    using Type = variant<Args..., T>;
                };
                template<bool X>
                struct _Add<true, X> {
                    using Type = variant;
                };
                using Type = typename _Add<pack_has_type<T, Args...>::value>::Type;
            };

            template<typename A1 = void, typename A2=void, typename ...Args2>
            struct _Merge {
                using Type = typename Add<A1>::Type::template Interface<S>::template _Merge<A2, Args2...>::Type;
            };

            template<typename A1>
            struct _Merge<A1, void> {
                using Type = typename Add<A1>::Type;
            };

            template<typename T> using Merge = typename T::template Call<_Merge>;

            using Type = std::variant<typename Args::template Interface<S>::Type...>;
        };

    };

    template<typename I, typename J>
    struct optional_variant_variables {
        template<bool MakeOptional, bool _ = false>
        struct process_optional {
            template<bool first_is_variant, bool second_is_variant, bool is_same, typename T1, typename T2>
            struct possibly_variant {
                template<bool O1, bool O2, bool __ = false>
                struct reduce_optional {
                    using Type = T1;
                };
                template<bool __>
                struct reduce_optional<true, true, __> {
                    using Type = T1;
                };
                template<bool __>
                struct reduce_optional<true, false, __> {
                    using Type = T1;
                };
                template<bool __>
                struct reduce_optional<false, true, __> {
                    using Type = T2;
                };

                using Type = typename reduce_optional<T1::Optional, T2::Optional>::Type;
            };
            template<typename T1, typename T2>
            struct possibly_variant<false, false, false, T1, T2> {
                using VariantType = variant<typename T1::FormatType, typename T2::FormatType>;
                using Type = InternalVariable<T1::ID, typename VariantType::template Interface<InternalStack<true, void, void>>::Type, VariantType, false, true>;
            };

            template<typename T1, typename T2>
            struct possibly_variant<true, false, false, T1, T2> {
                using VariantType = typename T1::FormatType::template Interface<InternalStack<true, void, void>>::template Add<typename T2::FormatType>::Type;
                using Type = InternalVariable<T1::ID, typename VariantType::template Interface<InternalStack<true, void, void>>::Type, VariantType, false, true>;
            };

            template<typename T1, typename T2>
            struct possibly_variant<false, true, false, T1, T2> {
                using VariantType = typename T2::FormatType::template Interface<InternalStack<true, void, void>>::template Add<typename T1::FormatType>::Type;
                using Type = InternalVariable<T1::ID, typename VariantType::template Interface<InternalStack<true, void, void>>::Type, VariantType, false, true>;
            };

            template<typename T1, typename T2>
            struct possibly_variant<true, true, false, T1, T2> {
                using VariantType = typename T1::FormatType::template Interface<InternalStack<true, void, void>>::template Merge<typename T2::FormatType>::Type;
                using Type = InternalVariable<T1::ID, typename VariantType::template Interface<InternalStack<true, void, void>>::Type, VariantType, false, true>;
            };

            using ID = std::integral_constant<int, I::Val::ID>;
            using CurrentType = typename I::Val;
            using OtherType = std::remove_reference_t<decltype(std::declval<J>().template get<ID::value>())>;
            using CurrentBaseType = typename CurrentType::BaseType;
            using OtherBaseType = typename OtherType::BaseType;
            using Stack = InternalStack<I::IsReadStack, typename possibly_variant<CurrentType::Variant, OtherType::Variant, std::is_same_v<CurrentBaseType, OtherBaseType>, CurrentType, OtherType>::Type, typename optional_variant_variables<typename I::Next, J>::Stack>;
        };

        template<bool _>
        struct process_optional<false, _> {
            using Stack = InternalStack<I::IsReadStack, InternalVariable<I::Val::ID, std::optional<typename I::Val::BaseType>, typename I::Val::FormatType, true>, typename optional_variant_variables<typename I::Next, J>::Stack>;
        };

        using InFalse = typename J::template Has<I::Val::ID>;

        using Stack = typename process_optional<InFalse::value>::Stack;
    };

    template<typename J>
    struct optional_variant_variables<InternalStack<J::IsReadStack, void, void>, J> {
        using Stack = InternalStack<J::IsReadStack, void, void>;
    };

    template<>
    struct optional_variant_variables<InternalStack<true, void, void>, InternalStack<true, void, void>> {
        using Stack = InternalStack<true, void, void>;
    };

    template<>
    struct optional_variant_variables<InternalStack<false, void, void>, InternalStack<false, void, void>> {
        using Stack = InternalStack<false, void, void>;
    };

    template<typename S, typename A = void, typename B = void, typename C = void, typename ...Args>
    struct combined_type {
        using Type = typename variant<A>::template Interface<S>::template _Merge<B, C, Args...>::Type::template Interface<S>::Type;
    };

    template<typename S, typename A, typename B>
    struct combined_type<S, A, B, void> {
        template<bool T, bool X = false>
        struct _combined_type {
            using Type = typename A::template Interface<S>::Type;
        };

        template<bool X>
        struct _combined_type<false, X> {
            using Type = typename variant<A, B>::template Interface<S>::Type;
        };

        using Type = typename _combined_type<std::is_same_v<A, B>>::Type;
    };


    template<typename V, typename R>
    struct unpack_result {
        using Stack = InternalStack<R::IsReadStack, V, R>;
    };

    template<int I, typename T = void, typename ...Rest>
    struct pack_element {
        template<bool C, int = 0>
        struct _pack_element {
            using Type = T;
        };

        template<int X>
        struct _pack_element<false, X> {
            using Type = typename pack_element<I - 1, Rest...>::Type;
        };

        using Type = typename _pack_element<I == 0>::Type;

    };

    template<typename ...Args>
    struct tuple {
        template<typename ...E> using Add = tuple<Args..., tuple<E...>>;
        template<template<typename...> typename P> using Call = P<Args...>;
    };

    template<template<typename...> typename Result, typename P, int I, typename ...Rest>
    struct permutate_first_with_all {
        template<bool N, int = 0>
        struct _permutate_first_with_all {
            using Type = typename permutate_first_with_all<Result, typename P::template Add<typename pack_element<I, Rest...>::Type, Rest...>,
                    I + 1, Rest...>::Type;
        };

        template<int X>
        struct _permutate_first_with_all<true, X> {
            using Type = typename P::template Call<Result>;
        };

        using Type = typename _permutate_first_with_all<I == sizeof...(Rest)>::Type;
    };

    template<template<typename, typename> typename ReduceFun, typename I>
    struct reduce {
        template<int X, typename T = void, typename ...Args>
        struct _reduce {
            using Type = typename ReduceFun<T, typename reduce<ReduceFun, I>::template _reduce<X, Args...>::Type>::Type;
        };

        template<int X>
        struct _reduce<X, void> {
            using Type = I;
        };

        template<typename ...Args> using Operation = _reduce<0, Args...>;
    };

    template<bool R>
    struct merge_and_unwind {

        struct _merge_and_unwind {
            template<typename T1, typename T2>
            struct Operation {
                using T1Vars = typename unpack_globals<InternalStack<R, void, void>, T1>::Stack;
                using T2Vars = T2;
                using OptionalT1Vars = typename optional_variant_variables<T1Vars, T2Vars>::Stack;
                using OptionalT2Vars = typename optional_variant_variables<T2Vars, T1Vars>::Stack;
                using Type = typename merge_stacks<OptionalT1Vars, OptionalT2Vars>::Stack;
            };
            template<typename T1>
            struct Operation<T1, InternalStack<R, void, void>> {
                using Type = typename unpack_globals<InternalStack<R, void, void>, T1>::Stack;
            };
        };

        template<typename T1, typename T2>
        struct Operation {
            using EmptyStack = InternalStack<R, void, void>;
            using ReduceCmd = reduce<_merge_and_unwind::template Operation, EmptyStack>;
            using T1Vars = typename T1::template Call<ReduceCmd::template Operation>::Type;
            using OptionalT1Vars = typename optional_variant_variables<T1Vars, T2>::Stack;
            using OptionalT2Vars = typename optional_variant_variables<T2, T1Vars>::Stack;
            using Type = typename merge_stacks<OptionalT1Vars, OptionalT2Vars>::Stack;
        };

        template<typename T1>
        struct Operation<T1, InternalStack<R, void, void>> {
            using EmptyStack = InternalStack<R, void, void>;
            using ReduceCmd = reduce<_merge_and_unwind::template Operation, EmptyStack>;
            using Type = typename T1::template Call<ReduceCmd::template Operation>::Type;
        };
    };

    template<int I, bool Internal, typename T, typename Next>
    struct external_node_by_index {
        using Node = typename Next::Reader::template ByIndex<I - 1>::Node;
    };

    template<int I, typename T, typename Next>
    struct external_node_by_index<I, true, T, Next> {
        using Node = typename Next::Reader::template ByIndex<I>::Node;
    };

    template<typename T, typename Next>
    struct external_node_by_index<0, false, T, Next> {
        using Node = T;
    };

    template<int I, int C = (I - 1) / 8>
    struct bitfield_type {
        using type = void;
    };

    template<int I>
    struct bitfield_type<I, 0> {
        using type = uint8_t;
    };
    template<int I>
    struct bitfield_type<I, 1> {
        using type = uint16_t;
    };
    template<int I>
    struct bitfield_type<I, 2> {
        using type = uint32_t;
    };
    template<int I>
    struct bitfield_type<I, 3> {
        using type = uint32_t;
    };

    template<bool R, typename V>
    struct InternalStack<R, V, void> {
        static constexpr bool IsReadStack = R;
        static constexpr bool IsWriteStack = !R;
        static constexpr bool IsStack = true;
        static constexpr bool IsStackEnd = true;
        using Val = V;
        template<int N> using Has = std::conditional_t<N == V::ID, std::bool_constant<true>, std::bool_constant<false>>;
        V v;

        InternalStack(const InternalStack &other) = delete;

        InternalStack(InternalStack &&other) noexcept = default;

        template<typename V1, typename N1>
        explicit InternalStack(const InternalStack<!R, V1, N1> &other): v(other.template get<V::ID>()) {}

        template<typename V1, typename N1>
        explicit InternalStack(InternalStack<!R, V1, N1> &&other): v(std::move(other.template get<V::ID>())) {}

        template<int G, typename std::enable_if_t<G == V::ID, int> = 0>
        auto &get() {
            return v;
        }

        template<int G, typename std::enable_if_t<G == V::ID, int> = 0>
        auto &get() const {
            return v;
        }

        void debug() {
            v.debug();
        }
    };

    template<bool R>
    struct InternalStack<R, void, void> {
        static constexpr bool IsReadStack = R;
        static constexpr bool IsWriteStack = !R;
        static constexpr bool IsStack = true;
        static constexpr bool IsStackEnd = true;
        template<int N> using Has = std::bool_constant<false>;

        InternalStack() = default;

        template<typename V1, typename N1>
        InternalStack(const InternalStack<!R, V1, N1> &) {}

        template<typename V1, typename N1>
        explicit InternalStack(InternalStack<!R, V1, N1> &&) {}

        void debug() {}
    };

    using read_stream_t = std::istream;
    using write_stream_t = std::ostream;

    template<bool B>
    struct stream_type {
        using Type = read_stream_t;
    };

    template<>
    struct stream_type<false> {
        using Type = write_stream_t;
    };

    template<typename S> using stream_type_t = typename stream_type<std::remove_reference_t<S>::IsReadStack>::Type;

    template<int N, typename T, bool I = true, bool Ref = false>
    struct Variable {
        using VariableType = T;

        template<template<typename, typename...> typename P, typename OP, typename R, typename ...A>
        struct Process {
            using V = P<A..., T>;
        };

        template<template<typename, typename...> typename P, typename R, typename ...A>
        struct Process<P, OpGetType, R, A...> {
            using V = std::conditional_t<std::is_same_v<std::integral_constant<int, N>, A...>, get_result<T>, P<A..., T>>;
        };

        template<template<typename, typename...> typename P, typename R, typename ...A>
        struct Process<P, OpUnpackGlobals, R, A...> {
            using Type = typename VariableType::template Interface<InternalStack<R::value, void, void>>::Type;
            using V = unpack_result<InternalVariable<N, std::conditional_t<Ref, std::optional<std::reference_wrapper<const Type>>, Type>, T, Ref, false, Ref>, typename P<A..., T>::Stack>;
        };

        template<typename St, bool I1 = I>
        struct Interface {
        };

        template<typename St>
        struct Interface<St, true> {
            static constexpr bool Internal = true;
            using Type = typename T::template Interface<St>::Type;
            using Stack = std::remove_reference_t<St>;

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read(Type &v) {
                typename T::template Interface<St>{processor, stack}.read(v);
                stack.template get<N>().val = v;
            }

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read() {
                typename T::template Interface<St>{processor, stack}.read(stack.template get<N>().currentValue());
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write(const Type &v) {
                typename T::template Interface<St>{processor, stack}.write(v);
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write() {
                typename T::template Interface<St>{processor, stack}.write(stack.template get<N>().currentValue());
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };

        template<typename St>
        struct Interface<St, false> {
            static constexpr bool Internal = false;
            using Type = typename T::template Interface<St>::Type;
            using Stack = St;

            void read(Type &v) {
                typename T::template Interface<Stack>{processor, stack}.read(v);
                stack.template get<N>().val = v;
            }

            void write(const Type &v) {
                typename T::template Interface<Stack>{processor, stack}.write(v);
                stack.template get<N>().val = v;
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };

    };

    template<int N, typename T> using Set = Variable<N, T, true>;
    template<int N, typename T> using Copy = Variable<N, T, false>;
    template<int N, typename T> using Ref = Variable<N, T, false, true>;


    enum Type {
        READER = 0,
        WRITER = 1
    };

    template<int TYPE, bool TopLevel, typename ProcessorType, typename T, typename S, typename ...R>
    struct Node {
    };

    template<typename S, bool TopLevel>
    struct StackContainer {
        using Stack = std::conditional_t<TopLevel, S, S &>;

        template<bool D = TopLevel, std::enable_if_t<D, int> = 0>
        auto stack() {
            return std::move(_stack);
        }

        template<typename T, typename S1>
        StackContainer(T &&, S1 &&stack): _stack(std::forward<S1>(stack)) {}

        Stack _stack;
    };

    template<typename S, bool TopLevel, typename ProcessorType, typename T = void, typename ...Rest>
    struct InternalFormat {

        static constexpr bool End = false;
        using Reader = Node<READER, TopLevel, ProcessorType, T, S, Rest...>;
        using Writer = Node<WRITER, TopLevel, ProcessorType, T, S, Rest...>;
        template<int I> using Type = typename Reader::template ByIndex<I>::Node::template Interface<S>::Type;

        template<typename ...A>
        static auto reader(A &&...fun) {
            return Reader{std::forward<A>(fun)...};
        }

        template<typename ...A>
        static auto writer(A &&...fun) {
            return Writer{std::forward<A>(fun)...};
        }
    };

    template<typename S, bool TopLevel, typename ProcessorType>
    struct InternalFormat<S, TopLevel, ProcessorType, void> {
        static constexpr bool End = true;
        struct Reader {
            static constexpr bool Internal = true;
        };
        struct Writer {
            static constexpr bool Internal = true;
        };

        template<typename ...A>
        static auto reader(A &&...fun) {
            return StackContainer<S, TopLevel>{std::forward<A>(fun)...};
        }

        template<typename ...A>
        static auto writer(A &&...fun) {
            return StackContainer<S, TopLevel>{std::forward<A>(fun)...};
        }
    };

    template<typename S, typename ...Rest> using SubFile = InternalFormat<S, false, stream_type_t<S> &, Rest...>;

    template<typename R, typename S, typename T = void, typename ...Rest>
    struct [[nodiscard]] StackPreparation {
        using Stack = S;
        using ProcessorType = std::remove_reference_t<R>;
        using Processor = std::conditional_t<std::is_rvalue_reference_v<R>, ProcessorType, std::reference_wrapper<ProcessorType>>;
        using Reader = Node<READER, true, R, T, S, Rest...>;
        using Writer = Node<WRITER, true, R, T, S, Rest...>;

        template<typename ST, typename S1=std::remove_reference_t<ST>, std::enable_if_t<
                S1::IsStack && !S1::IsStackEnd, int> = 0>
        auto stack(ST &&s) {
            auto v = Stack{std::forward<ST>(s)};
            return StackPreparation<R, decltype(v), T, Rest...>{std::move(processor), std::move(v)};
        }

        auto reader() {
            return Reader{std::move(processor), std::move(variables)};
        }

        auto writer() {
            return Writer{std::move(processor), std::move(variables)};
        }

        template<int V, typename A, std::enable_if_t<!Stack::template Has<V>::value, int> = 0>
        auto stack(A &&arg) {
            using VType = typename get_type<std::integral_constant<int, V>, T, Rest...>::VariableType;
            using Type = typename VType::template Interface<Stack>::Type;
            auto s = InternalStack<S::IsReadStack, InternalVariable<V, Type, VType>, Stack>{std::forward<A>(arg),
                                                                                            std::move(variables)};
            return StackPreparation<R, decltype(s), T, Rest...>{std::move(processor), std::move(s)};
        }

        template<int V, typename A, typename S1=Stack, std::enable_if_t<S1::template Has<V>::value, int> = 0>
        auto stack(A &&arg) {
            variables.template get<V>().val = std::forward<A>(arg);
            return StackPreparation<R, decltype(variables), T, Rest...>{std::move(processor), std::move(variables)};
        }

        template<typename ...A>
        auto read(A &&...args) {
            return reader().read(std::forward<A>(args)...);
        }

        template<typename ...A>
        auto write(A &&...args) {
            return writer().write(std::forward<A>(args)...);
        }

        auto debug() {
            variables.debug();
            return *this;
        }

        Processor processor;
        S variables;
    };

    template<typename T = void, typename ...Rest>
    struct Format {

        static constexpr bool End = false;
        using ReadStack = typename unpack_globals<InternalStack<true, void, void>, T, Rest...>::Stack;
        using WriteStack = typename unpack_globals<InternalStack<false, void, void>, T, Rest...>::Stack;
        template<int I> using Type = typename StackPreparation<read_stream_t &&, ReadStack, T, Rest...>::Reader::template ByIndex<I>::Node::template Interface<ReadStack>::Type;

        template<typename R>
        static auto reader(R &&reader) {
            return StackPreparation<R &&, ReadStack, T, Rest...>{std::forward<R>(reader), {}};
        }

        template<typename R>
        static auto writer(R &&writer) {
            return StackPreparation<R &&, WriteStack, T, Rest...>{std::forward<R>(writer), {}};
        }
    };

    template<typename T, bool TopLevel, typename ProcessorType, typename S, typename ...R>
    struct [[nodiscard]] Node<READER, TopLevel, ProcessorType, T, S, R...> {
        using Type = T;
        using Next = InternalFormat<S, TopLevel, ProcessorType, R...>;
        using Stack = std::conditional_t<TopLevel, S, S &>;
        using Processor = std::conditional_t<
                TopLevel && std::is_rvalue_reference_v<ProcessorType>, std::decay_t<ProcessorType>,
                std::reference_wrapper<std::decay_t<ProcessorType>>>;
        static constexpr bool Internal = T::template Interface<S>::Internal;
        template<int I> using ByIndex = external_node_by_index<I, Internal, Type, Next>;

        template<typename ...Args, typename T1=T, typename R1=Next, typename std::enable_if_t<
                TopLevel && !T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                !R1::End && R1::Reader::Internal, int> = 0>
        auto read(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.read(std::forward<Args>(args)...);
            return next().read();
        }

        template<typename ...Args, typename T1=T, typename R1=Next, typename std::enable_if_t<
                TopLevel && T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                !R1::End && R1::Reader::Internal, int> = 0>
        auto read(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.read();
            return next().read(std::forward<Args>(args)...);
        }

        template<typename T1=T, typename R1=Next, typename std::enable_if_t<
                TopLevel && T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                R1::End || !R1::Reader::Internal, int> = 0>
        auto read() {
            typename T::template Interface<S>{processor, stack}.read();
            return next();
        }

        template<typename ...Args, typename T1=T, typename R1=Next, typename std::enable_if_t<
                !TopLevel || !T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                R1::End || !R1::Reader::Internal, int> = 0>
        auto read(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.read(std::forward<Args>(args)...);
            return next();
        }

        auto debug() {
            stack.n.debug();
            return *this;
        }

        Processor processor;
        Stack stack;
    private:
        template<bool D = TopLevel, std::enable_if_t<!D, int> = 0>
        auto next() {
            return Next::reader(std::move(processor), stack);
        }

        template<bool D = TopLevel, std::enable_if_t<D, int> = 0>
        auto next() {
            return Next::reader(std::move(processor), std::move(stack));
        }

    };

    template<typename T, bool TopLevel, typename ProcessorType, typename S, typename ...R>
    struct [[nodiscard]] Node<WRITER, TopLevel, ProcessorType, T, S, R...> {
        using Type = T;
        using Stack = std::conditional_t<TopLevel, S, S &>;
        using Processor = std::conditional_t<
                TopLevel && std::is_rvalue_reference_v<ProcessorType>, std::decay_t<ProcessorType>,
                std::reference_wrapper<std::decay_t<ProcessorType>>>;
        using Next = InternalFormat<S, TopLevel, ProcessorType, R...>;
        static constexpr bool Internal = T::template Interface<S>::Internal;
        template<int I> using ByIndex = external_node_by_index<I, Internal, Type, Next>;

        template<typename ...Args>
        auto write(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.write(std::forward<Args>(args)...);
            return next();
        }

        auto debug() {
            stack.n.debug();
            return *this;
        }

        Processor processor;
        Stack stack;
    private:
        template<bool D = TopLevel, std::enable_if_t<!D, int> = 0>
        auto next() {
            return Next::writer(std::move(processor), stack);
        }

        template<bool D = TopLevel, std::enable_if_t<D, int> = 0>
        auto next() {
            return Next::writer(std::move(processor), std::move(stack));
        }

    };

    template<typename ProcessorType, typename T, typename S, typename ...R>
    struct [[nodiscard]] Node<WRITER, true, ProcessorType, T, S, R...> {
        using Type = T;
        using Stack = S;
        using Processor = std::conditional_t<std::is_rvalue_reference_v<ProcessorType>, std::decay_t<ProcessorType>,
                std::reference_wrapper<std::decay_t<ProcessorType>>>;
        using Next = InternalFormat<S, true, ProcessorType, R...>;
        static constexpr bool Internal = T::template Interface<S>::Internal;
        template<int I> using ByIndex = external_node_by_index<I, Internal, Type, Next>;

        template<typename ...Args, typename T1=T, typename R1=Next, typename std::enable_if_t<
                !T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                !R1::End && R1::Reader::Internal, int> = 0>
        auto write(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.write(std::forward<Args>(args)...);
            return next().write();
        }

        template<typename ...Args, typename T1=T, typename R1=Next, typename std::enable_if_t<
                T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                !R1::End && R1::Reader::Internal, int> = 0>
        auto write(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.write();
            return next().write(std::forward<Args>(args)...);
        }

        template<typename T1=T, typename R1=Next, typename std::enable_if_t<
                T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                R1::End || !R1::Reader::Internal, int> = 0>
        auto write() {
            typename T::template Interface<S>{processor, stack}.write();
            return next();
        }

        template<typename ...Args, typename T1=T, typename R1=Next, typename std::enable_if_t<
                !T1::template Interface<S>::Internal, int> = 0, typename std::enable_if_t<
                R1::End || !R1::Reader::Internal, int> = 0>
        auto write(Args &&...args) {
            typename T::template Interface<S>{processor, stack}.write(std::forward<Args>(args)...);
            return next();
        }

        auto debug() {
            stack.n.debug();
            return *this;
        }

        Processor processor;
        Stack stack;
    private:
        auto next() {
            return Next::writer(std::move(processor), std::move(stack));
        }

    };

    template<typename C, typename T=void>
    struct Constant {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., T>;

        template<typename S>
        struct Interface {
            static constexpr bool Internal = true;
            using Stack = S;

            template<typename Cmp>
            static bool matches(const Cmp &cmp) {
                return C::value == cmp;
            }

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read() {
                typename T::template Interface<S>::Type tv;
                SubFile<S, T>::reader(processor, stack).read(tv);
                if (tv != C::value) {
                    throw constant_mismatch();
                }
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write() {
                SubFile<S, T>::writer(processor, stack).write(C::value);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<typename C, typename T>
    struct Case {
        using Format = T;
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., T>;

        template<typename S>
        struct Interface {
            using Type = typename T::template Interface<S>::Type;
            using Stack = S;

            template<typename Cmp>
            static bool matches(const Cmp &cmp) {
                return C::template Interface<S>::matches(cmp);
            }

            void read(Type &t) {
                SubFile<S, T>::reader(processor, stack).read(t);
            }

            void write(const Type &t) {
                SubFile<S, T>::writer(processor, stack).write(t);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<typename T>
    struct Default {
        using Format = T;
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., T>;

        template<typename S>
        struct Interface {
            using Type = typename T::template Interface<S>::Type;
            using Stack = S;

            template<typename Cmp>
            static bool matches(const Cmp &) {
                return true;
            }

            void read(Type &t) {
                SubFile<S, T>::reader(processor, stack).read(t);
            }

            void write(const Type &t) {
                SubFile<S, T>::writer(processor, stack).write(t);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<typename Cmp, typename Case = void, typename ...Cases>
    struct Switch {

        template<template<typename, typename...> typename P, typename OP, typename R, typename ...A>
        struct Process {
            using V = P<A..., Cmp, Case, Cases...>;
        };

        template<bool R>
        struct Variables {
            using EmptyStack = InternalStack<R, void, void>;
            using ReduceCmd = reduce<merge_and_unwind<R>::template Operation, EmptyStack>;
            using Stack = typename permutate_first_with_all<ReduceCmd::template Operation, tuple<>, 0, Case, Cases...>::Type::Type;
        };

        template<template<typename, typename...> typename P, typename R, typename ...A>
        struct Process<P, OpUnpackGlobals, R, A...> {
            using EmptyStack = InternalStack<R::value, void, void>;
            using CVars = typename unpack_globals<EmptyStack, Cmp>::Stack;
            using V = unpack_globals<typename merge_stacks<CVars, typename merge_stacks<typename P<A..., void>::Stack, typename Variables<R::value>::Stack>::Stack>::Stack, void>;
        };

        template<typename S>
        struct Interface {
            static constexpr bool Internal = false;
            using CmpType = typename Cmp::template Interface<S>::Type;
            using Type = std::variant<typename Case::template Interface<S>::Type, typename Cases::template Interface<S>::Type...>;
            using Stack = S;

            template<typename V>
            void internalRead(const CmpType &cmp, V &val) {
                if (Case::template Interface<S>::matches(cmp)) {
                    typename Case::template Interface<S>::Type tv;
                    SubFile<S, typename Case::Format>::reader(processor, stack).read(tv);
                    val = std::move(tv);
                } else {
                    (typename Switch<Cmp, Cases...>::template Interface<S>{processor, stack}).internalRead(cmp, val);
                }
            }

            template<typename V>
            void internalWrite(const CmpType &cmp, const V &val) {
                if (Case::template Interface<S>::matches(cmp)) {
                    SubFile<S, typename Case::Format>::writer(processor, stack).write(
                            std::get<typename Case::template Interface<S>::Type>(val));
                } else {
                    (typename Switch<Cmp, Cases...>::template Interface<S>{processor, stack}).internalWrite(cmp, val);
                }
            }


            void read(Type &v) {
                CmpType cmp;
                SubFile<S, Cmp>::reader(processor, stack).read(cmp);
                internalRead(cmp, v);
            }

            void write(const Type &v) {
                CmpType cmp;
                (typename Cmp::template Interface<Stack>{processor, stack}).read(cmp);
                internalWrite(cmp, v);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<typename Cmp>
    struct Switch<Cmp, void> {
        template<typename S>
        struct Interface {
            using CmpType = typename Cmp::template Interface<S>::Type;

            template<typename V>
            void internalRead(const CmpType &cmp, V &) {
                throw no_matching_case(cmp);
            }

            template<typename V>
            void internalWrite(const CmpType &cmp, const V &) {
                throw no_matching_case(cmp);
            }

            stream_type_t<S> &processor;
            S &stack;
        };
    };

    template<typename Cmp, typename T, typename F=void>
    struct Conditional {
        template<template<typename, typename...> typename P, typename OP, typename R, typename ...A>
        struct Process {
            using V = P<A..., Cmp, T, F>;
        };

        template<template<typename, typename...> typename P, typename R, typename ...A>
        struct Process<P, OpUnpackGlobals, R, A...> {
            using CVars = typename unpack_globals<InternalStack<R::value, void, void>, Cmp>::Stack;
            using TVars = typename unpack_globals<InternalStack<R::value, void, void>, T>::Stack;
            using FVars = typename unpack_globals<InternalStack<R::value, void, void>, F>::Stack;
            using OptionalTVars = typename optional_variant_variables<TVars, FVars>::Stack;
            using OptionalFVars = typename optional_variant_variables<FVars, TVars>::Stack;
            using Vars = typename merge_stacks<OptionalTVars, OptionalFVars>::Stack;
            using V = unpack_globals<typename merge_stacks<CVars, typename merge_stacks<typename P<A..., void>::Stack, Vars>::Stack>::Stack, void>;
        };

        template<typename S>
        struct Interface {
            static constexpr bool Internal = false;
            using CmpType = typename Cmp::template Interface<S>::Type;
            using GoodType = typename T::template Interface<S>::Type;
            using BadType = typename F::template Interface<S>::Type;
            using Type = typename combined_type<S, T, F>::Type;
            using Stack = S;

            template<typename T1=T, typename F1=F, std::enable_if_t<!std::is_same_v<T1, F1>, int> = 0>
            void read(Type &v) {
                CmpType cmp;
                SubFile<S, Cmp>::reader(processor, stack).read(cmp);
                if (cmp) {
                    GoodType tv;
                    SubFile<S, T>::reader(processor, stack).read(tv);
                    v = std::move(tv);
                } else {
                    BadType tv;
                    SubFile<S, F>::reader(processor, stack).read(tv);
                    v = std::move(tv);
                }
            }

            template<typename T1=T, typename F1=F, std::enable_if_t<std::is_same_v<T1, F1>, int> = 0>
            void read(Type &v) {
                CmpType cmp;
                SubFile<S, Cmp>::reader(processor, stack).read(cmp);
                GoodType tv;
                SubFile<S, T>::reader(processor, stack).read(tv);
                v = std::move(tv);
            }

            template<typename T1=T, typename F1=F, std::enable_if_t<!std::is_same_v<T1, F1>, int> = 0>
            void write(const Type &v) {
                CmpType cmp;
                (typename Cmp::template Interface<Stack>{processor, stack}).read(cmp);
                if (cmp) {
                    SubFile<S, T>::writer(processor, stack).write(std::get<GoodType>(v));
                } else {
                    SubFile<S, F>::writer(processor, stack).write(std::get<BadType>(v));
                }
            }

            template<typename T1=T, typename F1=F, std::enable_if_t<std::is_same_v<T1, F1>, int> = 0>
            void write(const Type &v) {
                CmpType cmp;
                (typename Cmp::template Interface<Stack>{processor, stack}).read(cmp);
                SubFile<S, T>::writer(processor, stack).write(v);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };


    template<typename Cmp, typename T>
    struct Conditional<Cmp, T, void> {
        template<template<typename, typename...> typename P, typename OP, typename R, typename ...A>
        struct Process {
            using V = P<A..., T>;
        };

        template<template<typename, typename...> typename P, typename R, typename ...A>
        struct Process<P, OpUnpackGlobals, R, A...> {
            using TVars = typename unpack_globals<InternalStack<R::value, void, void>, T>::Stack;
            using Vars = typename optional_variant_variables<TVars, InternalStack<R::value, void, void>>::Stack;
            using V = unpack_globals<typename merge_stacks<typename P<A..., void>::Stack, Vars>::Stack, void>;
        };

        template<typename S>
        struct Interface {
            static constexpr bool Internal = false;
            using CmpType = typename Cmp::template Interface<S>::Type;
            using GoodType = typename T::template Interface<S>::Type;
            using Type = std::optional<GoodType>;
            using Stack = S;

            void read(Type &v) {
                CmpType cmp;
                SubFile<S, Cmp>::reader(processor, stack).read(cmp);
                if (cmp) {
                    GoodType tv;
                    SubFile<S, T>::reader(processor, stack).read(tv);
                    v = std::make_optional(std::move(tv));
                } else {
                    v = std::nullopt;
                }
            }

            void write(const Type &v) {
                CmpType cmp;
                (typename Cmp::template Interface<Stack>{processor, stack}).read(cmp);
                if (cmp) {
                    SubFile<S, T>::writer(processor, stack).write(v.value());
                }
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<int N>
    struct Get {
        static constexpr int ID = N;
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., void>;

        template<typename S, bool InternalOverwrite = false, bool NewInternal = false >
        struct Interface {
            static constexpr bool Internal = InternalOverwrite ? NewInternal : true;
            using VariableType = std::remove_reference_t<decltype(std::declval<S>().template get<N>())>;
            using FormatType = typename VariableType::FormatType;
            using Type = typename VariableType::Type;
            using Stack = S;

            void read() {
                Type v{};
                SubFile<Stack, FormatType>::reader(processor, stack).read(v);
                if (v != stack.template get<N>().val) {
                    throw get_mismatch();
                }
            }

            template<typename T>
            void read(T &v) {
                v = stack.template get<N>().val;
            }

            void write() {
                auto &ref = stack.template get<N>();
                SubFile<Stack, FormatType>::writer(processor, stack).write(ref.val);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<typename T, bool Reading = true, bool Writing = true>
    struct Internal {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., void>;
        template<typename S> using Interface = typename T::template Interface<S, true, std::conditional_t<S::IsReadStack, std::bool_constant<Reading>, std::bool_constant<Writing>>::value>;
    };

    template<auto C, typename T>
    struct Call {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., T>;

        template<typename S, bool InternalOverwrite = false, bool NewInternal = false >
        struct Interface {
            using Stack = std::remove_reference_t<S>;
            static constexpr bool Internal = InternalOverwrite ? NewInternal : std::conditional_t<Stack::IsReadStack, std::bool_constant<false>, std::bool_constant<true>>::value;
            using Type = std::invoke_result_t<decltype(C), const typename T::template Interface<S>::Type&>;

            void read() {
                SubFile<S, T>::reader(processor, stack).read();
            }

            void read(Type &value) {
                typename SubFile<S, format::Internal<T, false, false>>::template Type<0> v;
                SubFile<S, format::Internal<T, false, false>>::reader(processor, stack).read(v);
                value = C(std::move(v));
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write() {
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write(const Type &value) {
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<auto RFun, auto WFun, typename T>
    struct Map {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., T>;

        template<typename S, bool InternalOverwrite = false, bool NewInternal = false >
        struct Interface {
            using Stack = std::remove_reference_t<S>;
            static constexpr bool Internal = InternalOverwrite ? NewInternal : false;
            using Type = std::invoke_result_t<decltype(RFun), const typename T::template Interface<S>::Type&>;

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read() {
                SubFile<S, T>::reader(processor, stack).read();
            }

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read(Type &value) {
                typename SubFile<S, T>::template Type<0> v;
                SubFile<S, T>::reader(processor, stack).read(v);
                value = RFun(std::move(v));
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write() {
                SubFile<S, T>::writer(processor, stack).write();
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write(const Type &value) {
                SubFile<S, T>::writer(processor, stack).write(WFun(value));
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

/* Data Types */

/*!
 * \brief Read sizeof(T) bytes and cast to T.
 * \tparam T type to read
 */
    template<typename T>
    struct Scalar {
        FORMAT_HPP_TYPE(void)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = T;

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read() {
                char data[sizeof(T)];
                processor.read(data, sizeof(T));
                if (processor.eof()) {
                    throw binary_eof();
                }
            }

            /*!
             * \brief Read into variable.
             * \throws binary_eof()
             */
            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read(T &output) {
                char data[sizeof(T)];
                processor.read(data, sizeof(T));
                if (processor.eof()) {
                    throw binary_eof();
                }
                output = *(reinterpret_cast<T *>(data));
            }

            /*!
             * \brief Write from variable.
             */
            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write(const T &input) {
                processor.write(reinterpret_cast<const char *>(&input), sizeof(T));
                if (processor.bad()) {
                    throw binary_write_error();
                }
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write() {}

        };
    };

    template<>
    struct Scalar<void> {
        FORMAT_HPP_TYPE(void)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = uint8_t;

            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read() {
            }

            /*!
             * \brief Read into variable.
             * \throws binary_eof()
             */
            template<typename S1=Stack, std::enable_if_t<S1::IsReadStack, int> = 0>
            void read(Type &output) {
                output = 0;
            }

            /*!
             * \brief Write from variable.
             */
            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write(const Type &input) {
            }

            template<typename S1=Stack, std::enable_if_t<!S1::IsReadStack, int> = 0>
            void write() {}

        };
    };

    template<typename T>
    struct Packed {
        T value;

        Packed() {
            memset(&value, 0, sizeof(T));
        }

        Packed(const T &v) : value(v) {}

        template<int Start, int End, std::enable_if_t<((End - Start) == 7) && ((Start % 8) == 0), int> = 0>
        uint8_t get() const {
            return *(reinterpret_cast<const uint8_t *>(&value) + (Start / 8u));
        }

        template<int Start, int End, std::enable_if_t<((End - Start) == 7) && ((Start % 8) == 0), int> = 0>
        auto apply(const uint8_t &v) {
            *(reinterpret_cast<uint8_t *>(&value) + (Start / 8u)) = v;
            return *this;
        }

        template<int Start, int End, std::enable_if_t<((End - Start) == 15) && ((Start % 8) == 0), int> = 0>
        uint16_t get() const {
            return *(reinterpret_cast<const uint16_t *>(reinterpret_cast<const uint8_t *>(&value) + (Start / 8u)));
        }

        template<int Start, int End, std::enable_if_t<((End - Start) == 15) && ((Start % 8) == 0), int> = 0>
        auto apply(const uint16_t &v) {
            *(reinterpret_cast<uint16_t *>(reinterpret_cast<uint8_t *>(&value) + (Start / 8u))) = v;
            return *this;
        }

        template<int Start, int End, std::enable_if_t<((End - Start) == 31) && ((Start % 8) == 0), int> = 0>
        uint32_t get() const {
            return *(reinterpret_cast<const uint32_t *>(reinterpret_cast<const uint8_t *>(&value) + (Start / 8u)));
        }

        template<int Start, int End, std::enable_if_t<((End - Start) == 31) && ((Start % 8) == 0), int> = 0>
        auto apply(const uint32_t &v) {
            *(reinterpret_cast<uint32_t *>(reinterpret_cast<uint8_t *>(&value) + (Start / 8u))) = v;
            return *this;
        }

        template<int Start, int End, std::enable_if_t<((End - Start) < 7) && ((Start / 8) == (End / 8)), int> = 0>
        uint8_t get() const {
            constexpr unsigned S = Start;
            constexpr unsigned E = End;
            return (*(reinterpret_cast<const uint8_t *>(&value) + (S / 8u)) >> (S % 8u)) & ((2u << (E - S)) - 1u);
        }

        template<int Start, int End, std::enable_if_t<((End - Start) < 7) && ((Start / 8) == (End / 8)), int> = 0>
        auto apply(const uint8_t &v) {
            constexpr unsigned S = Start;
            constexpr unsigned E = End;
            constexpr uint8_t Mask = ((2u << (E - S)) - 1u) << (S % 8u);
            auto *p = reinterpret_cast<uint8_t*>(&value) + (S / 8u);
            *p = static_cast<uint8_t>(*p & ~Mask) | static_cast<uint8_t>((v << (S % 8u)) & Mask);
            return *this;
        }

        template<int Start, int End, std::enable_if_t<(Start / 8) == (End / 8) - 1u, int> = 0>
        uint16_t get() const {
            constexpr unsigned S = Start;
            constexpr unsigned E = End;
            return (*reinterpret_cast<const uint16_t *>(reinterpret_cast<const uint8_t *>(&value) + (S / 8u))
                    >> (S % 8u)) & ((2u << (E - S)) - 1u);
        }

        template<int Start, int End, std::enable_if_t<(Start / 8) == (End / 8) - 1u, int> = 0>
        auto apply(const uint16_t &v) {
            constexpr unsigned S = Start;
            constexpr unsigned E = End;
            constexpr uint16_t Mask = ((2u << (E - S)) - 1u) << (S % 8u);
            auto *p = reinterpret_cast<uint16_t*>(reinterpret_cast<uint8_t *>(&value) + (S / 8u));
            *p = static_cast<uint16_t>(*p & ~Mask) | static_cast<uint16_t>((v << (S % 8u)) & Mask);
            return *this;
        }

        template<int Start, int End, std::enable_if_t<(Start / 8) == (End / 8) - 2u, int> = 0>
        uint32_t get() const {
            constexpr unsigned S = Start;
            constexpr unsigned E = End;
            return (*reinterpret_cast<const uint32_t *>(reinterpret_cast<const uint8_t *>(&value) + (S / 8u))
                    >> (S % 8u)) & ((2u << (E - S)) - 1u);
        }

        template<int Start, int End, std::enable_if_t<(Start / 8) == (End / 8) - 2u, int> = 0>
        auto apply(const uint32_t &v) {
            constexpr unsigned S = Start;
            constexpr unsigned E = End;
            constexpr uint32_t Mask = ((2u << (E - S)) - 1u) << (S % 8u);
            auto *p = reinterpret_cast<uint32_t*>(reinterpret_cast<uint8_t *>(&value) + (S / 8u));
            *p = static_cast<uint32_t>(*p & ~Mask) | static_cast<uint32_t>((v << (S % 8u)) & Mask);
            return *this;
        }

    };

    template<int Offset, int Start, int End, std::enable_if_t<(Start < End) && (Start >= 0), int> = 0>
    struct Bits {
        static constexpr int StartBit = Start;
        static constexpr int EndBit = End;
        static constexpr int Size = End - Start;

        using BaseType = typename bitfield_type<Size>::type;

        template<typename T>
        static auto value(T &str) {
            return *reinterpret_cast<const BaseType *>(reinterpret_cast<const char *>(&str) + Offset);
        }

        template<typename S>
        struct Interface {
            static constexpr bool Internal = false;
            using Type = BaseType;
        };
    };

    template<typename S, typename F, typename ...Rest>
    struct Bitfield {
        FORMAT_HPP_TYPE(void)

        template<typename St, bool InternalOverwrite = false, bool NewInternal = false >
        struct Interface {
            static constexpr bool Internal = InternalOverwrite ? NewInternal : false;
            using Stack = St;
            using Type = S;
            using ValueType = typename unpack_types<Stack, Rest...>::type;

            void read() {
                SubFile<Stack, Scalar<F>>::reader(processor, stack).read();
            }

            void read(S &output) {
                F v;
                SubFile<Stack, Scalar<F>>::reader(processor, stack).read(v);
                read_into(output, Packed<F>{v});
            }

            void write(const S &input) {
                SubFile<Stack, Scalar<F>>::writer(processor, stack).write(write_from(input));
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        private:
            static void read_into(S &output, const Packed<F> &value) {
                ValueType v;
                process_read<0, sizeof...(Rest) - 1, Rest...>(v, value);
                output = std::make_from_tuple<S>(v);
            }

            static F write_from(const S &input) {
                Packed<F> v;
                process_write<0, sizeof...(Rest) - 1, Rest...>(v, input);
                return v.value;
            }

            template<int I, int M, typename T = void, typename ...Rem, std::enable_if_t<
                    I < M, int> = 0>
            static void process_write(Packed<F> &v, const S &input) {
                (v.template apply<T::StartBit, T::EndBit>(T::value(input)));
                process_write<I + 1, M, Rem...>(v, input);
            }

            template<int I, int M, typename T = void, typename ...Rem, std::enable_if_t<
                    I == M, int> = 0>
            static void process_write(Packed<F> &v, const S &input) {
                (v.template apply<T::StartBit, T::EndBit>(T::value(input)));
            }

            template<int I, int M, typename T1 = void, typename ...Rem, std::enable_if_t<
                    I < M, int> = 0>
            static void process_read(ValueType &v, const Packed<F> &value) {
                std::get<I>(v) = value.template get<T1::StartBit, T1::EndBit>();
                process_read<I + 1, M, Rem...>(v, value);
            }

            template<int I, int M, typename T1 = void, typename ...Rem, std::enable_if_t<
                    I == M, int> = 0>
            static void process_read(ValueType &v, const Packed<F> &value) {
                std::get<I>(v) = value.template get<T1::StartBit, T1::EndBit>();
            }
        };
    };

/*!
 * \brief Read arguments from Rest and construct object of type S.
 * \tparam S Constructor
 * \tparam Rest Argument-Types
 */
    template<typename S, typename ...Rest>
    struct Structure {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., Rest...>;

        template<typename St, bool InternalOverwrite = false, bool NewInternal = false >
        struct Interface {
            static constexpr bool Internal = InternalOverwrite ? NewInternal : false;
            using Stack = St;
            using Type = S;
            using ValueType = typename unpack_types<Stack, Rest...>::type;

            void read() {
                read_into(processor, stack);
            }

            /*!
             * \brief Assign output to newly constructed element.
             * \throws binary_eof()
             */
            void read(S &output) {
                read_into(output, processor, stack);
            }

            static void read_into(read_stream_t &processor, Stack &stack) {
                auto reader = SubFile<St, Rest...>::reader(processor, stack);
                process_read<0, sizeof...(Rest) - 1, decltype(reader)>(std::move(reader));
            }

            static void read_into(S &output, read_stream_t &processor, Stack &stack) {
                auto reader = SubFile<St, Rest...>::reader(processor, stack);
                ValueType v;
                process_read<0, sizeof...(Rest) - 1, decltype(reader), Rest...>(std::move(reader), v);
                output = std::make_from_tuple<S>(v);
            }

            /*!
             * \brief Write variable using callback function.
             *
             * The callback is called with a writer object for its members.
             */
            template<typename F, typename W = typename SubFile<St, Rest...>::Writer, typename std::enable_if_t<std::is_invocable_v<F, W>, int> = 0>
            void write(F &&fun) {
                write_cb(std::forward<F>(fun), processor, stack);
            }

            template<typename F>
            static void write_cb(F &&fun, write_stream_t &processor, Stack &stack) {
                fun(SubFile<St, Rest...>::writer(processor, stack));
            }

            void write(const S &input) {
                write_from(input, processor, stack);
            }

            static void write_from(const S &input, write_stream_t &processor, Stack &stack) {
                auto &&r = SubFile<St, Rest...>::writer(processor, stack);
                process_write<0, sizeof...(Rest) - 1, std::remove_reference_t<decltype(r)>, Rest...>(std::move(r),
                                                                                                     input);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        private:
            template<int I, int M, typename R, std::enable_if_t<I < M, int> = 0>
            static void process_read(R &&r) {
                auto &&p = std::forward<R>(r).read();
                process_read<I + 1, M, decltype(p)>(std::move(p));
            }

            template<int I, int M, typename R, std::enable_if_t<I == M, int> = 0>
            static void process_read(R &&r) {
                r.read();
            }

            template<int I, int M, typename R, typename T = void, typename ...Rem, std::enable_if_t<
                    I < M, int> = 0, std::enable_if_t<!T::template Interface<Stack>::Internal, int> = 0>
            static void process_write(R &&r, const S &input) {
                auto &&p = std::forward<R>(r).write(
                        T::template Interface<Stack>::value(input));
                process_write<I + 1, M, std::remove_reference_t<decltype(p)>, Rem...>(std::move(p), input);
            }

            template<int I, int M, typename R, typename T = void, typename ...Rem, std::enable_if_t<
                    I < M, int> = 0, std::enable_if_t<T::template Interface<Stack>::Internal, int> = 0>
            static void process_write(R &&r, const S &input) {
                auto &&p = std::forward<R>(r).write();
                process_write<I, M - 1, std::remove_reference_t<decltype(p)>, Rem...>(std::move(p), input);
            }

            template<int I, int M, typename R, typename T = void, typename ...Rem, std::enable_if_t<
                    I == M, int> = 0, std::enable_if_t<!T::template Interface<Stack>::Internal, int> = 0>
            static void process_write(R &&r, const S &input) {
                r.write(T::template Interface<Stack>::value(input));
            }

            template<int I, int M, typename R, typename T = void, typename ...Rem, std::enable_if_t<
                    I == M, int> = 0, std::enable_if_t<T::template Interface<Stack>::Internal, int> = 0>
            static void process_write(R &&r, const S &) {
                r.write();
            }

            template<int I, int M, typename R, typename T1 = void, typename ...Rem, std::enable_if_t<
                    I < M, int> = 0, std::enable_if_t<!T1::template Interface<Stack>::Internal, int> = 0>
            static void process_read(R &&r, ValueType &v) {
                auto &&p = std::forward<R>(r).read(std::get<I>(v));
                process_read<I + 1, M, decltype(p), Rem...>(std::move(p), v);
            }

            template<int I, int M, typename R, typename T1 = void, typename ...Rem, std::enable_if_t<
                    I < M, int> = 0, std::enable_if_t<T1::template Interface<Stack>::Internal, int> = 0>
            static void process_read(R &&r, ValueType &v) {
                auto &&p = std::forward<R>(r).read();
                process_read<I, M - 1, decltype(p), Rem...>(std::move(p), v);
            }

            template<int I, int M, typename R, typename T1 = void, typename ...Rem, std::enable_if_t<
                    I == M, int> = 0>
            static void process_read(R &&r, ValueType &v) {
                r.read(std::get<I>(v));
            }

        };
    };

    template<typename St, typename S, typename ...Rest>
    class CustomStructure {
    public:
        using Type = S;
        using Stack = St;
        using Reader = SubFile<St, Rest...>;
        using Writer = SubFile<St, Rest...>;
        using Helper = typename Structure<S, Rest...>::template Interface<Stack>;
        static constexpr bool Internal = false;

        explicit CustomStructure(stream_type_t<Stack> &p, Stack &s) : processor(p), stack(s) {}

        virtual ~CustomStructure() = default;

    protected:
        auto make_reader() { return Reader::reader(processor, stack); }

        auto make_writer() { return Writer::writer(processor, stack); }

        stream_type_t<Stack> &processor;
        Stack &stack;
    };

/*!
 * \brief Type with byte offset in parent type.
 * \tparam I byte offset
 * \tparam T type
 */
    template<int I, typename T>
    struct Offset {
        FORMAT_HPP_TYPE(T)

        FORMAT_HPP_INTERFACE_DECL {
            static constexpr bool Internal = InternalOverwrite ? NewInternal : false;
            using Type = typename T::template Interface<S>::Type;
            using Stack = typename T::template Interface<S>::Stack;

            template<typename St>
            static const auto &value(const St &st) {
                return *reinterpret_cast<const Type *>(reinterpret_cast<const uint8_t *>(&st) + I);
            }

            template<typename ...FArgs>
            auto read(FArgs &&...args) {
                typename T::template Interface<S>{processor, stack}.read(std::forward<FArgs>(args)...);
            }

            template<typename ...FArgs>
            auto write(FArgs &&...args) {
                typename T::template Interface<S>{processor, stack}.write(std::forward<FArgs>(args)...);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

    template<auto Acc, typename T>
    struct Accessor {
        FORMAT_HPP_TYPE(T)

        FORMAT_HPP_INTERFACE_DECL {
            static constexpr bool Internal = InternalOverwrite ? NewInternal : false;
            using Type = typename T::template Interface<S>::Type;
            using Stack = typename T::template Interface<S>::Stack;

            template<typename St>
            static const auto &value(const St &st) {
                return (st.*Acc)();
            }

            template<typename ...FArgs>
            auto read(FArgs &&...args) {
                typename T::template Interface<S>{processor, stack}.read(std::forward<FArgs>(args)...);
            }

            template<typename ...FArgs>
            auto write(FArgs &&...args) {
                typename T::template Interface<S>{processor, stack}.write(std::forward<FArgs>(args)...);
            }

            stream_type_t<Stack> &processor;
            Stack &stack;
        };
    };

/*!
 * \brief When to terminate a TerminatedArray.
 */
    enum ArrayTerminationModeEnum {
        VALID, /*!< add element. keep reading. */
        BREAK, /*!< stop reading. */
        BREAKAFTER, /*!< add element. stop reading. */
        SKIP /*!< ignore element. keep reading */
    };

/*!
 * \brief Container for ArrayTerminationModeEnum.
 * \note This class (and Bool) is necessary, because TerminatedArray::read() are ambiguous if the callback return
 * type can be cast to bool.
 */
    struct ArrayTerminationMode {
        ArrayTerminationMode(bool b) = delete;

        ArrayTerminationMode(ArrayTerminationModeEnum mode) : m(mode) {}

        ArrayTerminationMode &operator=(bool b) = delete;

        ArrayTerminationMode &operator=(ArrayTerminationModeEnum mode) {
            m = mode;
            return *this;
        }

        operator ArrayTerminationModeEnum() { return m; }

        ArrayTerminationModeEnum m;
    };

/*!
 * \brief Container for a boolean.
 * \note This class (and ArrayTerminationNode) is necessary, because TerminatedArray::read() are ambiguous if the callback return
 * type can be cast to bool.
 */
    struct Bool {
        Bool(bool bo) : b(bo) {}

        operator bool() { return b; }

        bool b;
    };

/*!
 * \brief Read variable-length array.
 * \tparam IntegralAbort if T is integral, array is terminated by this element
 */
    template<typename T, int IntegralAbort = 0>
    struct TerminatedArray {
        FORMAT_HPP_TYPE(T)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::vector<typename T::template Interface<S>::Type>;

            void read() {
                typename T::template Interface<S>::Type abort{IntegralAbort};
                do {
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor).read(v);
                    if (v == abort) break;
                } while (true);
            }

            /*!
             * \brief Read until element matches null element.
             */
            template<typename T1=T, std::enable_if_t<std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void read(Type &vec) {
                typename T::template Interface<S>::Type abort{IntegralAbort};
                do {
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    if (v == abort) break;
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read until element matches default-constructed element.
             */
            template<typename T1=T, std::enable_if_t<!std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void read(Type &vec) {
                typename T::template Interface<S>::Type abort{};
                do {
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor).read(v);
                    if (v == abort) break;
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Write array, terminate with IntegralAbort.
             */
            template<typename T1=T, std::enable_if_t<std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void write(const Type &in) {
                for (const auto &i : in) {
                    SubFile<S, T>::writer(processor, stack).write(i);
                }
                SubFile<S, T>::writer(processor, stack).write(
                        static_cast<typename T::template Interface<S>::Type>(IntegralAbort));
            }

            /*!
             * \brief Write array, terminate with default-constructed element.
             */
            template<typename T1=T, std::enable_if_t<!std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void write(const Type &in) {
                for (const auto &i : in) {
                    SubFile<S, T>::writer(processor, stack).write(i);
                }
                SubFile<S, T>::writer(processor, stack).write(typename T::Type{});
            }

            /*!
             * \brief Read until element matches abort.
             */
            void read(Type &vec, typename T::template Interface<Stack>::Type abort) {
                do {
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    if (v == abort) break;
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read until callback function returns false for an element.
             */
            template<typename F, typename std::enable_if_t<std::is_invocable_r_v<Bool, F, const typename T::template Interface<Stack>::Type &>, int> = 0>
            void read(Type &vec, F &&fun) {
                do {
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    if (!fun(v)) break;
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read elements and adjust behaviour based on the returned ArrayTerminationMode.
             * \sa ArrayTerminationMode.
             */
            template<typename F, typename std::enable_if_t<std::is_invocable_r_v<ArrayTerminationMode, F, const typename T::template Interface<Stack>::Type &>, int> = 0>
            void read(Type &vec, F &&fun) {
                ArrayTerminationModeEnum mode = VALID;
                do {
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    mode = fun(v);
                    if (mode == BREAK) break;
                    else if (mode == SKIP) continue;
                    vec.push_back(std::move(v));
                } while (mode != BREAKAFTER);
            }

        };
    };

/*!
 * \brief Read prefixed elements until a prefix matches IntegralAbort (if integral) or a default-constructed prefix.
 * \tparam IntegralAbort If Prefix is integral, terminate array with this.
 * \tparam IntegralGood If Prefix is integral, set good prefixes to this.
 */
    template<typename T, typename Prefix, int IntegralAbort = 0, int IntegralGood = 1>
    struct PrefixedTerminatedArray {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., Prefix, T>;

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::vector<typename T::template Interface<S>::Type>;

            /*!
             * \brief Read elements and their prefixes until prefix matches null element.
             */
            template<typename T1=Prefix, std::enable_if_t<std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void read(Type &vec) {
                typename T1::template Interface<S>::Type abort{IntegralAbort};
                do {
                    typename T1::template Interface<S>::Type prefix;
                    SubFile<S, T1>::reader(processor, stack).read(prefix);
                    if (prefix == abort) break;
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read until element matches default-constructed element.
             */
            template<typename T1=Prefix, std::enable_if_t<!std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void read(Type &vec) {
                typename T1::template Interface<S>::Type abort{};
                do {
                    typename T1::template Interface<S>::Type prefix;
                    SubFile<S, T1>::reader(processor, stack).read(prefix);
                    if (prefix == abort) break;
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read until prefix matches abort.
             */
            void read(Type &vec, typename Prefix::template Interface<Stack>::Type abort) {
                do {
                    typename Prefix::template Interface<Stack>::Type prefix;
                    SubFile<S, Prefix>::reader(processor).read(prefix);
                    if (prefix == abort) break;
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor).read(v);
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read until callback function returns false for a prefix.
             */
            template<typename F, typename std::enable_if_t<std::is_invocable_r_v<Bool, F, const typename T::template Interface<Stack>::Type &>, int> = 0>
            void read(Type &vec, F &&fun) {
                do {
                    typename Prefix::template Interface<Stack>::Type prefix;
                    SubFile<S, Prefix>::reader(processor, stack).read(prefix);
                    if (!fun(prefix)) break;
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    vec.push_back(std::move(v));
                } while (true);
            }

            /*!
             * \brief Read elements and adjust behaviour based on the returned ArrayTerminationMode.
             * \sa ArrayTerminationMode.
             */
            template<typename F, typename std::enable_if_t<std::is_invocable_r_v<ArrayTerminationMode, F, const typename T::template Interface<Stack>::Type &>, int> = 0>
            void read(Type &vec, F &&fun) {
                ArrayTerminationModeEnum mode = VALID;
                do {
                    typename Prefix::template Interface<S>::Type prefix;
                    SubFile<S, T>::reader(processor, stack).read(prefix);
                    mode = fun(prefix);
                    if (mode == BREAK) break;
                    else if (mode == SKIP) continue;
                    typename T::template Interface<S>::Type v;
                    SubFile<S, T>::reader(processor, stack).read(v);
                    vec.push_back(std::move(v));
                } while (mode != BREAKAFTER);
            }

            template<typename T1=Prefix, typename std::enable_if_t<std::is_integral_v<typename T1::template Interface<Stack>::Type>, int> = 0>
            void write(const Type &in) {
                for (const auto &i : in) {
                    typename Prefix::template Interface<S>::Type prefix{IntegralGood};
                    SubFile<S, Prefix, T>::writer(processor, stack).write(prefix).write(i);
                }
                SubFile<S, Prefix>::writer(processor, stack).write(
                        static_cast<typename Prefix::template Interface<S>::Type>(IntegralAbort));
            }

            template<typename T1=Prefix, typename std::enable_if_t<!std::is_integral_v<typename T1::type>, int> = 0>
            void write(const Type &in) {
                for (const auto &i : in) {
                    typename Prefix::template Interface<S>::Type prefix{i};
                    SubFile<S, Prefix, T>::writer(processor, stack).write(prefix).write(i);
                }
                SubFile<S, Prefix>::writer(processor, stack).write(typename Prefix::template Interface<S>::Type{});
            }

        };
    };

    enum StringTerminationMode {
        Fixed = 0,
        BreakSet
    };

/*!
 * \brief Read array of fixed size (at compile time) from input.
 * \tparam Len length must be greater than zero.
 */
    template<typename T, int Len, typename std::enable_if_t<std::greater<>()(Len, 0), int> = 0>
    struct StaticArray {
        FORMAT_HPP_TYPE(T)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::array<typename T::template Interface<S>::Type, Len>;

            void read(Type &vec) {
                for (size_t i = 0u; i < static_cast<size_t>(Len); ++i) {
                    SubFile<S, T>::reader(processor, stack).read(vec[i]);
                }
            }

            void read(typename T::template Interface<Stack>::Type *output) {
                for (size_t i = 0u; i < static_cast<size_t>(Len); ++i) {
                    SubFile<S, T>::reader(processor, stack).read(*(output + i));
                }
            }

            void write(const Type &vec) {
                for (const auto &i : vec) {
                    SubFile<S, T>::writer(processor, stack).write(i);
                }
            }

        };
    };

/*!
 * \brief Read array of fixed size (at runtime) from input.
 *
 * Use StaticArray for arrays whose size is known at compile-time.
 * \tparam T element type of array
 */
    template<typename T, typename LengthVariable>
    struct FixedArray {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., LengthVariable, T>;

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::vector<typename T::template Interface<S>::Type>;

            void read(Type &vec) {
                auto l = stack.template get<LengthVariable::ID>().val;
                if (l >= 0) read(vec, static_cast<size_t>(l));
                else throw binary_read_error();
            }

            void read(Type &vec, size_t len) {
                if (len == 0) return;
                vec.resize(len);
                for (size_t i = 0u; i < len; ++i) {
                    SubFile<S, T>::reader(processor, stack).read(vec[i]);
                }
            }

            /*!
             * \brief Write from std::vector<T>.
             * \warning length is not serialized, has to be done manually!
             */
            void write(const Type &vec) {
                for (const auto &i : vec) {
                    SubFile<S, T>::writer(processor, stack).write(i);
                }
            }

        };
    };

    template<typename T>
    struct FixedArray<T, void> {
        FORMAT_HPP_TYPE(T)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::vector<typename T::template Interface<S>::Type>;

            /*!
             * \brief Read into std::vector<T>.
             */
            void read(Type &vec, size_t len) {
                if (len == 0) return;
                vec.resize(len);
                for (size_t i = 0u; i < len; ++i) {
                    SubFile<S, T>::reader(processor, stack).read(vec[i]);
                }
            }

            /*!
             * \brief Write from std::vector<T>.
             * \warning length is not serialized, has to be done manually!
             */
            void write(const Type &vec) {
                for (const auto &i : vec) {
                    SubFile<S, T>::writer(processor, stack).write(i);
                }
            }

        };
    };

/*!
 * \brief Read array. Length of array is read first.
 * \tparam Prefix array length
 * \tparam T array element type
 */
    template<typename Prefix, typename T>
    struct PrefixedArray {
        template<template<typename, typename...> typename P, typename OP, typename ...A> using Process = default_process<P, OP, A..., Prefix, T>;

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::vector<typename T::template Interface<S>::Type>;

            /*!
             * \brief Read into std::vector
             */
            template<typename V>
            void read(V &&vec) {
                typename Prefix::template Interface<S>::Type prefix;
                SubFile<S, Prefix, FixedArray<T, void>>::reader(processor, stack).read(prefix).read(
                        std::forward<V>(vec),
                        (static_cast<size_t>(prefix) < 1u
                         ? 0u
                         : static_cast<size_t>(prefix)));
            }

            template<typename PrefixType=typename Prefix::template Interface<S>::Type, std::enable_if_t<
                    is_promoted_cast_v<decltype(std::declval<Type>().size()), PrefixType>, int> = 0>

            void write(const Type &v) {
                PrefixType prefix{v.size()};
                SubFile<S, Prefix, FixedArray<T, void>>::writer(processor, stack).write(prefix).write(v);
            }

            template<typename PrefixType=typename Prefix::template Interface<S>::Type, std::enable_if_t<
                    !is_promoted_cast_v<decltype(std::declval<Type>().size()), PrefixType> &&
                    std::is_arithmetic_v<PrefixType>, int> = 0>

            void write(const Type &v) {
                using Limit = std::numeric_limits<PrefixType>;
                auto s = v.size();
                if (s < Limit::min() || s > Limit::max()) {
                    throw std::range_error(
                            std::string("Vector Size cannot be represented by type ") + typeid(PrefixType).name());
                }
                PrefixType prefix{static_cast<PrefixType>(s)};
                SubFile<S, Prefix, FixedArray<T, void>>::writer(processor, stack).write(prefix).write(v);
            }

            template<typename PrefixType=typename Prefix::template Interface<S>::Type, std::enable_if_t<
                    !is_promoted_cast_v<decltype(std::declval<Type>().size()), PrefixType> &&
                    !std::is_arithmetic_v<PrefixType>, int> = 0>

            void write(const Type &v) {
                PrefixType prefix{v.size()};
                SubFile<S, Prefix, FixedArray<T, void>>::writer(processor, stack).write(prefix).write(v);
            }

        };
    };

/* String Specializations */

/*!
 * \brief Specialization for static strings
 */
    template<int Len, typename std::enable_if_t<std::greater<>()(Len, 0), int> I>
    struct StaticArray<char, Len, I> {
        FORMAT_HPP_TYPE(void)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::string;

            void read(Type &out) {
                out.resize(Len);
                processor.read(out.data(), static_cast<size_t>(Len));
                if (processor.eof()) {
                    throw binary_eof();
                }
            }

            void write(const Type &in) {
                processor.write(in.data(), static_cast<size_t>(Len));
                if (processor.bad()) {
                    throw binary_write_error();
                }
            }

        };
    };

/*!
 * \brief Specialization for fixed strings.
 */
    template<>
    struct FixedArray<char, void> {
        FORMAT_HPP_TYPE(void)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::string;

            template<typename L>
            void read(Type &out, L len) {
                if (len <= 0) {
                    out.clear();
                    return;
                }
                out.resize(static_cast<size_t>(len));
                processor.read(out.data(), static_cast<std::streamsize>(len));
                if (processor.eof()) {
                    throw binary_eof();
                }
            }

            void write(const Type &in) {
                processor.write(in.data(), static_cast<std::streamsize>(in.size()));
                if (processor.bad()) {
                    throw binary_write_error();
                }
            }

        };
    };

/*!
 * \brief Specialization for strings whose length is stored before the string data.
 */
    template<typename Prefix>
    struct PrefixedArray<Prefix, char> {
        FORMAT_HPP_TYPE(Prefix)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::string;

            void read(Type &out) {
                typename Prefix::template Interface<S>::Type prefix;
                SubFile<S, Prefix, FixedArray<char, void>>::reader(processor, stack).read(prefix).read(out, prefix);
            }

            template<typename PrefixType=typename Prefix::template Interface<S>::Type, std::enable_if_t<
                    is_promoted_cast_v<decltype(std::declval<Type>().size()), PrefixType>, int> = 0>

            void write(const Type &v) {
                PrefixType prefix{v.size()};
                SubFile<S, Prefix, FixedArray<char, void>>::writer(processor, stack).write(prefix).write(v);
            }

            template<typename PrefixType=typename Prefix::template Interface<S>::Type, std::enable_if_t<
                    !is_promoted_cast_v<decltype(std::declval<Type>().size()), PrefixType> &&
                    std::is_arithmetic_v<PrefixType>, int> = 0>

            void write(const Type &v) {
                using Limit = std::numeric_limits<PrefixType>;
                auto s = v.size();
                if (s < Limit::min() || s > Limit::max()) {
                    throw std::range_error(
                            std::string("Vector Size cannot be represented by type ") + typeid(PrefixType).name());
                }
                PrefixType prefix{static_cast<PrefixType>(s)};
                SubFile<S, Prefix, FixedArray<char, void>>::writer(processor, stack).write(prefix).write(v);
            }

            template<typename PrefixType=typename Prefix::template Interface<S>::Type, std::enable_if_t<
                    !is_promoted_cast_v<decltype(std::declval<Type>().size()), PrefixType> &&
                    !std::is_arithmetic_v<PrefixType>, int> = 0>

            void write(const Type &v) {
                PrefixType prefix{v.size()};
                SubFile<S, Prefix, FixedArray<char, void>>::writer(processor, stack).write(prefix).write(v);
            }

        };
    };

/*!
 * \brief Read string until delimiter.
 */
    template<>
    struct TerminatedArray<char> {
        FORMAT_HPP_TYPE(void)

        FORMAT_HPP_INTERFACE_DECL {
            FORMAT_HPP_INTERFACE
            using Type = std::string;

            /*!
             * \brief Read string until del or EOF.
             * \param string output
             * \param del Delimiter character
             * \param breakOnEndOfFile if false and EOF is encountered, a binary_eof() is thrown
             * \throws binary_eof()
             */
            void read(Type &string, char del = '\0', bool breakOnEndOfFile = true) {
                string.clear();
                do {
                    char c;
                    processor.read(&c, 1);
                    if (processor.eof()) {
                        if (!breakOnEndOfFile) throw binary_eof();
                        break;
                    }
                    if (c == del) break;
                    string.push_back(c);
                } while (true);
            }

            /*!
             * \brief Read string until one of the exit conditions is satisfied.
             *
             * If mode is `Fixed` then an exit condition is finding `del` in the input.
             * If mode is `BreakSet` then exit conditions are finding any of the characters in `del` in the input.
             * If breakOnEndOfFile is false, throw binary_eof on EOF. Return normally otherwise.
             * \param string output
             * \param del Delimiter string
             * \param mode StringTerminationMode
             * \param breakOnEndOfFile might throw binary_eof on false.
             * \throws binary_eof()
             */
            void
            read(Type &string, std::string del, StringTerminationMode mode = Fixed, bool breakOnEndOfFile = true) {
                string.clear();
                do {
                    char c;
                    processor.read(&c, 1);
                    if (processor.eof()) {
                        if (!breakOnEndOfFile) throw binary_eof();
                        break;
                    }
                    if (mode == BreakSet && del.find(c) != std::string::npos) break;
                    if (mode == Fixed
                        && string.length() >= del.length()
                        && strcmp(string.data() + string.length() - del.length(), del.data()) == 0) {
                        string.resize(string.length() - del.length());
                        break;
                    }
                    string.push_back(c);
                } while (true);
            }

        };
    };

/* Shortcuts */

    template<typename Prefix> using PrefixedString = PrefixedArray<Prefix, char>;
    template<int Len> using StaticString = StaticArray<char, Len>;
    template<typename L = void> using FixedString = FixedArray<char, L>;
    using TerminatedString = TerminatedArray<char>;

    template<typename ...A> using Sc = Scalar<A...>;
    template<typename ...A> using St = Structure<A...>;
    template<typename ...A> using Fa = FixedArray<A...>;
    template<typename T, int I> using Sa = StaticArray<T, I>;
    template<typename ...A> using Pa = PrefixedArray<A...>;
    template<typename ...A> using Ta = TerminatedArray<A...>;
    template<typename T, typename P, int A, int G> using Pt = PrefixedTerminatedArray<T, P, A, G>;
    using Ts = TerminatedString;
    template<typename Cmp, typename T, typename F> using Cd = Conditional<Cmp, T, F>;

    template<typename Prefix> using Ps = PrefixedString<Prefix>;
    template<int Len> using Ss = StaticString<Len>;
    template<typename L = void> using Fs = FixedString<L>;

    template<int I, typename T> using O = Offset<I, T>;
    template<auto A, typename T> using Acc = Accessor<A, T>;

}

#endif //BINARYFILE_HPP_FORMAT_HPP
