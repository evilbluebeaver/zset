-module(zset).

-record(zset, {map :: #{},
               tree :: gb_trees:tree()}).

-export_type([zset/2, result/2, iterator/2, next/2]).

-type result(K, V) :: {K, integer(), V}.

-opaque zset(K, V) :: #zset{map::#{K => integer()},
                            tree::gb_trees:tree({integer(), K}, V)}.

-opaque iterator(K, V) :: gb_trees:iter({integer(), K}, V).

-opaque next(K, V) :: {{integer(), K}, V, iterator(K, V)}.

-export([new/0, enter/4, find/2, delete/2]).
-export([to_list/1, from_list/1]).
-export([size/1, fold/3, range/3, page/3, top/2, top/3]).

%% @doc Create a new empty zset.
-spec new() -> #zset{}.
new() ->
    #zset{map = #{}, tree = gb_trees:empty()}.

%% @doc Enter the `Member' with the `Value', rated by `Score' into the `ZSet'.
-spec enter(Member :: K, integer(), Value :: V, zset(K, V)) -> zset(K, V).
enter(Member, Score, Value, ZSet = #zset{map=Map,
                                         tree=Tree}) ->
    case maps:find(Member, Map) of
        error ->
            Map1 = maps:put(Member, Score, Map),
            Tree1 = gb_trees:enter({Score, {v, Member}}, Value, Tree),
            ZSet#zset{map=Map1, tree =Tree1};
        {ok, PrevScore} ->
            Map1 = maps:put(Member, Score, Map),
            Tree1 = gb_trees:delete({PrevScore, {v, Member}}, Tree),
            Tree2 = gb_trees:enter({Score, {v, Member}}, Value, Tree1),
            ZSet#zset{map=Map1, tree=Tree2}
    end.

%% @doc Delete the `Member' from the `ZSet'.
-spec delete(Member :: K, zset(K, term())) -> zset(K, term()).
delete(Member, ZSet=#zset{map=Map, tree=Tree}) ->
    case maps:find(Member, Map) of
        error ->
            ZSet;
        {ok, Score} ->
            Map1 = maps:remove(Member, Map),
            Tree1 = gb_trees:delete_any({Score, {v, Member}}, Tree),
            ZSet#zset{map=Map1, tree=Tree1}
    end.

%% @doc Get the size of the `ZSet'.
-spec size(zset(term(), term())) -> non_neg_integer().
size(ZSet) ->
    maps:size(ZSet#zset.map).

%% @doc Find a `Member' in the `ZSet'.
-spec find(Member :: K, zset(K, V)) -> error | result(K, V).
find(Member, ZSet = #zset{map=Map}) ->
    case maps:find(Member, Map) of
        error ->
            error;
        {ok, Score} ->
            Value = gb_trees:get({Score, {v, Member}}, ZSet#zset.tree),
            {Member, Score, Value}
    end.

%% @doc Fold the `ZSet' using the `Fun' starts with the `Initial'.
-spec fold(fun((K, V, term(), term()) ->
               term()), term(), zset(K, V)) -> term().
fold(Fun, Initial, ZSet) ->
    I = iterator(ZSet),
    do_fold(Fun, next(I), Initial).

%% @doc Get a list representation of the `ZSet'.
-spec to_list(zset(K, V)) -> [result(K, V)].
to_list(ZSet) ->
    Fun = fun({{Score, {v, Member}}, Value}) ->
            {Member, Score, Value}
    end,
    L = gb_trees:to_list(ZSet#zset.tree),
    lists:map(Fun, L).

%% @doc Convert the `List' into a zset.
-spec from_list([result(K, V)]) -> zset(K, V).
from_list(List) ->
    Fun = fun({K, S, V}, Acc) ->
                  enter(K, S, V, Acc)
          end,
    lists:foldl(Fun, new(), List).

%% @doc Get the range from the `ZSet' between `Start' and `Stop' scores.
-spec range(integer(), integer(), zset(K, V)) -> [result(K, V)].
range(Start, Stop, ZSet) ->
    I = iterator({Start, undefined}, ZSet),
    lists:reverse(do_range(Stop, next(I), [])).

%% @doc Get top `Count' values from the `ZSet' starting from the lowest score.
-spec top(non_neg_integer(), zset(K, V)) -> [result(K, V)].
top(Count, ZSet) ->
    I = iterator(ZSet),
    lists:reverse(do_top(Count, next(I), [])).

%% @doc Get top `Count' values from the `ZSet' starting from the `Score'.
-spec top(integer(), non_neg_integer(), zset(K, term())) -> [result(K, term())].
top(Score, Count, ZSet) ->
    I = iterator({Score, undefined}, ZSet),
    lists:reverse(do_top(Count, next(I), [])).

%% @doc Get `Count' values from the `ZSet' starting from the `Member'.
-spec page(K, non_neg_integer(), zset(K, V)) -> [result(K, V)].
page(Member, Count, ZSet=#zset{map=Map}) ->
    case maps:find(Member, Map) of
        error ->
            [];
        {ok, Score} ->
            I = iterator({Score, {v, Member}}, ZSet),
            lists:reverse(do_top(Count, next(I), []))
    end.

do_fold(_, none, Acc) ->
    Acc;

do_fold(Fun, {{Score, {v, Member}}, Value, Iterator}, Acc) ->
    Acc1 = Fun(Member, Score, Value, Acc),
    do_fold(Fun, next(Iterator), Acc1).

do_top(0, _, Acc) ->
    Acc;
do_top(_Count, none, Acc) ->
    Acc;
do_top(Count, {{Score, {v, Member}}, Value, Iterator}, Acc) ->
    do_top(Count - 1, next(Iterator), [{Member, Score, Value}| Acc]).

do_range(_Stop, none, Acc) ->
    Acc;
do_range(Stop, {{Score, {v, Member}}, Value, Iter}, Acc) when Score =< Stop ->
    do_range(Stop, next(Iter), [{Member, Score, Value} | Acc]);
do_range(_, _, Acc) ->
    Acc.

-spec iterator(zset(K, V)) -> iterator(K, V).
iterator(ZSet) ->
    gb_trees:iterator(ZSet#zset.tree).

-spec iterator({integer(), K}, zset(K, V)) -> iterator(K, V).
iterator(Start, #zset{tree=T}) ->
    gb_trees:iterator_from(Start, T).

-spec next(gb_trees:iter()) ->
    none | {{integer(), any()}, any(), gb_trees:iter()}.
next(Iter1) ->
    gb_trees:next(Iter1).

