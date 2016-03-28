-module(zset_SUITE).

-export([all/0,
         groups/0, init_per_suite/1,
         end_per_suite/1]).

-export([test_create/1,
         test_enter/1,
         test_delete/1,
         test_find/1,
         test_fold/1,
         test_to_list/1,
         test_from_list/1,
         test_range/1,
         test_top/1,
         test_page/1]).

all() ->
    [test_create,
     test_enter,
     test_delete,
     test_find,
     test_fold,
     test_to_list,
     test_from_list,
     test_range,
     test_top,
     test_page].

groups() ->
    [].

init_per_suite(Config) ->
    Fun = fun(I, {ZS, L}) -> {zset:enter(I, I * 10, I * 100, ZS),
                              [{I, I * 10, I * 100} | L]} end,
    {ZSet, List} = lists:foldl(Fun, {zset:new(), []}, lists:seq(1, 10)),
    [{test_zset, ZSet},
     {test_list, lists:sort(List)} |Config].

end_per_suite(Config) ->
    lists:keydelete(test_zset, 1, Config).

test_create(_Config) ->
    ZSet = zset:new(),
    0 = zset:size(ZSet),
    ok.

test_enter(_Config) ->
    ZSet = zset:new(),
    ZSet1 = zset:enter(1, 10, <<"value1">>, ZSet),
    1 = zset:size(ZSet1),
    ZSet2 = zset:enter(1, 10, <<"value2">>, ZSet1),
    1 = zset:size(ZSet2),
    false = ZSet2 == ZSet1,
    ZSet3 = zset:enter(1, 20, <<"value2">>, ZSet2),
    1 = zset:size(ZSet3),
    false = ZSet3 == ZSet2,
    ok.

test_delete(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    ZSet1 = zset:delete(1, ZSet),
    9 = zset:size(ZSet1),
    ZSet2 = zset:delete(unknown, ZSet1),
    9 = zset:size(ZSet2),
    ok.

test_find(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    {1, 10, 100} = zset:find(1, ZSet),
    error = zset:find(unknown, ZSet),
    ok.

test_fold(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    TestList = proplists:get_value(test_list, Config),
    FoldFun = fun(M, S, V, Acc) -> [{M, S, V} | Acc] end,
    TestList = lists:sort(zset:fold(FoldFun, [], ZSet)),
    ok.

test_to_list(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    TestList = proplists:get_value(test_list, Config),
    TestList = lists:sort(zset:to_list(ZSet)),
    ok.

test_from_list(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    TestList = proplists:get_value(test_list, Config),
    ZSet = zset:from_list(TestList),
    ok.

test_range(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    TestList = proplists:get_value(test_list, Config),
    [{1, 10, 100}, {2, 20, 200}, {3, 30, 300}] = zset:range(10, 30, ZSet),
    TestList = zset:range(10, 1000, ZSet),
    ok.

test_top(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    TestList = proplists:get_value(test_list, Config),
    [{1, 10, 100}, {2, 20, 200}, {3, 30, 300}] = zset:top(3, ZSet),
    TestList = zset:top(100, ZSet),
    [{2, 20, 200}, {3, 30, 300}] = zset:top(20, 2, ZSet),
    ok.

test_page(Config) ->
    ZSet = proplists:get_value(test_zset, Config),
    [{1, 10, 100}, {2, 20, 200}, {3, 30, 300}] = zset:page(1, 3, ZSet),
    [] = zset:page(unknown, 3, ZSet),
    ok.
