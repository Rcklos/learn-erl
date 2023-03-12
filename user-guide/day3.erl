-module(day3).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


role_to_list() -> [ #role{ id = X } || X <- lists:seq(1, 10) ].

% 两种实现方式(本质只有一种)
% 第一种是列表推导
delete_role_by_id(List, Id) -> [ X || X <- List, X#role.id /= Id].
% 第二种是过滤，但实际上，过滤内部实现的方式还是列表推导
delete_role_by_filter(List, Id) -> lists:filter(fun(E) -> E#role.id /= Id end, List).

% 两种实现方式: 关卡和case..of
% 关卡实现
search_role_by_id([], _) -> false;
search_role_by_id([H | _], Id) when H#role.id == Id -> H;
search_role_by_id([_ | L], Id) -> search_role_by_id(L, Id).

% 第二种实现方法
search_role_by_id_by_lib(List, Id) -> 
    % 实际上lists:search内部是由case..of实现
    lists:search(fun(E) -> E#role.id == Id end, List).

%% ``

% 根据id修改Score
set_score_by_id(List, Id, Score) -> 
    lists:keyfind(Arg1, Arg2, Arg3)
    lists:foldr(fun(E, Acc) -> 
                    case E#role.id == Id of
                        true -> [E#role{score = Score} | Acc];
                        false -> [E | Acc]
                    end
                end, [], List).
    % [X || X <- List, X#role.id /= Id] 
    % ++ [X#role{score = Score} || X <- List, X#role.id == Id].


% 根据键值排序role的列表
sort_byKV_and_print_roles(Nth, List) -> 
    F = fun(E1, E2) -> 
            element(Nth, E1) < element(Nth, E2)
        end,
    lists:sort(F, List).

% 映射的操作练习
map_test() -> 
    M = #{
          id => 1
          ,name => "Rcklos"
          ,sex => man
          ,lev => 0
          ,age => 0
          ,score => 0
     },
    % 加入score信息
    M1 = M#{score => 0},
    % 验证是否加入成功
    true = maps:is_key(score, M1),

    % 删除lev信息
    M2 = maps:remove(lev, M1),
    % 验证是否删除成功
    false = maps:is_key(lev, M2),

    % 输出M2
    % io:format("~p~n", [M2]),
    % 查找score信息(根据上文，M2中score = 0)
    % 当出现Failed时下面这行代码只会输出badmatch以及对应的函数、行号
    % 0 = maps:get(score, M2),
    % Failed时这里输出不仅会包含函数名和行号，还会输出断言的内容
    ?assert(maps:get(score, M2) =:= 0),

    % 修改score信息
    M3 = M2#{score:= 5},
    % 验证是否修改成功
    5 = maps:get(score, M3).

% todo: 学习dict库
delete(TupleList, DelKey) -> 
    T = [ X || X <- TupleList, element(1, X) /= DelKey],
    Fun = fun({K,V}, D) -> dict:append(K, V, D) end,
    dict:to_list(lists:foldr(Fun, dict:new(), T)).

delete_test() -> 
    [{3, [2, 4]}] = delete([{1, 2}, {3, 4}, {3, 2}], 1).

recurrence_geti(A) -> recurrence_geti_(A, []).
recurrence_geti_([], I) -> lists:reverse(I);
recurrence_geti_([H | T], I) ->
    case is_integer(H) andalso H > 0 of
        true -> recurrence_geti_(T, [H | I]);
        false -> recurrence_geti_(T, I)
    end.

% 取出第一个正整数
recurrence_fi([]) -> false;
recurrence_fi([H | _]) when is_integer(H), H > 0 -> H;
recurrence_fi([_ | T]) -> recurrence_fi(T).

% 累加值
recurrence_acc_([], R) -> R;
recurrence_acc_([H | T], R) ->
    recurrence_acc_(T, H + R).
recurrence_acc(A) -> recurrence_acc_(A, 0).

recurrence(A) ->
    % 取出所有正整数
    [1, 3, 10, 29] = recurrence_geti(A),
    % 取出第一个正整数
    1 = recurrence_fi(A),
    % 计算所有正整数的累加值
    43 = recurrence_acc(recurrence_geti(A)).

reduction_geti(A) ->
    [X || X <- A, is_integer(X), X > 0].

reduction_fi([]) -> false;
reduction_fi(A) ->
    [H | _] = reduction_geti(A),
    H.

reduction_acc(A) ->
    recurrence_acc(reduction_geti(A)).

reduction(A) ->
    % 取出所有正整数
    [1, 3, 10, 29] = reduction_geti(A),
    % 取出第一个正整数
    1 = reduction_fi(A).

stdlib(A) -> 
    Ai = lists:filter(fun(X) -> is_integer(X) andalso X > 0 end, A),
    % 断言取出所有正整数
    [1, 3, 10, 29] = Ai,
    % 取出第一个正整数
    H = lists:nth(1, Ai),
    % 断言
    1 = H,
    % 断言计算所有正整数的累计值
    43 = lists:sum(Ai).

ergodic_test() ->
    A = [1, -2, 3, 0.4, 1.4, -9.9, 10, 29],
    % 纯递归
    recurrence(A),
    % 列表推导
    reduction(A),
    % 纯lists库
    stdlib(A).



%% @doc 生成杨辉三角
-spec generate(NumRows :: integer()) -> [[integer()]].


% 生成NumRows行的杨辉三角
generate(NumRows) when is_integer(NumRows) andalso NumRows > 0 ->
    do_generate(1, NumRows, [[1]]);
generate(_) -> 
    [].

%% 生成杨辉三角函数
do_generate(NumRows, NumRows, Result) ->
    lists:reverse(Result);
do_generate(Index, NumRows, Result = [PreList | _]) when Index < NumRows ->
    TempList = do_generate_(PreList),%% 补全代码，生成函数
    NowRowList = [1] ++ TempList ++ [1],
    lists:partition(fun(_) -> true end, []),
    do_generate(Index + 1, NumRows, [NowRowList | Result]).

%% 生成函数
do_generate_(List) -> do_generate_(List, []).
do_generate_([], Res) -> lists:reverse(Res);
do_generate_([Pre | Tail], Res) -> 
    if 
        [] =:= Tail -> do_generate_(Tail, Res);
        true -> 
                [Next | _ ] = Tail,
                do_generate_(Tail, [ Pre + Next | Res ])
    end.
