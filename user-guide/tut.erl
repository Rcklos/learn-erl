-module(tut).
% -export([double/1, fun1/1, fun2/1]).
-compile(export_all).

double(X) ->
    2 * X.

fun1(1) -> true;
fun1(_) -> false.


fun2(X) when is_integer(X), (X div 2 == 1) -> true;
fun2(_) -> false.

area(triangle, {L, H}) -> L*H/2;
area(circle, R) -> R * R * 3.14;
area(_, _) -> err.

% List = [1, 2, 3, 4, 5, 6, 7, 8, 9]
% 列表推导
printByComprehension(List) -> [X || X <- List, X rem 2 == 0].

% 递归
printByRecurrence([H | List], List2) when (H rem 2 == 0) -> printByRecurrence(List, [H | List2]);
printByRecurrence([_ | List], List2) -> printByRecurrence(List, List2);
printByRecurrence([], List2) -> List2.

% 高阶函数
printByFun(List) -> lists:filter(fun(Elem) -> 
                                    case is_integer(Elem) of
                                        true -> Elem rem 2 == 0;
                                        false -> false
                                    end
                                end, List).


sum(1) -> 1;
sum(N) when is_integer(N), N > 1 -> sum(N) + sum(N - 1);
sum(_) -> err.

split(Fun, List) -> split(Fun, List, {[], []}).
split(_, [], Tuple) -> Tuple;
split(Fun, [H | List], {List1, List2}) ->
    case Fun(H) of
        true ->  split(Fun, List, {[H | List1], List2});
        false -> split(Fun, List, {List1, [H | List2]})
    end.

quickSort([Pivot | T]) ->
    [quickSort([X || X <- T, x < Pivot]) ++ [Pivot] ++ quickSort([X || X <- T, x >= Pivot])];
quickSort([]) -> [].

split_test() ->
    List = [1, a, 3, b, 4],
    split(fun(Elem) -> is_integer(Elem) end, List).
