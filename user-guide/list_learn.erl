-module(list_learn).
-export([get/2, my_tuple_to_list/1]).


get([], _, _) -> err;
get([Item | _], Index, Index) -> Item;
get([_| List], Index, Now) -> get(List, Index, Now + 1).
get(List, Index) -> get(List, Index, 0).

my_tuple_to_list(T) -> 
    [element(X, T) || X <- lists:seq(1, tuple_size(T))].
