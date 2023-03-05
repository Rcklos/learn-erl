-module(list_learn).
-export([get/2]).


get([], _, _) -> err;
get([Item | _], Index, Index) -> Item;
get([_| List], Index, Now) -> get(List, Index, Now + 1).
get(List, Index) -> get(List, Index, 0).
