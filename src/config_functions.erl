-module(config_functions).
-export([config_as_map/1, map_search_pred/2]).

config_as_map(Filename) ->
    {ok, Data} = file:consult(Filename),
    maps:from_list(Data).

search(_Pred, []) -> error;
search(Pred, [{K, V}|Ts]) ->
    case Pred(K, V) of
        true -> {K, V};
        false -> search(Pred, Ts)
    end.

map_search_pred(M, Pred) ->
    search(Pred,  maps:to_list(M)).