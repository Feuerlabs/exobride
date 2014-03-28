-module(util).

-export([get_opt/3]).

get_opt(K, Opts, Default) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} -> V;
        false  ->
            if is_function(Default,0) -> Default();
               true -> Default
            end
    end.

