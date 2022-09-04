#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, [{application, _, App}]} = file:consult("src/lc.app.src"),
    {ok, [{VsnAppup, _Up, _Down}]} = file:consult("src/lc.appup.src"),
    {vsn, VsnApp} = lists:keyfind(vsn, 1, App),
    case VsnAppup =:= VsnApp of
        true -> ok;
        false -> error([{appup, VsnAppup}, {app, VsnApp}])
    end.
