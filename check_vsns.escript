#!/usr/bin/env escript

-mode(compile).

main(_) ->
    {ok, [{application, _, App}]} = file:consult("src/lc.app.src"),
    {ok, [{VsnAppup, _Up, _Down}]} = file:consult("src/lc.appup.src"),
    {vsn, VsnApp} = lists:keyfind(vsn, 1, App),
    case VsnApp of
        "git" ->
            %% vsn is resolved from the git tag at build time; the appup
            %% top key tracks the next intended release independently.
            ok;
        VsnAppup ->
            ok;
        _ ->
            error([{appup, VsnAppup}, {app, VsnApp}])
    end.
