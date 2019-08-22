-module(relx_fixup_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, relx_fixup).
-define(DEPS, [release]).


-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, false},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 release"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "relx fixup"},
            {desc, "remove unneeded files from release, copy start.boot"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Relx = rebar_state:get(State, relx, []),
    {release,{Name, Vsn},_} = lists:keyfind(release, 1, Relx),
    case p_get(include_erts, Relx, false) of
	false -> rebar_api:warn("erts not included, skip ~p", [?PROVIDER]);
	_ ->
	    case p_get(dev_mode, Relx, true) of
		true ->
		    rebar_api:warn("dev mode enabled, skip ~p", [?PROVIDER]);
		false -> ok = do({Name, Vsn}, State)
	    end end,
    {ok, State}.

do({Name, Vsn}, State) ->
    PState = rebar_state:get(State, ?PROVIDER, []),
    RelDir = filename:join([rebar_dir:base_dir(State), "rel", Name]),

    rebar_api:info("~p: apply whitelist in erts-*", [?PROVIDER]),
    ErtsDir = filename:join(RelDir, hd(filelib:wildcard("erts-*", RelDir))),
    ErtsWhiteList = p_get(erts_whitelist, PState,
			  [ "bin/erlexec"
			  , "bin/beam.smp"
			  , "bin/erl_child_setup"
			  , "bin/epmd"
			  , "bin/heart"]),
    ok = filter_erts(ErtsDir, ErtsWhiteList),

    rebar_api:info("~p: wildcard filter lib dirs", [?PROVIDER]),
    LibDir = filename:join(RelDir, "lib"),
    LibRmWildCards = p_get(lib_rm_wildcards, PState,
			   [ "include/*.hrl"
			   , "priv/obj/*.o", "priv/obj/Makefile"
			   , "priv/lib/otp_test_engine.so"
			   , "priv/conf/agent/*.conf"
			   , "priv/conf/manager/*.conf" ]),
    ok = filter_libs(LibDir, LibRmWildCards),

    rebar_api:info("~p: copy start.boot", [?PROVIDER]),
    RDir = filename:join([RelDir, "releases", Vsn]),
    {ok, _} = file:copy(filename:join(RDir, atom_to_list(Name) ++ ".boot"),
			filename:join(RDir, "start.boot")),
    ok.


p_get(Key, PropList, Default) ->
    hd(lists:reverse([Default|proplists:get_all_values(Key, PropList)])).


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


filter_libs(Dir, WildCard) ->
    {ok, L0} = file:list_dir(Dir),
    L = lists:filter(fun filelib:is_dir/1, [filename:join(Dir, V) || V <- L0]),
    lists:foreach(fun file:delete/1, filter_libs1(L, WildCard, [])),
    zap_empty_dirs_r(Dir).

filter_libs1([], _, Acc) -> Acc;
filter_libs1([D|T], WC, Acc) ->
    filter_libs1(T, WC, filter_libs2(WC, D, Acc)).

filter_libs2([], _, Acc) -> Acc;
filter_libs2([WC|T], D, Acc0) ->
    Acc = Acc0 ++ [filename:join(D, V) || V <- filelib:wildcard(WC,D)],
    filter_libs2(T, D, Acc).


filter_erts(Dir, WhiteList ) ->
    lists:foreach( fun(F) -> ok = file:delete(filename:join(Dir, F)) end,
		   find_all_files([Dir], []) --
		       [filename:join(Dir, V) || V <- WhiteList]),
    zap_empty_dirs_r(Dir).


find_all_files([], Acc) -> Acc;
find_all_files([D|T], Acc) ->
    {ok, L} = file:list_dir(D),
    {Dirs, Files} = lists:partition(fun filelib:is_dir/1,
				    [filename:join(D,V) || V<- L]),
    find_all_files(T ++ Dirs, Acc ++ Files).


zap_empty_dirs_r(D) ->
    case file:list_dir(D) of
	{ok, []} -> ok = file:del_dir(D);
	{ok, L} ->
	    lists:foreach(fun zap_empty_dirs_r/1,
			  lists:filter(fun filelib:is_dir/1,
				       [filename:join(D,V) || V <- L])),
	    case file:list_dir(D) of
		{ok, []} -> file:del_dir(D);
		_ -> ok
	    end
    end.
