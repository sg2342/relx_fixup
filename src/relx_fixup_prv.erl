-module(relx_fixup_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, relx_fixup).
-define(DEPS, [release]).


-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Desc = "remove unneeded files from release(s), copy start.boot",
    Provider = providers:create([ {name, ?PROVIDER}
				, {module, ?MODULE}
				, {bare, false}
				, {deps, ?DEPS}
				, {example, "rebar3 tar"}
				, {opts, []}
				, {short_desc, "relx fixup"}
				, {desc, Desc}]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Relx = rebar_state:get(State, relx, []),
    do1(p_get(include_erts, Relx, false), p_get(dev_mode, Relx, true), State).

do1(false, _, State) ->
    rebar_api:warn("erts not included, skip ~p", [?PROVIDER]), {ok, State};
do1(true, true, State) ->
    rebar_api:warn("dev mode enabled, skip ~p", [?PROVIDER]), {ok, State};
do1(true, false, State) ->
    BaseDir = rebar_dir:base_dir(State),
    Releases =
	[V || {release, _, _} = V <- rebar_state:get(State, relx, [])],

    %% obtain VSNs for all releases configured
    %% through the rexl config mangler
    ResolvedRs =
	case rebar_version() >= "3.14" of
	    true -> % relx >= 4.0.0
		{ok, XS} = rlx_config:to_state(Releases),
		maps:keys(rlx_state:configured_releases(XS));
	    false -> % relx < 4.0.0
		XS0 = rlx_state:new("", [], []),
		{ok, XS} = lists:foldl(fun rlx_config:load_terms/2,
				       {ok, XS0}, Releases),

		ec_dictionary:keys(rlx_state:configured_releases(XS))
	end,

    InstalledRs = lists:filter(
		    fun ({Name, _Vsn}) ->
			    filelib:is_dir(filename:join([BaseDir, rel, Name]))
		    end, ResolvedRs),
    lists:foldl(fun do2/2, {ok, State}, InstalledRs).


do2({Name, Vsn}, {ok, State}) ->
    rebar_api:info("~p: found Release: ~p with Vsn:~p", [?PROVIDER, Name, Vsn]),

    PState = rebar_state:get(State, ?PROVIDER, []),
    LibRmWildCards = p_get(lib_rm_wildcards, PState,
			   [ "include/*.hrl"
			   , "priv/obj/*.o", "priv/obj/Makefile"
			   , "priv/lib/otp_test_engine.so"
			   , "priv/conf/agent/*.conf"
			   , "priv/conf/manager/*.conf" ]),
    BaseDir = rebar_dir:base_dir(State),
    RelDir = filename:join([BaseDir, "rel", Name]),

    rebar_api:info("~p:  apply whitelist in erts-*", [?PROVIDER]),
    ErtsWhiteList = p_get(erts_whitelist, PState,
			  [ "bin/erlexec"
			  , "bin/beam.smp"
			  , "bin/erl_child_setup"
			  , "bin/epmd"
			  , "bin/heart"]),
    filter_erts(RelDir, ErtsWhiteList),

    rebar_api:info("~p:  wildcard filter lib dirs", [?PROVIDER]),
    LibDir = filename:join(RelDir, "lib"),
    filter_libs(LibDir, LibRmWildCards),

    case rebar_version() >= "3.14" of
	true -> ok;
	false ->
	    rebar_api:info("~p:  copy start.boot", [?PROVIDER]),
	    RDir = filename:join([RelDir, "releases", Vsn]),
	    copy_start_boot(RDir, Name)
    end,
    {ok, State};
do2(_, R) -> R.


copy_start_boot(RDir, Name) ->
    BootF = filename:join(RDir, atom_to_list(Name) ++ ".boot"),
    StartF = filename:join(RDir, "start.boot"),
    copy_start_boot1(filelib:is_file(BootF), BootF, StartF).

copy_start_boot1(false, BootF, _) ->
    rebar_api:warn("~p: SKIP (not found ~p)", [?PROVIDER, BootF]);
copy_start_boot1(true, BootF, StartF) ->
    {ok, _} = file:copy(BootF, StartF), ok.


p_get(Key, PropList, Default) ->
    hd(lists:reverse([Default|proplists:get_all_values(Key, PropList)])).


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


filter_libs(Dir, WildCard) ->
    filter_libs1(file:list_dir(Dir), Dir, WildCard).

filter_libs1({error, enoent}, Dir, _) ->
    rebar_api:warn("~p: SKIP (no lib dir ~p)", [?PROVIDER, Dir]);
filter_libs1({ok, L0}, Dir, WildCard) ->
    {ok, L0} = file:list_dir(Dir),
    L = lists:filter(fun filelib:is_dir/1, [filename:join(Dir, V) || V <- L0]),
    lists:foreach(fun file:delete/1, filter_libs2(L, WildCard, [])),
    zap_empty_dirs_r(Dir).

filter_libs2([], _, Acc) -> Acc;
filter_libs2([D|T], WC, Acc) ->
    filter_libs2(T, WC, filter_libs3(WC, D, Acc)).

filter_libs3([], _, Acc) -> Acc;
filter_libs3([WC|T], D, Acc0) ->
    Acc = Acc0 ++ [filename:join(D, V) || V <- filelib:wildcard(WC,D)],
    filter_libs3(T, D, Acc).


filter_erts(RelDir, WhiteList) ->
    filter_erts1(filelib:wildcard("erts-*", RelDir), RelDir, WhiteList).

filter_erts1([ED|_], RelDir, WhiteList) ->
    filter_erts2(filename:join(RelDir, ED), WhiteList);
filter_erts1(_, RelDir, _) ->
    rebar_api:warn("~p: SKIP (no erts-* dir in ~p)", [?PROVIDER, RelDir]).

filter_erts2(Dir, WhiteList ) ->
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

rebar_version() ->
    {ok, Vsn} = application:get_key(rebar,vsn),
    Vsn.
