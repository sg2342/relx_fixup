-module(relx_fixup).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = relx_fixup_prv:init(State),
    {ok, State1}.
