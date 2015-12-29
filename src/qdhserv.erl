-module(qdhserv).

-export([
          main/1
    ]).

%%====================================================================
%% API
%%====================================================================
-spec main(Args) -> no_return() when Args :: [string()].
main(Args) ->
    PgmName = filename:basename(escript:script_name()),
    halt(qdhserv_main:main(PgmName, Args)).
