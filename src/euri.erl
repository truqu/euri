-module(euri).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API exports
-export([ new/0
        , new/1
        ]
       ).

%% Record and macro definitions
-record( uri
       , { scheme :: nonempty_string()
         , host :: nonempty_string()
         , port :: non_neg_integer()
         , path :: string()
         }
       ).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

new() ->
  new(#{}).

new(Args) ->
  #uri{ scheme = maps:get(scheme, Args, "https")
      , host = maps:get(host, Args, "localhost")
      , port = maps:get(port, Args, 80)
      , path = maps:get(path, Args, "")
      }.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

new_test() ->
  %% Test defaults
  U = new(),
  "https" = U#uri.scheme,
  "localhost" = U#uri.host,
  80 = U#uri.port,
  "" = U#uri.path.

-endif.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
