-module(euri).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API exports
-export([ new/0
        , new/1
        , to_string/1
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

to_string(U) ->
  lists:flatten(
    [ U#uri.scheme, "://", U#uri.host
    , case U#uri.port of
        80 -> [];
        P  -> [":", integer_to_list(P)]
      end
    , U#uri.path
    ]
   ).

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

new_test() ->
  %% Test defaults
  U1 = new(),
  "https" = U1#uri.scheme,
  "localhost" = U1#uri.host,
  80 = U1#uri.port,
  "" = U1#uri.path,
  %% Test overrides
  U2 = new(#{scheme => "http", host => "erlang.org", port => 8080, path => "/"}),
  "http" = U2#uri.scheme,
  "erlang.org" = U2#uri.host,
  8080 = U2#uri.port,
  "/" = U2#uri.path,
  %% Done
  ok.

to_string_test() ->
  %% Test simplest case
  U1 = new(),
  "https://localhost" = to_string(U1),
  %% Test port
  U2 = new(#{port => 8080}),
  "https://localhost:8080" = to_string(U2),
  %% Done
  ok.

-endif.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
