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
       , { scheme = "https"   :: nonempty_string()
         , host = "localhost" :: nonempty_string()
         , port = 80          :: non_neg_integer()
         , path = ""          :: string()
         }
       ).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

new() ->
  new(#{}).

new(Args) ->
  #uri{}.

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
