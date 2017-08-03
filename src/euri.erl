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

%% Record, type, and macro definitions
-record( uri
       , { scheme :: nonempty_string()
         , host :: nonempty_string()
         , port :: non_neg_integer()
         , path :: string()
         , query :: [{nonempty_string(), boolean() | integer() | string()}]
         }
       ).

-opaque uri() :: #uri{}.

-type args() :: #{ scheme => nonempty_string()
                 , host => nonempty_string()
                 , port => non_neg_integer()
                 , path => string()
                 , query => [{nonempty_string(), boolean() | integer() | string()}]
                 }.

%% Type exports
-export_type([ uri/0
             , args/0
             ]).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

-spec new() -> uri().
new() ->
  new(#{}).

-spec new(args()) -> uri().
new(Args) ->
  #uri{ scheme = maps:get(scheme, Args, "https")
      , host = maps:get(host, Args, "localhost")
      , port = maps:get(port, Args, 80)
      , path = maps:get(path, Args, "")
      , query = maps:get(query, Args, [])
      }.

-spec to_string(uri()) -> nonempty_string().
to_string(U) ->
  lists:flatten(
    [ %% scheme and host
      U#uri.scheme, "://", U#uri.host
      %% port
    , case U#uri.port of
        80 -> [];
        P  -> [":", integer_to_list(P)]
      end
      %% path
    , case U#uri.path of
        ""           -> "";
        [$/ | _] = P -> encode_path(P);
        _ = P        -> [$/, encode_path(P)]
      end
      %% query
    , case U#uri.query of
        [] -> [];
        Q  -> [$?, encode_query(Q)]
      end
    ]
   ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

encode_path(P) ->
  [encode_path_char(C) || C <- P].

encode_path_char(C) ->
  case C of
    $/ -> C;
    _  -> http_uri:encode([C])
  end.

encode_query(Q) ->
  intersperse($&, [encode_query_param(K, V) || {K, V} <- Q, V /= false]).

encode_query_param(K, true) ->
  [http_uri:encode(K)];
encode_query_param(K, V) when is_integer(V) ->
  [http_uri:encode(K), "=", integer_to_list(V)];
encode_query_param(K, V) when is_list(V) ->
  [http_uri:encode(K), "=", http_uri:encode(V)].

intersperse(_S, L = [_]) ->
  L;
intersperse(S, [X | Xs]) ->
  [X, S | intersperse(S, Xs)].

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
  %% Test path
  U3 = new(#{path => "/"}),
  "https://localhost/" = to_string(U3),
  U4 = new(#{path => "foo"}),
  "https://localhost/foo" = to_string(U4),
  U5 = new(#{path => "/path that/needs encoding"}),
  "https://localhost/path%20that/needs%20encoding" = to_string(U5),
  %% Test query
  U6 = new(#{query => [{"foo", true}, {"bar", 42}, {"b a z", "huh?"}]}),
  "https://localhost?foo&bar=42&b%20a%20z=huh%3F" = to_string(U6),
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
