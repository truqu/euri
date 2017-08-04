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
         , path_segments :: [string()]
         , query :: [{nonempty_string(), boolean() | integer() | string()}]
         , trailing_slash :: boolean()
         }
       ).

-opaque uri() :: #uri{}.

-type nonempty_binary() :: <<_:8, _:_*8>>.

-type args() :: #{ scheme => nonempty_string() | nonempty_binary()
                 , host => nonempty_string() | nonempty_binary()
                 , port => non_neg_integer()
                 , path => string() | [nonempty_string()]
                 , query => [{nonempty_string(), boolean() | integer() | string()}]
                 }.

%% Type exports
-export_type([ uri/0
             , args/0
             , nonempty_binary/0
             ]).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

-spec new() -> uri().
new() ->
  new(#{}).

-spec new(args()) -> uri().
new(Args) ->
  %% Get path
  Path = maps:get(path, Args, ""),
  %% Construct record
  #uri{ scheme = get_arg(scheme, Args, "https")
      , host = get_arg(host, Args, "localhost")
      , port = maps:get(port, Args, 80)
      , path_segments = case is_list_of_lists(Path) of
                          true  -> Path;
                          false -> string:tokens(Path, "/")
                        end
      , query = maps:get(query, Args, [])
      , trailing_slash =
          case is_list_of_lists(Path) of
            true  -> false;
            false -> match == re:run(Path, "/$", [{capture, none}])
          end
      }.

-spec to_string(uri()) -> nonempty_string().
to_string(U) ->
  lists:flatten(
    [ %% Scheme and host
      U#uri.scheme, "://", U#uri.host
      %% Port
    , case U#uri.port of
        80 -> [];
        P  -> [":", integer_to_list(P)]
      end
      %% Path
    , case U#uri.path_segments of
        [] -> "";
        _  -> [ "/"
              , string:join( [http_uri:encode(P) || P <- U#uri.path_segments]
                           , "/"
                           )
              ]
      end
    , if
        U#uri.trailing_slash -> "/";
        true                 -> ""
      end
      %% Query
    , case U#uri.query of
        [] -> [];
        Q  -> [$?, encode_query(Q)]
      end
    ]
   ).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

get_arg(K, M, D) ->
  V = maps:get(K, M, D),
  if
    is_binary(V) -> erlang:binary_to_list(V);
    true         -> V
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

is_list_of_lists(L) ->
  lists:all(fun (X) -> erlang:is_list(X) end, L).

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
  [] = U1#uri.path_segments,
  %% Test overrides
  U2 = new(#{scheme => <<"http">>, host => "erlang.org", port => 8080, path => "/"}),
  "http" = U2#uri.scheme,
  "erlang.org" = U2#uri.host,
  8080 = U2#uri.port,
  [] = U2#uri.path_segments,
  true = U2#uri.trailing_slash,
  %% Test path segments
  U3 = new(#{path => ["foo", "bar", "baz"]}),
  ["foo", "bar", "baz"] = U3#uri.path_segments,
  false = U3#uri.trailing_slash,
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
  %% Test path segments
  U7 = new(#{path => ["foo", "bar", "baz"]}),
  "https://localhost/foo/bar/baz" = to_string(U7),
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
