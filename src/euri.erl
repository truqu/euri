-module(euri).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API exports
-export([ new/0
        , new/1
        , to_string/1
        , to_string/2
        , to_binary/1
        , to_binary/2
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

-type path() :: string() | binary() | [nonempty_string() | nonempty_binary()].

-type query() :: [ { atom() | nonempty_string() | nonempty_binary()
                   , boolean() | integer() | string() | nonempty_binary()
                   }
                 ].

-type args() :: #{ scheme => nonempty_string() | nonempty_binary()
                 , host => nonempty_string() | nonempty_binary()
                 , port => non_neg_integer()
                 , path => path()
                 , query => query()
                 }.

%% Type exports
-export_type([ uri/0
             , args/0
             , nonempty_binary/0
             , query/0
             , path/0
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
  Scheme = to_l(maps:get(scheme, Args, "https")),
  #uri{ scheme = Scheme
      , host = to_l(maps:get(host, Args, "localhost"))
      , port = maps:get(port, Args, case Scheme of
                                      "http"  -> 80;
                                      "https" -> 443
                                    end
                       )
      , path_segments = case is_list_of_lists(Path) of
                          true  -> Path;
                          false ->
                            case is_list_with_binary(Path) of
                              true  -> lists:map(fun to_l/1, Path);
                              false -> string:tokens(to_l(Path), "/")
                            end
                        end
      , query = [{to_l(K), b_to_l(V)} || {K, V} <- maps:get(query, Args, [])]
      , trailing_slash =
          case is_list_of_lists(Path) of
            true  -> false;
            false -> match == re:run(Path, "/$", [{capture, none}])
          end
      }.

-spec to_string(uri()) -> nonempty_string().
to_string(U) ->
  to_string(U, []).

-spec to_string(uri(), [atom()]) -> nonempty_string().
to_string(U, Opts) ->
  Relative = lists:member(relative, Opts),
  lists:flatten(
    [ %% Scheme, host, and port
      if
        Relative ->
          [];
        true ->
          [ U#uri.scheme, "://", U#uri.host
          , case U#uri.port of
              80  -> [];
              443 -> [];
              P   -> [":", integer_to_list(P)]
            end
          ]
      end
      %% Path
    , case {U#uri.path_segments, Relative} of
        {[], false} ->
          "";
        {[], true} ->
          "/";
        {_, _} ->
          [ "/"
          , string:join([http_uri:encode(P) || P <- U#uri.path_segments] , "/")
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

-spec to_binary(uri()) -> nonempty_binary().
to_binary(U) ->
  to_binary(U, []).

-spec to_binary(uri(), [atom()]) -> nonempty_binary().
to_binary(U, Opts) ->
  list_to_binary(to_string(U, Opts)).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

to_l(V) when is_binary(V) ->
  erlang:binary_to_list(V);
to_l(V) when is_atom(V) ->
  erlang:atom_to_list(V);
to_l(V) ->
  V.

b_to_l(V) when is_binary(V) ->
  erlang:binary_to_list(V);
b_to_l(V) ->
  V.

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

is_list_of_lists(L) when is_list(L) ->
  lists:all(fun (X) -> erlang:is_list(X) end, L);
is_list_of_lists(_) ->
  false.

is_list_with_binary(L) when is_list(L) ->
  lists:any(fun (X) -> erlang:is_binary(X) end, L);
is_list_with_binary(_) ->
  false.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(TEST).

new_test() ->
  %% Test defaults
  U1 = new(),
  "https" = U1#uri.scheme,
  "localhost" = U1#uri.host,
  443 = U1#uri.port,
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
  "/" = to_string(U1, [relative]),
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
  U6 = new(#{query => [{foo, true}, {<<"bar">>, 42}, {"b a z", "huh?"}]}),
  "https://localhost?foo&bar=42&b%20a%20z=huh%3F" = to_string(U6),
  %% Test path segments
  U7 = new(#{path => ["foo", "bar", "baz"]}),
  "https://localhost/foo/bar/baz" = to_string(U7),
  %% Done
  ok.

to_binary_test() ->
  %% Test
  <<"https://localhost">> = to_binary(new()),
  %% Done
  ok.

is_list_of_lists_regression_test() ->
  false = is_list_of_lists(<<"/">>),
  ok.

is_list_with_binary_regression_test() ->
  false = is_list_with_binary(<<"/">>),
  ok.

-endif.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% coding: latin-1
%% End:
