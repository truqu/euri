-module(euri).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% API exports
-export([new/0, new/1, to_string/1, to_string/2, to_binary/1, to_binary/2]).

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

-if(?OTP_RELEASE < 24).

%% From OTP 24 onwards, `nonempty_binary/0` is predefined
-type nonempty_binary() :: <<_:8, _:_*8>>.

-endif.

-type path() :: string() | binary() | [nonempty_string() | nonempty_binary()].
-type query() :: [{ Key :: atom() | nonempty_string() | nonempty_binary()
                  , Value :: boolean() | integer() | string() | nonempty_binary()
                  }].
-type args() :: #{ scheme => nonempty_string() | nonempty_binary()
                 , host => nonempty_string() | nonempty_binary()
                 , port => non_neg_integer()
                 , path => path()
                 , query => query()
                 }.
-type option() :: relative.

%% Type exports
-export_type([uri/0, args/0, nonempty_binary/0, query/0, path/0, option/0]).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------
%% @doc Create a new, "empty" URI.
%%
%% This will default to an uri pointing to `https://localhost'. In other words,
%% the scheme will be set to `https', the port to `443', the host to `localhost'
%% and both the path and the query will be left empty.
%%
%% This is equivalent to calling `euri:new(#{})'.

-spec new() -> uri().
new() -> new(#{}).

%% @doc Create a URI from {@type args()}.
%%
%% Note that all the values in {@type args()} are optional. When not supplied,
%% defaults will be used: `scheme' defaults to `https', `host' defaults to
%% `localhost', `port' to `443' and the `path' and `query' are left empty when
%% not supplied.

-spec new(args()) -> uri().
new(Args) ->
  %% Get path
  Path = maps:get(path, Args, ""),
  %% Construct record
  Scheme = to_l(maps:get(scheme, Args, "https")),
  #uri{ scheme = Scheme
      , host = to_l(maps:get(host, Args, "localhost"))
      , port = maps:get( port
                       , Args
                       , case Scheme of
                           "http" -> 80;
                           "https" -> 443
                         end
                       )
      , path_segments = case is_list_of_lists(Path) of
                          true -> Path;
                          false ->
                            case is_list_with_binary(Path) of
                              true -> lists:map(fun to_l/1, Path);
                              false -> string:tokens(to_l(Path), "/")
                            end
                        end
      , query = [{to_l(K), b_to_l(V)} || {K, V} <- maps:get(query, Args, [])]
      , trailing_slash = case is_list_of_lists(Path) of
                           true -> false;
                           false -> match == re:run(Path, "/$", [{capture, none}])
                         end
      }.

%% @doc Turn a {@type uri()} into a `string()'.
%%
%% By default, this will be an absolute URL including the scheme, hostname and
%% port (unless the port is the default port for the given scheme).
%%
%% ```
%%    Uri = euri:new(),
%%    "https://localhost" = euri:to_string(Uri).
%%
%%    Uri = euri:new(#{host => "neverssl.com", scheme => "http"}).
%%    "http://neverssl.com" = euri:to_string(Uri).
%% '''
%%
%% This is equivalent to calling `euri:to_string(Uri, [])'.

-spec to_string(uri()) -> nonempty_string().
to_string(U) -> to_string(U, []).

%% @doc Turn a {@type uri()} into a `string()' with some options.
%%
%% Currently, the only {@type option()} is `relative' which will make this
%% function return an URL relative to the root of the hostname.

-spec to_string(uri(), [option()]) -> nonempty_string().
to_string(U, Opts) ->
  Relative = lists:member(relative, Opts),
  %% Scheme, host, and port
  lists:flatten([ if
                    Relative -> [];
                    true ->
                      [ U#uri.scheme
                      , "://"
                      , U#uri.host
                      , case U#uri.port of
                          80 -> [];
                          443 -> [];
                          P -> [":", integer_to_list(P)]
                        end
                      ]
                  end
                , %% Path
                  case {U#uri.path_segments, Relative} of
                    {[], false} -> "";
                    {[], true} -> "/";
                    {_, _} ->
                      [ "/"
                      , string:join([html5_byte_encode(P) || P <- U#uri.path_segments], "/")
                      ]
                  end
                , if
                    U#uri.trailing_slash -> "/";
                    true -> ""
                  end
                , %% Query
                  case U#uri.query of
                    [] -> [];
                    Q -> [$?, encode_query(Q)]
                  end
                ]).

%% @doc Same as {@link to_string/1} but returns a `binary()'.
-spec to_binary(uri()) -> nonempty_binary().
to_binary(U) -> to_binary(U, []).

%% @doc Same as {@link to_string/2} but returns a `binary()'.
-spec to_binary(uri(), [option()]) -> nonempty_binary().
to_binary(U, Opts) -> list_to_binary(to_string(U, Opts)).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

to_l(V) when is_binary(V) -> erlang:binary_to_list(V);
to_l(V) when is_atom(V) -> erlang:atom_to_list(V);
to_l(V) -> V.

b_to_l(V) when is_binary(V) -> erlang:binary_to_list(V);
b_to_l(V) -> V.

encode_query(Q) -> intersperse($&, [encode_query_param(K, V) || {K, V} <- Q, V /= false]).

encode_query_param(K, true) -> [html5_byte_encode(K)];
encode_query_param(K, V) when is_integer(V) -> [html5_byte_encode(K), "=", integer_to_list(V)];
encode_query_param(K, V) when is_list(V) -> [html5_byte_encode(K), "=", html5_byte_encode(V)].

intersperse(_S, L = [_]) -> L;
intersperse(S, [X | Xs]) -> [X, S | intersperse(S, Xs)].

is_list_of_lists(L) when is_list(L) -> lists:all(fun (X) -> erlang:is_list(X) end, L);
is_list_of_lists(_) -> false.

is_list_with_binary(L) when is_list(L) -> lists:any(fun (X) -> erlang:is_binary(X) end, L);
is_list_with_binary(_) -> false.

-define( DEC2HEX(X)
       , if
           ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
         end
       ).

html5_byte_encode(B) when is_list(B) -> html5_byte_encode(list_to_binary(B));
html5_byte_encode(B) -> html5_byte_encode(B, <<>>).

html5_byte_encode(<<>>, Acc) -> binary_to_list(Acc);
html5_byte_encode(<<$\s, T/binary>>, Acc) -> html5_byte_encode(T, <<Acc/binary, "%20">>);
html5_byte_encode(<<H, T/binary>>, Acc) ->
  case is_url_char(H) of
    true -> html5_byte_encode(T, <<Acc/binary, H>>);
    false ->
      <<A:4, B:4>> = <<H>>,
      html5_byte_encode(T, <<Acc/binary, $%, (?DEC2HEX(A)), (?DEC2HEX(B))>>)
  end;
html5_byte_encode(H, _Acc) -> throw({error, invalid_input, H}).

%% Return true if input char can appear in form-urlencoded string
%% Allowed chararacters:
%%   0x2A, 0x2D, 0x2E, 0x30 to 0x39, 0x41 to 0x5A,
%%   0x5F, 0x61 to 0x7A

is_url_char(C) when
    C =:= 16#2A;
    C =:= 16#2D;
    C =:= 16#2E;
    C =:= 16#5F;
    16#30 =< C,
    C =< 16#39;
    16#41 =< C,
    C =< 16#5A;
    16#61 =< C,
    C =< 16#7A ->
  true;
is_url_char(_) -> false.

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
