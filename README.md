# euri

An Erlang library for constructing URIs

## URIs

A generic URI is of the form:

    scheme:[//[user[:password]@]host[:port]][/path][?query][#fragment]
    
Currently, this library does not support specifying `user`, `password` or
`fragment`. However, we're open to PR's!

## Build

    $ rebar3 compile

## Testing

    $ rebar3 do eunit, dialyzer
