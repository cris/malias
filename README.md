# malias - module alias

To prevent name collision, every Erlang library uses some prefix for each
module(e.g.: `cowboy_`, `ejabberd_`, `rabbit_`). But standard Erlang doesn't
provide means for aliasing such long names to more readable and short forms.

**malias** is a parse-transform library, which allows to use any module with
another(more terse) name.

Here is a short example of using Cowboy library:

    -module(index_handler).
    -behaviour(cowboy_http_handler).

    -compile({parse_transform, malias}).
    -malias([{cowboy_req, req}]).

    handle(Req, State) ->
        Body = <<"<h1>It works!</h1>">>,
        {ok, Req2} = req:reply(200, [], Body, Req),
        {ok, Req2, State}.

Here `cowboy_req` module is aliased to `req` name inside of `index_handler`
module.

## Usage

Add `malias` as first dependency in `rebar.config`:

    %% add malias in dependencies
    {deps, [
      {malias, ".*", {git, "https://github.com/cris/malias.git", {branch,"master"}}}
    ]}.

Being first is crucial, because parse-transform module should be built before
usage in project.

Now add `parse_transform` compile option in each module, where `malias` will be
used:

    -compile({parse_transform, malias}).

Then add `malias` substitution option. It's a simple proplist with
`{original_name, aliased_name}` pairs:

    -malias([{lists, l}, {string, s}, {myproject_handler, handler}]).

For one substitution, bare tuple can be used:

    -malias({lists, l}).

## Implementation details and support

Right now **malias** correctly handle substitution in module function call:

    A = short_name:fun()

or wrapping module function into lambda:

    F = fun short_name:fun/0

**malias** is tested on Erlang R16, and should correctly work on R15 and
R17(with maps support).

In case of any issues fill in a bug or do a pull-request.
