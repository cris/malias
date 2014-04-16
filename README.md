# malias - module alias

To prevent name collision, every Erlang library uses some prefix for each
module(e.g.: cowboy\_, ejabberd\_, rabbit\_). But standard Erlang doesn't
provide means for importing such long names in more readable and short form.

**malias** is a parse-transform library, which allows to use any module with
another name.

Here is a short example of using Cowboy library:

    -module(index_handler).
    -behaviour(cowboy_http_handler).

    -compile({parse_transform, malias}).
    -malias([{cowboy_req, req}]).

    handle(Req, State) ->
        Body = <<"<h1>It works!</h1>">>,
        {ok, Req2} = req:reply(200, [], Body, Req),
        {ok, Req2, State}.

We imported **cowboy_req** module as **req** module and used it inside
index_handler module.

## Usage

To work with **malias**, you need to provide import options via malias
attribute. It's a simple proplist with {original\_name, aliased\_name} pairs:

    -malias([{lists, l}, {string, s}, {myproject_handler, handler}]).

Also, add {parse\_transform, import\_as} compile option:

    -compile({parse_transform, malias}).

Or set it in compilation options for rebar:

    %% add malias in dependencies
    {deps, [
      {malias, ".*", {git, "https://github.com/cris/malias.git", {branch,"master"}}}
    ]}.

    %% Erlang compiler options
    {erl_opts, [{parse_transform, malias}]}.


