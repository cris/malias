# import_as

To prevent name collision, every Erlang library uses some prefix for each
module(e.g.: cowboy\_, ejabberd\_, rabbit\_). But standard Erlang doesn't
provide means for importing such long names in more readable and short form.

import_as is a parse-transform library, which allows to use any module with
another name.

Here is a short example of using Cowboy library:

    -module(index_handler).
    -behaviour(cowboy_http_handler).

    -compile({parse_transform, import_as}).
    -import_as([{cowboy_req, req}]).

    handle(Req, State) ->
        Body = <<"<h1>It works!</h1>">>,
        {ok, Req2} = req:reply(200, [], Body, Req),
        {ok, Req2, State}.

We imported **cowboy_req** module as **req** module and used it inside
index_handler module.

## Usage

To work with import_as, you need to provide import options via import_as
attribute. It's a simple proplist with {original_name, aliased_name} pairs:

    -import_as([{lists, l}, {string, s}, {myproject_handler, handler}]).

Also, add {parse_transform, import_as} compile option:

    -compile({parse_transform, import_as}).

Or set it in compilation options for rebar:

    %% add import_as in dependencies
    {deps, [
      {import_as, ".*", {git, "https://github.com/cris/import_as.git", {branch,
"master"}}}
    ]}.

    %% Erlang compiler options
    {erl_opts, [{parse_transform, import_as}]}.


