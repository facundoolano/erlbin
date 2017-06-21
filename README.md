# erlbin

An example cowboy application, using the REST and WebSocket behaviours.

## Build

    $ rebar3 compile

## Release

     $ rebar3 release -n prod tar

Will generate the release in `_build/default/rel/prod/prod-0.0.1.tar.gz`.

To run the release:

    $ tar -zxvf prod-0.0.1.tar.gz
    $ bin/prod start

## Release upgrade

Assuming an upgrade from `0.0.1` to `0.0.2`, update the `vsn` accordingly in
`src/erlbin.app.src`:

``` erlang
{application, erlbin,
 [{description, "An OTP application"},
  {vsn, "0.0.2"},
  ...
 ]}.
```

And in the relx release in `rebar.config`:

``` erlang
{relx, [{release, {prod, "0.0.1"},
         ...
        ]}]}
```

Then, create the `ebin/erlbin.appup` file in the root of the project with
instructions for the upgrade (see the [appup cookbook](http://erlang.org/doc/design_principles/appup_cookbook.html)).
For example, to update the code in the `erlbin_table` gen_server, the file could look like:

``` erlang
{"0.0.2",
 [{"0.0.1", [{update, erlbin_table, {advanced, []}}]}],
 [{"0.0.1", [{update, erlbin_table, {advanced, []}}]}]}.
```

Now generate the release, the relup file and package it in a tar:

     $ rebar3 release -n prod
     $ rebar3 relup -n prod
     $ rebar3 release -n prod tar

This will generate the new release, with the proper upgrade configuration, in
`_build/default/rel/prod/prod-0.0.2.tar.gz`.

The new release needs to be placed (without unpacking) in the `releases` directory
of a previously running release (the directory where the release was unpacked in
the previous section). Once the .tar.gz is in place, the release can be upgraded
by running:

    $ bin/prod install 0.0.2
