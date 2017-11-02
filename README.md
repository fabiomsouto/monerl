# monerl

WORK IN PROGRESS!

A library to allow receiving monero payments in an Erlang project.

# Usage

## API

- `do_payment/1`: pay to the currently configured wallet
- `verify_payment/1`: verify if a payment went through

## Dependencies

This library depends on the following:

- Erlang/OTP 19
- rebar3
- The monero wallet RPC daemon. You can find the Monero wallet-rpc [here](https://getmonero.org/downloads/).

## 1. Configuring your RPC daemon

These are very general guidelines, if you'd like to expand them feel free to open a PR and I'll glady merge them in.
You will need an RPC daemon, linked to your wallet, so that the module will be able to function.

- Setup a monero wallet. More instructions can be read at [getmonero.org](https://getmonero.org).
- Start the wallet RPC process, ideally in background. This example makes the following assumptions:
    - You will run the daemon at port 18082. Change it if you want, and take note of it.
    - This example uses username:password as an example for the RPC login, please make sure to use a strong combination.
    - A log level of 2 is set
    - The wallet file is located at /path/to/your/wallet, change it to your wallet file path.


    $ ./monero-wallet-rpc --rpc-bind-port 18082 --rpc-login username:password --log-level 2 --wallet-file /path/to/your/wallet

## 2. Library configuration
For the example above, a configuration for your application will look something like this:

```erlang
{application, example, [
    {description, "An example application"},
    {vsn, "0.1"},
    {applications, [kernel, stdlib, sasl]},
    {modules, [example]},
    {registered, [example]},
    {mod, {example, []}},
    {env, [
        {monerl, [
            {wallet_address, <<"49BKZLHvmgKacPjYFv4iZKJTg2KEhyM958tWaJHX3bUCGrZ3SKXNirvfrCw611eNJid9DKzrUuSwbfXtJjaiNHggLrH6PLN">>},
            {rpc_daemon_host, <<"localhost">>},
            {rpc_daemon_port, 18082},
            {rpc_daemon_username, <<"username">>},
            {rpc_daemon_passwd, <<"password">>},
            {rpc_daemon_proto, json_rpc}
        ]}
    ]}
]}.
```

## Future plans

Right now the module is quite dumb. There's only two operations available, your application still has the burden to verify if payment went through, etc. So, to make things better, this should be done:
- Build a mechanism for asynchronously notifying a process if a payment was received.
- Make the mechanism work even if the application crashes totally (probably with some external component to store payment data).
- Have some nice unit tests.

## Tinkering

    $ rebar3 compile
    $ rebar3 shell

## Disclaimer

This project is done mostly as a hobby. I am not affiliated in any way to the Monero movement, but I relate to the ideals of the project. Online privacy is taking a beating nowadays, and standing still doesn't do much.

I am not affiliated to any dark web projects. Those are for Mr. Robot.

This piece of software is provided as is. Use it at your own risk.

## Donations

I do this mostly as a hobby, but if you'd like to buy me a beer, you're more than welcome! :)

My XMR address is `49BKZLHvmgKacPjYFv4iZKJTg2KEhyM958tWaJHX3bUCGrZ3SKXNirvfrCw611eNJid9DKzrUuSwbfXtJjaiNHggLrH6PLN`.