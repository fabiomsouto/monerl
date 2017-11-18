-module(monerl_json_rpc).

-include("include/monerl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([make_integrated_address/0]).
-export([make_integrated_address/1]).

-record(settings, {
    host :: string(),
    port :: non_neg_integer(),
    connect_timeout :: non_neg_integer(),
    timeout :: non_neg_integer()
}).
-type settings() :: #settings{}.

-record(integrated_address, {
    integrated_address :: binary(),
    payment_id :: binary()
}).
-type integrated_address() :: #integrated_address{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec make_integrated_address() 
    -> Result :: success(integrated_address()) | error(Reason :: any()).
make_integrated_address() ->
    make_integrated_address(undefined).

-spec make_integrated_address(PaymentId :: payment_id() | undefined) 
    -> Result :: success(integrated_address()) | error(Reason :: any()).
make_integrated_address(undefined) ->
    PaymentId = monerl_utils:generate_payment_id(),
    make_integrated_address(PaymentId);
make_integrated_address(PaymentId) ->
    HexPaymentId = monerl_utils:bin_to_hex(PaymentId),
    Payload = json_rpc_req(0, <<"make_integrated_address">>, [
        {<<"payment_id">>, HexPaymentId}
    ]),
    {ok, {_Status, _Headers, Body, _, _}} = fusco_rpc_call(get_settings(), Payload),
    DecodedBody = jsx:decode(Body),
    Result = proplists:get_value(<<"result">>, DecodedBody),
    IntegratedAddress = proplists:get_value(<<"integrated_address">>, Result),
    HexPaymentId = proplists:get_value(<<"payment_id">>, Result),
    {ok, #integrated_address{
        integrated_address = IntegratedAddress,
        payment_id = HexPaymentId
    }}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec json_rpc_req(Id :: non_neg_integer(), Method :: binary(), Params :: proplists:proplist()) -> binary().
json_rpc_req(Id, Method, Params) ->
    jsx:encode([
        {<<"jsonrpc">>, <<"2.0">>},
        {<<"id">>, list_to_binary(integer_to_list(Id))},
        {<<"method">>, Method},
        {<<"params">>, Params}
    ]).

% todo improve spec
-spec fusco_rpc_call(Settings :: settings(), Payload :: binary())
    -> Result :: any().
fusco_rpc_call(#settings{host=Host, port=Port, connect_timeout=ConnT, timeout=T}, Payload) ->
    {ok, Client} = fusco:start(Host ++ ":" ++ integer_to_list(Port) , [
        {connect_timeout, ConnT}
    ]),
    Headers = [],
    Result = fusco:request(Client, <<"/json_rpc">>, <<"POST">>, Headers, Payload, T),
    fusco:disconnect(Client),
    Result.

-spec get_settings() -> settings().
get_settings() ->
    {ok, Host} = application:get_env(monerl, rpc_daemon_host),
    {ok, Port} = application:get_env(monerl, rpc_daemon_port),
    {ok, ConnT} = application:get_env(monerl, rpc_daemon_conn_timeout),
    {ok, T} = application:get_env(monerl, rpc_daemon_timeout),
    #settings{
        host = Host,
        port = Port,
        connect_timeout = ConnT,
        timeout = T
    }.
