-module(monerl_json_rpc).

-include("include/monerl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([make_integrated_address/0]).
-export([make_integrated_address/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec make_integrated_address() -> Result :: binary().
make_integrated_address() ->
	make_integrated_address(undefined).

-spec make_integrated_address(PaymentId :: payment_id()) -> Result :: binary().
make_integrated_address(undefined) ->
	PaymentId = monerl_utils:generate_payment_id(),
	make_integrated_address(PaymentId);
make_integrated_address(PaymentId) ->
	HexPaymentId = monerl_utils:bin_to_hex(PaymentId),
	Payload = json_rpc_req(0, <<"make_integrated_address">>, [
		{<<"payment_id">>, HexPaymentId}
	]),
	Payload.

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