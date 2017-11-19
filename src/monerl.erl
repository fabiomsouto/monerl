-module(monerl).

-include("include/monerl.hrl").

%% ------------------------------------------------------------------
%% Constant definitions
%% ------------------------------------------------------------------
-define(MONERO_ATOMIC_UNITS, 1000000000000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([do_payment/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
%% do_payment/1: Make a payment. Amount is equal to the atomic amount
%% that you want to receive.
-spec do_payment(Amount :: non_neg_integer()) -> success(Payment :: payment()) | error(Reason :: any()).
do_payment(Amount) when Amount >= 0 ->
	{ok, WalletAddress} =  application:get_env(monerl, wallet_address),
	{ok, #integrated_address{
		integrated_address = IntegratedAddress,
		payment_id = PaymentId
	}} = monerl_json_rpc:make_integrated_address(),
	[NonAtomicAmount] = io_lib:format("~.12f", [Amount / ?MONERO_ATOMIC_UNITS]),
	BinNonAtomicAmount = list_to_binary(NonAtomicAmount),
	MoneroURL = monerl_utils:generate_monero_uri(IntegratedAddress, PaymentId, BinNonAtomicAmount),
	QrCodeURL = monerl_utils:gcharts_qr_url(MoneroURL),
	#payment{
		payment_id = PaymentId,
		address = IntegratedAddress,
		amount = Amount,
		qr_url = QrCodeURL
	};
do_payment(_Amount) ->
	{error, invalid_amount}.