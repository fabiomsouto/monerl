-module(monerl_utils).

-include("include/monerl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([generate_payment_id/0]).
-export([generate_monero_uri/3]).
-export([gcharts_qr_url/1]).
-export([bin_to_hex/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec generate_payment_id() -> payment_id().
generate_payment_id() ->
    crypto:strong_rand_bytes(8).

-spec generate_monero_uri(WalletAddress :: binary(),
                          Amount :: binary(),
                          PaymentId :: binary())
    -> URI :: binary().
generate_monero_uri(WalletAddress, Amount, PaymentId) ->
    <<"monero:",WalletAddress/binary,
      "?tx_amount=",Amount/binary,
      "&tx_payment_id=",PaymentId/binary>>.

-spec gcharts_qr_url(URI :: binary()) -> binary().
gcharts_qr_url(URI) ->
    % this API is deprecated but didn't research for an alternative yet.
    % generate a blob and return it? who knows.
    StrURI = binary_to_list(URI),
    StrEncodedURI = http_uri:encode(StrURI),
    BinEncodedURI = list_to_binary(StrEncodedURI),
    <<"https://chart.googleapis.com/chart?cht=qr&chs=250x250&chl=",BinEncodedURI/binary>>.

-spec bin_to_hex(Bin :: binary()) -> binary().
% check if this is correct
bin_to_hex(<<Bin:64/big-unsigned-integer>>) ->
    list_to_binary(lists:flatten(io_lib:format("~16.16.0b", [Bin]))).