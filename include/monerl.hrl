-define(env(A), application:get_env(A)).

-type payment_id() :: <<_:64>>.

-record(payment, {
	payment_id :: payment_id() | undefined,
	wallet :: binary(),
	amount :: binary(),
	qr_url :: binary()
}).
-type payment() :: #payment{}.