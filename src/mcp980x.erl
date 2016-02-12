%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2016, Robert Balogh
%% @doc @todo Add description to mcp980x.


-module(mcp980x).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 set_temp_limit_set_register/1
		 ]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("mcp980x.hrl").
-include("ale_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {hwAddress}).

%% ====================================================================
%% @doc
%% Set Temperature Limit-Set register
%% @end
-spec set_temp_limit_set_register(integer()) -> ok | {error, term()}.
%% ====================================================================
set_temp_limit_set_register(Value) ->
	do_gen_server_call({set_temp_limit_set_register, Value}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([HwAddress]) ->
	%% Configure default setup in the device.
	
    {ok, #state{hwAddress = HwAddress}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({set_temp_limit_set_register, Value}, _From, State) ->
	{reply, do_set_temp_limit_set_register(State#state.hwAddress, Value), State};
	
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Set Temperature Limit-Set register
%% @end
-spec do_set_temp_limit_set_register(HwAddress :: integer(), Value :: integer()) -> ok | {error, term()}.
%% ====================================================================
do_set_temp_limit_set_register(HwAddress, Value) ->
	RegisterRec = #mcp980xTemperatureLimitSetReg{},
	case dev_common:i2c_read(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, RegisterRec#mcp980xTemperatureLimitSetReg.address) of
		{ok, RegisterValue} ->
			case bitfield_set(RegisterValue, RegisterRec, #rtcSecondReg.address, #rtcSecondReg.bit_st, ?RTC_SECOND_BIT_ST_DIS) of
				{ok,_} ->
					?DO_INFO("RTC oscillator has been stopped", []),
					ok;
				ER->
					?DO_ERR("Failed to stop RTC oscillator", [{reason, ER}]),
					ER
			end;
		ER->
			?DO_ERR("Failed to stop RTC oscillator", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Init a gen_server call.
%% @end
-spec do_gen_server_call(tuple()) -> term().
%% ====================================================================
do_gen_server_call(MSG) ->
	case whereis(?SERVER) of
		P when is_pid(P) ->
			gen_server:call(?SERVER, MSG, ?TIMEOUT);
		ER->{error, ER}
	end.

