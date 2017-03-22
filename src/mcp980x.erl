%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2016, Robert Balogh
%% @doc @todo Add description to mcp980x.


-module(mcp980x).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, stop/0]).

-export([
 		 set_temperature_limit_set_register/1,
		 get_temperature_limit_set_register/0
		 ]).
-export([
		 set_temperature_hysteresis_register/1,
		 get_temperature_hysteresis_register/0
		 ]).
-export([
		 get_temperature/0, get_temperature_test/3
		 ]).
-export([
		 conversion_mode_continous/0,
		 conversion_mode_shutdown/0,
		 conversion_mode_continous_oneshot_ignored/0,
		 conversion_mode_oneshot/0
		 ]).
-export([
		 get_shutdown_cfg_bit/0,
		 set_shutdown_cfg_bit/1,
		 
		 get_mode_cfg_bit/0,
		 set_mode_cfg_bit/1,
		 
		 get_alertpol_cfg_bit/0,
		 set_alertpol_cfg_bit/1,
		 
		 get_faultqueue_cfg_bit/0,
		 set_faultqueue_cfg_bit/1,
		 
		 get_adcres_cfg_bit/0,
		 set_adcres_cfg_bit/1,
		 
		 get_oneshot_cfg_bit/0,
		 set_oneshot_cfg_bit/1
		 ]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("dev_common.hrl").
-include("mcp980x.hrl").
-include("ale_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).
-define(NUMBER_OF_BYTE_TO_READ, 1).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {hwAddress}).

%% ====================================================================
%% @doc
%% Start server
%% @end
-spec start(HwAddress :: integer()) -> {ok, pid()} | {error, term()}.
%% ====================================================================
start(HwAddress) ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Already started
			{ok, Pid};
		_->	%% The supervisor does not started yet.
			gen_server:start({local, ?SERVER}, ?MODULE, [HwAddress], [{timeout, 5000}]) 
	end.

%% ====================================================================
%% @doc 
%% Stop server.
%% @end
-spec stop() -> 'ok' | {'error', term()}.
%% ====================================================================
stop() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Process is already exists. Must stop it.
			gen_server:call(Pid, {stop}, infinity);
		
		_->	%% Process is not running.
			ok
	end. 


%% ====================================================================
%% @doc
%% Set Temperature Limit-Set register
%% @end
-spec set_temperature_limit_set_register(TemperatureLimit :: float()) -> ok | {error, term()}.
%% ====================================================================
set_temperature_limit_set_register(TemperatureLimit) ->
	do_gen_server_call({set_temperature_limit_set_register, TemperatureLimit}).

%% ====================================================================
%% @doc
%% Get Temperature Limit-Set register
%% @end
-spec get_temperature_limit_set_register() -> 
		  {ok, TempSign :: temperature_sing()} | {error, term()}.
%% ====================================================================
get_temperature_limit_set_register() ->
	do_gen_server_call({get_temperature_limit_set_registert}).

%% ====================================================================
%% @doc
%% Set Temperature Hysteresis register
%% @end
-spec set_temperature_hysteresis_register(TemperatureLimit :: float()) -> ok | {error, term()}.
%% ====================================================================
set_temperature_hysteresis_register(TempHystValue) ->
	do_gen_server_call({set_temperature_hysteresis_register, TempHystValue}).

%% ====================================================================
%% @doc
%% Get Temperature Hysteresis register
%% @end
-spec get_temperature_hysteresis_register() -> {ok, TemperatureLimit :: float()} | {error, term()}.
%% ====================================================================
get_temperature_hysteresis_register() ->
	do_gen_server_call({get_temperature_hysteresis_register}).

%% ====================================================================
%% Read measured temperature value in the device.
-spec get_temperature() -> {ok, TemperatureValue :: float()} | {error, term()}.
%% ====================================================================
get_temperature() ->
	do_gen_server_call({get_temperature}).

%% ====================================================================
%% @doc
%% Read measured temperature value in the device.
-spec get_temperature_test(
		ADCRes :: adcres_cfg_bit(),
		TempSign :: temperature_sing(),
		TempValue :: integer()) -> {ok, TemperatureValue :: float()} | {error, term()}.
%% @end
%% ====================================================================
get_temperature_test(ADCRes, TempSign, TempValue) ->
	do_gen_server_call({get_temperature_test, ADCRes, TempSign, TempValue}).

%% ====================================================================
%% @doc
%% Set conversion mode to continous.
-spec conversion_mode_continous() -> ok | {error, term()}.
%% @end
%% ====================================================================
conversion_mode_continous() ->
	do_gen_server_call({conversion_mode_continous}).

%% ====================================================================
%% @doc
%% Set conversion mode to shutdow.
-spec conversion_mode_shutdown() -> ok | {error, term()}.
%% @end
%% ====================================================================
conversion_mode_shutdown() ->
	do_gen_server_call({conversion_mode_shutdown}).

%% ====================================================================
%% @doc
%% Set conversion mode to continous when one shot is ignored.
-spec conversion_mode_continous_oneshot_ignored() -> ok | {error, term()}.
%% @end
%% ====================================================================
conversion_mode_continous_oneshot_ignored() ->
	do_gen_server_call({conversion_mode_continous_oneshot_ignored}).

%% ====================================================================
%% @doc
%% Set conversion mode to one shot.
-spec conversion_mode_oneshot() -> ok | {error, term()}.
%% @end
%% ====================================================================
conversion_mode_oneshot() ->
	do_gen_server_call({conversion_mode_oneshot}).

%% ====================================================================
%% @doc
%% Read Shutdown cfg bit in the device.
-spec get_shutdown_cfg_bit() -> {ok, ShutdownCfgBit :: shutdown_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_shutdown_cfg_bit() ->
	do_gen_server_call({get_shutdown_cfg_bit}).

%% ====================================================================
%% @doc
%% Set Shutdown cfg bit in the device.
-spec set_shutdown_cfg_bit(ShutdownCfgBit :: shutdown_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
set_shutdown_cfg_bit(ShutdownCfgBit) ->
	do_gen_server_call({set_shutdown_cfg_bit, ShutdownCfgBit}).

%% ====================================================================
%% @doc
%% Read Mode cfg bit in the device.
-spec get_mode_cfg_bit() -> {ok, ModeCfgBit :: mode_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_mode_cfg_bit() ->
	do_gen_server_call({get_mode_cfg_bit}).

%% ====================================================================
%% @doc
%% Set Mode cfg bit in the device.
-spec set_mode_cfg_bit(ModeCfgBit :: shutdown_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
set_mode_cfg_bit(ModeCfgBit) ->
	do_gen_server_call({set_mode_cfg_bit, ModeCfgBit}).

%% ====================================================================
%% @doc
%% Read Alert Polarity cfg bit in the device.
-spec get_alertpol_cfg_bit() -> {ok, AlertPolCfgBit :: alertpol_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_alertpol_cfg_bit() ->
	do_gen_server_call({get_alertpol_cfg_bit}).

%% ====================================================================
%% @doc
%% Set Alert polarity cfg bit in the device.
-spec set_alertpol_cfg_bit(AlertPolCfgBit :: alertpol_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
set_alertpol_cfg_bit(AlertPolCfgBit) ->
	do_gen_server_call({set_alertpol_cfg_bit, AlertPolCfgBit}).

%% ====================================================================
%% @doc
%% Read Fault Queue cfg bit in the device.
-spec get_faultqueue_cfg_bit() -> {ok, FaultQueueCfgBit :: faultqueue_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_faultqueue_cfg_bit() ->
	do_gen_server_call({get_faultqueue_cfg_bit}).

%% ====================================================================
%% @doc
%% Set Fault Queue cfg bit in the device.
-spec set_faultqueue_cfg_bit(FaultQueueCfgBit :: faultqueue_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
set_faultqueue_cfg_bit(FaultQueueCfgBit) ->
	do_gen_server_call({set_faultqueue_cfg_bit, FaultQueueCfgBit}).

%% ====================================================================
%% @doc
%% Read ADC resolution cfg bit in the device.
-spec get_adcres_cfg_bit() -> {ok, ADCResCfgBit :: adcres_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_adcres_cfg_bit() ->
	do_gen_server_call({get_adcres_cfg_bit}).

%% ====================================================================
%% @doc
%% Set ADC resolution cfg bit in the device.
-spec set_adcres_cfg_bit(ADCResCfgBit :: adcres_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
set_adcres_cfg_bit(ADCResCfgBit) ->
	do_gen_server_call({set_adcres_cfg_bit, ADCResCfgBit}).

%% ====================================================================
%% @doc
%% Read One-Shot cfg bit in the device.
-spec get_oneshot_cfg_bit() -> {ok, OneShotCfgBit :: oneshot_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_oneshot_cfg_bit() ->
	do_gen_server_call({get_oneshot_cfg_bit}).

%% ====================================================================
%% @doc
%% Set One-Shot cfg bit in the device.
-spec set_oneshot_cfg_bit(OneShotCfgBit :: oneshot_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
set_oneshot_cfg_bit(OneShotCfgBit) ->
	do_gen_server_call({set_oneshot_cfg_bit, OneShotCfgBit}).

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
handle_call({set_temperature_limit_set_register, TemperatureLimit}, _From, State) ->
	{reply, do_set_temperature_limit_set_register(State#state.hwAddress, TemperatureLimit), State};

handle_call({get_temperature_limit_set_registert}, _From, State) ->
	{reply, do_get_temperature_limit_set_register(State#state.hwAddress), State};

handle_call({set_temperature_hysteresis_register, TempHystValue}, _From, State) ->
	{reply, do_set_temperature_hysteresis_register(State#state.hwAddress, TempHystValue), State};

handle_call({get_temperature_hysteresis_register}, _From, State) ->
	{reply, do_get_temperature_hysteresis_register(State#state.hwAddress), State};

handle_call({get_temperature}, _From, State) ->
	{reply, do_get_temperature(State#state.hwAddress), State};

handle_call({get_temperature_test, ADCRes, TempSign, TempValue }, _From, State) ->
	{reply, compute_temperature(ADCRes, TempSign, TempValue), State};

handle_call({conversion_mode_continous}, _From, State) ->
	{reply, do_conversion_mode_continous(State#state.hwAddress), State};

handle_call({conversion_mode_shutdown}, _From, State) ->
	{reply, do_conversion_mode_shutdown(State#state.hwAddress), State};

handle_call({conversion_mode_continous_oneshot_ignored}, _From, State) ->
	{reply, do_conversion_mode_continous_oneshot_ignored(State#state.hwAddress), State};

handle_call({conversion_mode_oneshot}, _From, State) ->
	{reply, do_conversion_mode_oneshot(State#state.hwAddress), State};

handle_call({get_shutdown_cfg_bit}, _From, State) ->
	{reply, do_get_shutdown_cfg_bit(State#state.hwAddress), State};

handle_call({set_shutdown_cfg_bit, ShutdownCfgBit}, _From, State) ->
	{reply, do_set_shutdown_cfg_bit(State#state.hwAddress, ShutdownCfgBit), State};

handle_call({get_mode_cfg_bit}, _From, State) ->
	{reply, do_get_mode_cfg_bit(State#state.hwAddress), State};

handle_call({set_mode_cfg_bit, ModeCfgBit}, _From, State) ->
	{reply, do_set_mode_cfg_bit(State#state.hwAddress, ModeCfgBit), State};

handle_call({get_alertpol_cfg_bit}, _From, State) ->
	{reply, do_get_alertpol_cfg_bit(State#state.hwAddress), State};

handle_call({set_alertpol_cfg_bit, AlertPolCfgBit}, _From, State) ->
	{reply, do_set_alertpol_cfg_bit(State#state.hwAddress, AlertPolCfgBit), State};

handle_call({get_faultqueue_cfg_bit}, _From, State) ->
	{reply, do_get_faultqueue_cfg_bit(State#state.hwAddress), State};

handle_call({set_faultqueue_cfg_bit, FaultQueueCfgBit}, _From, State) ->
	{reply, do_set_faultqueue_cfg_bit(State#state.hwAddress, FaultQueueCfgBit), State};

handle_call({get_adcres_cfg_bit}, _From, State) ->
	{reply, do_get_adcres_cfg_bit(State#state.hwAddress), State};

handle_call({set_adcres_cfg_bit, ADCResCfgBit}, _From, State) ->
	{reply, do_set_adcres_cfg_bit(State#state.hwAddress, ADCResCfgBit), State};

handle_call({get_oneshot_cfg_bit}, _From, State) ->
	{reply, do_get_oneshot_cfg_bit(State#state.hwAddress), State};

handle_call({set_oneshot_cfg_bit, OneShotCfgBit}, _From, State) ->
	{reply, do_set_oneshot_cfg_bit(State#state.hwAddress, OneShotCfgBit), State};

handle_call({stop}, _From, State) ->	
	%% Stop gen_server
	{stop, normal, ok, State};

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
%% Set Temperature Limit-Set register.
%% Note: The register contains the 9 bit data in two's complement,
%%       So get/set this register can accepts the 9 bit resolution
%%       values. All other values will be rejected.
%% @end
-spec do_set_temperature_limit_set_register(
		HwAddress :: integer(),
		TemperatureLimit :: float()) -> ok | {error, term()}.
%% ====================================================================
do_set_temperature_limit_set_register(HwAddress, TemperatureLimit) ->
	RegisterRec = #mcp980xTemperatureLimitSetReg{},
	
	%% Calculate the integer and fractionation part of the TemperatureLimit.
	%% To compute these values can be done by this formula: Code = TemperatureLimit / Multiplier_of_9bit_resolution.
	%% If no reminder of the result of division, the given TemperatureLimit value is valid, oherwise it does not.
	ADCRes = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
	case lists:keysearch(ADCRes, 1, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
		{value, {ADCRes, Multiplier}} ->
			Code = TemperatureLimit / Multiplier,
			
			case (Code - trunc(Code)) == 0 of
				true ->
					%% No reminder of the division, the given temperature limit is valid.
					case get_temperature_sign(TemperatureLimit) of
						{ok, TempSign} ->
							case dev_common:bitfield_set(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, 
														 0, RegisterRec, #mcp980xTemperatureLimitSetReg.address, 
														 [{#mcp980xTemperatureLimitSetReg.bit_Sign, TempSign},
														  {#mcp980xTemperatureLimitSetReg.bit_TempValue, round(Code)}]) of
								{ok, _} ->
									ok;
								
								ER->ER
							end;
						
						ER->ER
					end;
				
				false ->
					%% There is reminder of the division, this means the given temperature limit value does not fit to the
					%% 9-bit requirement.
					{error, {string:concat("Error, the given temperature limit value should be divide without reminder by ", erlang:float_to_list(Multiplier,[{decimals, 1}])),
							 [{temperatureLimit, TemperatureLimit},
							  {divideRes, Code}
							  ]}}
			end;

		false ->
			{error, {"Multiplier does not found", {adcRes, ADCRes}}}
	end.

%% ====================================================================
%% @doc
%% Get Temperature Limit-Set register.
%% Note: The register contains the 9 bit data in two's complement.
%% @end
-spec do_get_temperature_limit_set_register(
		HwAddress :: integer()) -> 
		  {ok, TemperatureLimit :: float()} | {error, term()}.
%% ====================================================================
do_get_temperature_limit_set_register(HwAddress) ->
	RegisterRec = #mcp980xTemperatureLimitSetReg{},
	case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, ?NUMBER_OF_BYTE_TO_READ,
								 RegisterRec#mcp980xAmbientTemperatureReg.length,
								 RegisterRec, 
								 {addrIdx, #mcp980xTemperatureLimitSetReg.address}, 
								 [#mcp980xTemperatureLimitSetReg.bit_Sign, 
								  #mcp980xTemperatureLimitSetReg.bit_TempValue]) of
		
		[{#mcp980xTemperatureLimitSetReg.bit_Sign, TempSign}, 
		 {#mcp980xTemperatureLimitSetReg.bit_TempValue, TempValue}] ->
			%% The 2 byte temperature register has been read. Now the temperature value needs to be compute from this value.
			ADCRes = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
			compute_temperature(ADCRes, TempSign, TempValue);
		
		{error, ER} ->
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Set Temperature Hysteresis register.
%% Note: The register contains the 9 bit data in two's complement,
%%       So get/set this register can accepts the 9 bit resolution
%%       values. All other values will be rejected.
%% @end
-spec do_set_temperature_hysteresis_register(
		HwAddress :: integer(),
		TempHystValue :: float()) -> ok | {error, term()}.
%% ====================================================================
do_set_temperature_hysteresis_register(HwAddress, TempHystValue) ->
	RegisterRec = #mcp980xTemperatureHystReg{},
	
	%% Calculate the integer and fractionation part of the TempHystValue.
	%% To compute these values can be done by this formula: Code = TempHystValue / Multiplier_of_9bit_resolution.
	%% If no reminder of the result of division, the given TempHystValue value is valid, oherwise it does not.
	ADCRes = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
	case lists:keysearch(ADCRes, 1, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
		{value, {ADCRes, Multiplier}} ->
			Code = TempHystValue / Multiplier,
			
			case (Code - trunc(Code)) == 0 of
				true ->
					%% No reminder of the division, the given temperature limit is valid.
					case get_temperature_sign(TempHystValue) of
						{ok, TempSign} ->
							case dev_common:bitfield_set(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, 
														 0, RegisterRec, #mcp980xTemperatureHystReg.address, 
														 [{#mcp980xTemperatureHystReg.bit_Sign, TempSign},
														  {#mcp980xTemperatureHystReg.bit_TempValue, round(Code)}]) of
								{ok, _} ->
									ok;
								
								ER->ER
							end;
						
						ER->ER
					end;
				
				false ->
					%% There is reminder of the division, this means the given temperature hysteresis value does not fit to the
					%% 9-bit requirement.
					{error, {string:concat("Error, the given temperature hysteresis value should be divide without reminder by ", erlang:float_to_list(Multiplier,[{decimals, 1}])),
							 [{temperatureLimit, TempHystValue},
							  {divideRes, Code}
							  ]}}
			end;

		false ->
			{error, {"Multiplier does not found", {adcRes, ADCRes}}}
	end.

%% ====================================================================
%% @doc
%% Get Temperature Hysteresis register.
%% Note: The register contains the 9 bit data in two's complement.
%% @end
-spec do_get_temperature_hysteresis_register(
		HwAddress :: integer()) -> 
		  {ok, TemperatureLimit :: float()} | {error, term()}.
%% ====================================================================
do_get_temperature_hysteresis_register(HwAddress) ->
	RegisterRec = #mcp980xTemperatureHystReg{},
	case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, ?NUMBER_OF_BYTE_TO_READ,
								 RegisterRec#mcp980xTemperatureHystReg.length,
								 RegisterRec, 
								 {addrIdx, #mcp980xTemperatureHystReg.address}, 
								 [#mcp980xTemperatureHystReg.bit_Sign, 
								  #mcp980xTemperatureHystReg.bit_TempValue]) of
		
		[{#mcp980xTemperatureHystReg.bit_Sign, TempSign}, 
		 {#mcp980xTemperatureHystReg.bit_TempValue, TempValue}] ->
			%% The 2 byte temperature register has been read. Now the temperature value needs to be compute from this value.
			ADCRes = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
			compute_temperature(ADCRes, TempSign, TempValue);
		
		{error, ER} ->
			{error, ER}
	end.

%% ====================================================================
%% Read measured temperature value in the device.
-spec do_get_temperature(HwAddress :: integer()) -> {ok, TemperatureValue :: float()} | {error, term()}.
%% ====================================================================
do_get_temperature(HwAddress) ->
	%% Read ADC Resolution in the device first
	case do_get_adcres_cfg_bit(HwAddress) of
		{ok, ADCRes} ->
			
			%% Read the measured temperature in the device.
			RegisterRec = #mcp980xAmbientTemperatureReg{},
			case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, ?NUMBER_OF_BYTE_TO_READ,
										 RegisterRec#mcp980xAmbientTemperatureReg.length,
										 RegisterRec, 
										 {addrIdx, #mcp980xAmbientTemperatureReg.address}, 
										 [#mcp980xAmbientTemperatureReg.bit_Sign, 
										  #mcp980xAmbientTemperatureReg.bit_TempValue]) of
				
				[{#mcp980xAmbientTemperatureReg.bit_Sign, TempSign}, 
				 {#mcp980xAmbientTemperatureReg.bit_TempValue, TempValue}] ->
					%% The 2 byte temperature register has been read. Now the temperature value needs to be compute from this value.
					compute_temperature(ADCRes, TempSign, TempValue);
				
				{error, ER} ->
					{error, ER}
			end;
		
		{error, ER} ->
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Set conversion mode to continous.
-spec do_conversion_mode_continous(HwAddress :: integer()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_conversion_mode_continous(HwAddress) ->
	case do_set_cfg_bit(HwAddress, [{#mcp980xConfigReg.bit_OneShot, ?CONFIGURATION_REG_ONE_SHOT_DIS},
									{#mcp980xConfigReg.bit_ShutDown, ?CONFIGURATION_REG_SHUTDOWN_DIS}]) of
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Set conversion mode to shutdow.
-spec do_conversion_mode_shutdown(HwAddress :: integer()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_conversion_mode_shutdown(HwAddress) ->
	case do_set_cfg_bit(HwAddress, [{#mcp980xConfigReg.bit_OneShot, ?CONFIGURATION_REG_ONE_SHOT_DIS},
									{#mcp980xConfigReg.bit_ShutDown, ?CONFIGURATION_REG_SHUTDOWN_EN}]) of
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Set conversion mode to continous when one shot is ignored.
-spec do_conversion_mode_continous_oneshot_ignored(HwAddress :: integer()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_conversion_mode_continous_oneshot_ignored(HwAddress) ->
	case do_set_cfg_bit(HwAddress, [{#mcp980xConfigReg.bit_OneShot, ?CONFIGURATION_REG_ONE_SHOT_EN},
									{#mcp980xConfigReg.bit_ShutDown, ?CONFIGURATION_REG_SHUTDOWN_DIS}]) of
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Set conversion mode to one shot.
-spec do_conversion_mode_oneshot(HwAddress :: integer()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_conversion_mode_oneshot(HwAddress) ->
	case do_conversion_mode_shutdown(HwAddress) of
		ok ->
			case do_set_cfg_bit(HwAddress, [{#mcp980xConfigReg.bit_OneShot, ?CONFIGURATION_REG_ONE_SHOT_EN},
											{#mcp980xConfigReg.bit_ShutDown, ?CONFIGURATION_REG_SHUTDOWN_EN}]) of
				ok ->
					ok;
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Read Shutdown cfg bit in the device.
-spec do_get_shutdown_cfg_bit(HwAddress :: integer()) -> {ok, ShutdownCfgBit :: shutdown_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
do_get_shutdown_cfg_bit(HwAddress) ->
	do_get_cfg_bit(HwAddress, #mcp980xConfigReg.bit_ShutDown).

%% ====================================================================
%% @doc
%% Set Shutdown cfg bit in the device.
-spec do_set_shutdown_cfg_bit(HwAddress :: integer(), ShutdownCfgBit :: shutdown_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_shutdown_cfg_bit(HwAddress, ShutdownCfgBit) ->
	do_set_cfg_bit(HwAddress, #mcp980xConfigReg.bit_ShutDown, ShutdownCfgBit).

%% ====================================================================
%% @doc
%% Read Mode cfg bit in the device.
-spec do_get_mode_cfg_bit(HwAddress :: integer()) -> {ok, ModeCfgBit :: mode_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
do_get_mode_cfg_bit(HwAddress) ->
	do_get_cfg_bit(HwAddress, #mcp980xConfigReg.bit_Mode).

%% ====================================================================
%% @doc
%% Set Mode cfg bit in the device.
-spec do_set_mode_cfg_bit(HwAddress :: integer(), ModeCfgBit :: mode_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_mode_cfg_bit(HwAddress, ModeCfgBit) ->
	do_set_cfg_bit(HwAddress, #mcp980xConfigReg.bit_Mode, ModeCfgBit).

%% ====================================================================
%% @doc
%% Read Alert polarity cfg bit in the device.
-spec do_get_alertpol_cfg_bit(HwAddress :: integer()) -> {ok, AlertPolCfgBit :: alertpol_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
do_get_alertpol_cfg_bit(HwAddress) ->
	do_get_cfg_bit(HwAddress, #mcp980xConfigReg.bit_AlertPol).

%% ====================================================================
%% @doc
%% Set Alert polarity cfg bit in the device.
-spec do_set_alertpol_cfg_bit(HwAddress :: integer(), AlertPolCfgBit :: alertpol_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_alertpol_cfg_bit(HwAddress, AlertPolCfgBit) ->
	do_set_cfg_bit(HwAddress, #mcp980xConfigReg.bit_AlertPol, AlertPolCfgBit).

%% ====================================================================
%% @doc
%% Get Fault Queue cfg bit in the device.
-spec do_get_faultqueue_cfg_bit(HwAddress :: integer()) -> {ok, FaultQueueCfgBit :: faultqueue_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
do_get_faultqueue_cfg_bit(HwAddress) ->
	do_get_cfg_bit(HwAddress, #mcp980xConfigReg.bit_FaultQueue).

%% ====================================================================
%% @doc
%% Set Fault Queue cfg bit in the device.
-spec do_set_faultqueue_cfg_bit(HwAddress :: integer(), FaultQueueCfgBit :: faultqueue_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_faultqueue_cfg_bit(HwAddress, FaultQueueCfgBit) ->
	do_set_cfg_bit(HwAddress, #mcp980xConfigReg.bit_FaultQueue, FaultQueueCfgBit).

%% ====================================================================
%% @doc
%% Read ADC Resolution in the device.
-spec do_get_adcres_cfg_bit(HwAddress :: integer()) -> {ok, AdcResolutionCfgBit :: adcres_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
do_get_adcres_cfg_bit(HwAddress) ->
	do_get_cfg_bit(HwAddress, #mcp980xConfigReg.bit_AdcResolution).

%% ====================================================================
%% @doc
%% Set ADC Resolution cfg bit in the device.
-spec do_set_adcres_cfg_bit(HwAddress :: integer(), AdcResolutionCfgBit :: adcres_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_adcres_cfg_bit(HwAddress, AdcResolutionCfgBit) ->
	do_set_cfg_bit(HwAddress, #mcp980xConfigReg.bit_AdcResolution, AdcResolutionCfgBit).

%% ====================================================================
%% @doc
%% Read One Shot in the device.
-spec do_get_oneshot_cfg_bit(HwAddress :: integer()) -> {ok, OneShotCfgBit :: oneshot_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
do_get_oneshot_cfg_bit(HwAddress) ->
	do_get_cfg_bit(HwAddress, #mcp980xConfigReg.bit_OneShot).

%% ====================================================================
%% @doc
%% Set One Shot cfg bit in the device.
-spec do_set_oneshot_cfg_bit(HwAddress :: integer(), AdcResolutionCfgBit :: oneshot_cfg_bit()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_oneshot_cfg_bit(HwAddress, OneShotCfgBit) ->
	do_set_cfg_bit(HwAddress, #mcp980xConfigReg.bit_OneShot, OneShotCfgBit).

%% ====================================================================
%% @doc
%% Read cfg bit in the device.
-spec do_get_cfg_bit(HwAddress :: integer(), CfgBitIdx :: integer()) -> {ok, CfgBit :: integer()} | {error, term()}.
%% @end
%% ====================================================================
do_get_cfg_bit(HwAddress, CfgBitIdx) ->
	%% Read cfg bit in the device

	case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, ?NUMBER_OF_BYTE_TO_READ,
								 #mcp980xConfigReg{}, {addrIdx, #mcp980xConfigReg.address}, 
								 [CfgBitIdx]) of
		[{CfgBitIdx, CfgBit}] ->
			{ok, CfgBit};
		ER->
			?DO_ERR("Failed to read cfg bit", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Set cfg bit in the device.
-spec do_set_cfg_bit(HwAddress :: integer(), 
					 CfgBitIdx :: integer(), 
					 CfgBit :: integer()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_cfg_bit(HwAddress, CfgBitIdx, CfgBit) ->
	do_set_cfg_bit(HwAddress, [{CfgBitIdx, CfgBit}]).

%% ====================================================================
%% @doc
%% Set cfg bit in the device.
-spec do_set_cfg_bit(HwAddress :: integer(), 
					 CfgData :: list({CfgBitIdx :: integer(), CfgBit :: integer()})) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_cfg_bit(HwAddress, CfgData) ->
	%% Set cfg bit in the device
	case read(HwAddress, 
			  erlang:element(#mcp980xConfigReg.address, #mcp980xConfigReg{}), 
			  erlang:element(#mcp980xConfigReg.length, #mcp980xConfigReg{})) of
		{ok, RegisterValue} ->
			case dev_common:bitfield_set(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress,
										 #mcp980xConfigReg{}, RegisterValue, 
										 {addrIdx, #mcp980xConfigReg.address}, 
										 CfgData) of
				{ok,_} ->
					ok;
				ER->
					?DO_ERR("Failed to read cfg bit", [{reason, ER}]),
					ER
			end;
		ER->
			?DO_ERR("Failed to read cfg bit", [{reason, ER}]),
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

%% ====================================================================
%% @doc
%% Give the temperature sign.
-spec get_temperature_sign(TempValue :: float()) -> {ok, temperature_sing()} | {error, term()}.
%% @end
%% ====================================================================
get_temperature_sign(TempValue) when TempValue > 0 ->
	{ok, ?MCP980x_TEMP_SIGN_PLUS};
get_temperature_sign(TempValue) when TempValue < 0 ->
	{ok, ?MCP980x_TEMP_SIGN_MINUS};
get_temperature_sign(TempValue) ->
	{error, {"Invalid temperature value", TempValue}}.

%% ====================================================================
%% @doc
%% Compute temperature value.
-spec compute_temperature(
		ADCRes :: adcres_cfg_bit(),
		TempSign :: temperature_sing(),
		TempValue :: integer()) -> {ok, TemperatureValue :: float()} | {error, term()}.
%% @end
%% ====================================================================
compute_temperature(ADCRes, TempSign, TempValue) ->
	%% The TemperatureRegValue is a 16 bit wide value, what contains the upper and lower part of the temperature value.
	%% The formula to compute the temperature value is this:
	%% Ta = (Code * Multiplier), where
	%% 					Code is the value of 16 bit temp. register shifted to most left
	%%					Multiplier is the ADC Res. value. Not the bit value, but the associated resulution value. This can be take in
	%%					?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST define.
	
	ShiftRight = case ADCRes of
					?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C ->
						8-(9-8);
					?CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C ->
						8-(10-8);
					?CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C ->
						8-(11-8);
					?CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C ->
						8-(12-8);
					 
					 _-> {error, {"Invalid ADC resolution", ADCRes}}
				end,
	case ShiftRight of
		{error, ER} ->
			{error, ER};
		_->
			%% Shift right the register value with N bit. The number of bit depends of the ADC res. value.
			Code = TempValue bsr ShiftRight,
			case lists:keysearch(ADCRes, 1, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
				{value, {ADCRes, Multiplier}} ->
					?DO_INFO("Compute temperature value", [
														   {adcRes, ADCRes}, 
														   {tempSign, TempSign}, 
														   {origTempValue, TempValue}, 
														   {shiftRight, ShiftRight},
														   {tempValue2, Code},
														   {multiplier, Multiplier}
														  ]),
					Ta = (Code * Multiplier),
					
					case TempSign of
						?MCP980x_TEMP_SIGN_MINUS ->
							{ok, ((-1) * Ta)};
						
						?MCP980x_TEMP_SIGN_PLUS ->
							{ok, Ta};
						
						_->	{error, {"Invalid temperature sign", TempSign}}
					end;
				
				false ->
					{error, {"Multiplier does not found", {adcRes, ADCRes}}}
			end
	end.

%% ====================================================================
%% @doc
%% Read a register in the device
-spec read(HwAddress :: integer(), 
		   RegAddress :: integer(),
		   NumberOfByteToRead :: integer()) -> {ok, int_data()} | {error, term()}.
%% @end
%% ====================================================================
read(HwAddress, RegAddr, NumberOfByteToRead) ->
	dev_common:i2c_read(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, RegAddr, NumberOfByteToRead).

