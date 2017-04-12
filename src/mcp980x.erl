%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2016, Robert Balogh
%% @doc @todo Add description to mcp980x.


-module(mcp980x).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

-export([start/1, start/7, start/9, stop/0]).

-export([
 		 set_temperature_limit_set_register/1,
		 get_temperature_limit_set_register/0
		 ]).
-export([
		 set_temperature_hysteresis_register/1,
		 get_temperature_hysteresis_register/0
		 ]).
-export([
		 get_temperature/0, get_temperature_str/0
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
		 
		 get_mode_cfg_labels/0, get_mode_cfg_label/1,
		 get_mode_cfg_bit/1,
		 get_mode_cfg_bit/0,
		 set_mode_cfg_bit/1,
		 
		 get_alertpol_cfg_labels/0, get_alertpol_cfg_label/1,
		 get_alertpol_cfg_bit/1,
		 get_alertpol_cfg_bit/0,
		 set_alertpol_cfg_bit/1,
		 
		 get_faultqueue_cfg_labels/0, get_faultqueue_cfg_label/1,
		 get_faultqueue_cfg_bit/1,
		 get_faultqueue_cfg_bit/0, 
		 set_faultqueue_cfg_bit/1,
		 
		 get_adcres_cfg_labels/0, get_adcres_cfg_label/1, 
		 get_adcres_cfg_bit/1,
		 get_adcres_cfg_bit/0,
		 set_adcres_cfg_bit/1,
		 
		 get_oneshot_cfg_bit/0,
		 set_oneshot_cfg_bit/1,
		 
		 get_default_cfg/0
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

test() ->
	io:format("@@@ mcp980xConfigReg - ~p~n",[#mcp980xConfigReg{}]),
	io:format("@@@ mcp980xAmbientTemperatureReg - ~p~n",[#mcp980xAmbientTemperatureReg{}]),
	io:format("@@@ mcp980xTemperatureHystReg - ~p~n",[#mcp980xTemperatureHystReg{}]),
	io:format("@@@ mcp980xTemperatureLimitSetReg - ~p~n",[#mcp980xTemperatureLimitSetReg{}]).

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
%% Start server
%% @end
-spec start(HwAddress :: integer(),
			AdcRes :: adcres_cfg_bit(),
			FaultQueue :: faultqueue_cfg_bit(),
			AlertPol :: alertpol_cfg_bit(),
			Mode :: mode_cfg_bit(),
			TempLimit :: float(),
			TempHyst :: float()) -> {ok, pid()} | {error, term()}.
%% ====================================================================
start(HwAddress, AdcRes, FaultQueue, AlertPol, Mode, TempLimit, TempHyst) ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Already started
			{ok, Pid};
		_->	%% The supervisor does not started yet.
			gen_server:start({local, ?SERVER}, ?MODULE, [HwAddress, AdcRes, FaultQueue, AlertPol, Mode, TempLimit, TempHyst], [{timeout, 5000}]) 
	end.

%% ====================================================================
%% @doc
%% Start server
%% @end
-spec start(HwAddress :: integer(),
			OneShot :: oneshot_cfg_bit(),
			AdcRes :: adcres_cfg_bit(),
			FaultQueue :: faultqueue_cfg_bit(),
			AlertPol :: alertpol_cfg_bit(),
			Mode :: mode_cfg_bit(),
			Shutdown :: shutdown_cfg_bit(),
			TempLimit :: float(),
			TempHyst :: float()) -> {ok, pid()} | {error, term()}.
%% ====================================================================
start(HwAddress, OneShot, AdcRes, FaultQueue, AlertPol, Mode, Shutdown, TempLimit, TempHyst) ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Already started
			{ok, Pid};
		_->	%% The supervisor does not started yet.
			gen_server:start({local, ?SERVER}, ?MODULE, [HwAddress, OneShot, AdcRes, FaultQueue, AlertPol, Mode, Shutdown, TempLimit, TempHyst], [{timeout, 5000}]) 
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
		  {ok, TemperatureLimit :: float()} | {error, term()}.
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
-spec get_temperature_hysteresis_register() -> {ok, TemperatureLimit :: float()} | {error, term()}.
%% @end
%% ====================================================================
get_temperature_hysteresis_register() ->
	do_gen_server_call({get_temperature_hysteresis_register}).

%% ====================================================================
%% @doc
%% Read measured temperature value in the device.
-spec get_temperature() -> {ok, TemperatureValue :: float()} | {error, term()}.
%% @end
%% ====================================================================
get_temperature() ->
	do_gen_server_call({get_temperature}).

%% ====================================================================
%% @doc
%% Read measured temperature value in the device and gives the value in string.
-spec get_temperature_str() -> {ok, TemperatureValue :: string()}.
%% @end
%% ====================================================================
get_temperature_str() ->
	case get_temperature() of
		{ok, T} ->
			{ok, erlang:float_to_list(T, [{decimals, 4}, compact])};
		{error, _ER} ->
			{ok, "N/A"}
	end.

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
%% Gives the possible Modes as string labels.
-spec get_mode_cfg_labels() -> {ok, list(string())}.
%% @end
%% ====================================================================
get_mode_cfg_labels() ->
	{ok, [?CONFIGURATION_REG_INT_MODE_LBL, ?CONFIGURATION_REG_COMP_MODE_LBL]}.

%% ====================================================================
%% @doc
%% Gives the Modes by its name.
-spec get_mode_cfg_label(ModeCfgBit :: mode_cfg_bit()) -> {ok, string()} | {error, term()}.
%% @end
%% ====================================================================
get_mode_cfg_label(ModeCfgBit) ->
	case ModeCfgBit of
		?CONFIGURATION_REG_INT_MODE ->
			{ok, ?CONFIGURATION_REG_INT_MODE_LBL};
		?CONFIGURATION_REG_COMP_MODE ->
			{ok, ?CONFIGURATION_REG_COMP_MODE_LBL};
		_-> {error, {"Invalid Mode", ModeCfgBit}}
	end.

%% ====================================================================
%% @doc
%% Get the Mode by its name.
-spec get_mode_cfg_bit(string()) -> {ok, ModeCfgBit :: mode_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_mode_cfg_bit(ModeLbl) ->
	case ModeLbl of
		?CONFIGURATION_REG_INT_MODE_LBL ->
			{ok, ?CONFIGURATION_REG_INT_MODE};
		?CONFIGURATION_REG_COMP_MODE_LBL ->
			{ok, ?CONFIGURATION_REG_COMP_MODE};
		_-> {error, {"Invalid Mode", ModeLbl}}
	end.

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
%% Gives the list of Alert Polarity of the device.
-spec get_alertpol_cfg_labels() -> {ok, list(string())}.
%% @end
%% ====================================================================
get_alertpol_cfg_labels() ->
	{ok, [?CONFIGURATION_REG_ALERT_POL_ACT_HIGH_LBL, ?CONFIGURATION_REG_ALERT_POL_ACT_LOW_LBL]}.

%% ====================================================================
%% @doc
%% Gives the name of the Alert Polarity of the device.
-spec get_alertpol_cfg_label(AlertPolCfgBit :: alertpol_cfg_bit()) -> {ok, string()} | {error, term()}.
%% @end
%% ====================================================================
get_alertpol_cfg_label(AlertPolCfgBit) ->
	case AlertPolCfgBit of
		?CONFIGURATION_REG_ALERT_POL_ACT_HIGH ->
			{ok, ?CONFIGURATION_REG_ALERT_POL_ACT_HIGH_LBL};
		?CONFIGURATION_REG_ALERT_POL_ACT_LOW ->
			{ok, ?CONFIGURATION_REG_ALERT_POL_ACT_LOW_LBL};
		_-> {error, {"Invalid Alert Polarity", AlertPolCfgBit}}
	end.

%% ====================================================================
%% @doc
%% Get the Alert Polarity cfg label by its bit value.
-spec get_alertpol_cfg_bit(string()) -> {ok, AlertPolCfgBit :: alertpol_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_alertpol_cfg_bit(AlertPolCfgBitLbl) ->
	case AlertPolCfgBitLbl of
		?CONFIGURATION_REG_ALERT_POL_ACT_HIGH_LBL ->
			{ok, ?CONFIGURATION_REG_ALERT_POL_ACT_HIGH};
		?CONFIGURATION_REG_ALERT_POL_ACT_LOW_LBL ->
			{ok, ?CONFIGURATION_REG_ALERT_POL_ACT_LOW};
		_-> {error, {"Invalid Alert Polarity", AlertPolCfgBitLbl}}
	end.
	
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
%% Gives the list of the available Fault Queue
-spec get_faultqueue_cfg_labels() -> {ok, list(string())}.
%% @end
%% ====================================================================
get_faultqueue_cfg_labels() ->
	L = [begin
			 erlang:element(?CONFIGURATION_REG_FAULT_QUEUE_LABEL_KEYPOS, FQValue)
		 end || FQValue <- ?CONFIGURATION_REG_FAULT_QUEUE_LIST],
	{ok, L}.

%% ====================================================================
%% @doc
%% Gives the integer id of FQ by its label
-spec get_faultqueue_cfg_label(faultqueue_cfg_bit()) -> {ok, string()} | {error, term()}.
%% @end
%% ====================================================================
get_faultqueue_cfg_label(FQ) ->
	case lists:keysearch(FQ, ?CONFIGURATION_REG_FAULT_QUEUE_ID_KEYPOS, ?CONFIGURATION_REG_FAULT_QUEUE_LIST) of
		{value, {FQ, FQLabel}} ->
			{ok, FQLabel};
		_->
			{error, "Invalid Fault Queue"}
	end.

%% ====================================================================
%% @doc
%% Get the FQ Id by its label.
-spec get_faultqueue_cfg_bit(string()) -> {ok, FaultQueueCfgBit :: faultqueue_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_faultqueue_cfg_bit(FQLabel) ->
	case lists:keysearch(FQLabel, ?CONFIGURATION_REG_FAULT_QUEUE_LABEL_KEYPOS, ?CONFIGURATION_REG_FAULT_QUEUE_LIST) of
		{value, {FQ, FQLabel}} ->
			{ok, FQ};
		_->
			{error, "Invalid Fault Queue"}
	end.

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
%% Gives the posible ADC resolutions as text label 
-spec get_adcres_cfg_labels() -> {ok, list(string())} | {error, term()}.
%% @end
%% ====================================================================
get_adcres_cfg_labels() ->
	L = [begin
			 erlang:element(?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_LABEL_KEYPOS, AdcValue)
		 end || AdcValue <- ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST],
	{ok, L}.

%% ====================================================================
%% @doc
%% Gives the label of the ADC resolution
-spec get_adcres_cfg_label(adcres_cfg_bit()) -> {ok, string()} | {error, term()}.
%% @end
%% ====================================================================
get_adcres_cfg_label(ADCRes) ->
	case lists:keysearch(ADCRes, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_ID_KEYPOS, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
		{value, {ADCRes, _Multiplier, AdcResLabel}} ->
			{ok, AdcResLabel};
		_->
			{error, "Invalid ADC resolution"}
	end.

%% ====================================================================
%% @doc
%% Gives the float value of the ADC resolution bit by its label 
-spec get_adcres_cfg_bit(string()) -> {ok, adcres_cfg_bit()} | {error, term()}.
%% @end
%% ====================================================================
get_adcres_cfg_bit(AdcResLabel) ->
	case lists:keysearch(AdcResLabel, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_LABEL_KEYPOS, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
		{value, {ADCRes, _Multiplier, AdcResLabel}} ->
			{ok, ADCRes};
		_->
			{error, "Invalid ADC resolution"}
	end.

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

%% ====================================================================
%% @doc
%% Gives the default configuration record, what contains the default values of
%% each attributes.
-spec get_default_cfg() -> {ok, tuple()}.
%% @end
%% ====================================================================
get_default_cfg() ->
	DefCfgReg = #mcp980xConfigReg{},
	{ok, DefCfgReg}.


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
	DefCfgReg = #mcp980xConfigReg{},
	
	OneShot = (DefCfgReg#mcp980xConfigReg.bit_OneShot)#bitParam.defValue,
	AdcRes = (DefCfgReg#mcp980xConfigReg.bit_AdcResolution)#bitParam.defValue,
	FaultQueue = (DefCfgReg#mcp980xConfigReg.bit_FaultQueue)#bitParam.defValue,
	AlertPol = (DefCfgReg#mcp980xConfigReg.bit_AlertPol)#bitParam.defValue,
	Mode = (DefCfgReg#mcp980xConfigReg.bit_Mode)#bitParam.defValue,
	Shutdown = (DefCfgReg#mcp980xConfigReg.bit_ShutDown)#bitParam.defValue,
	
	DefTemperatureHystReg = #mcp980xTemperatureHystReg{},
	TempHyst = case (DefTemperatureHystReg#mcp980xTemperatureHystReg.bit_Sign)#bitParam.defValue of
				   ?MCP980x_TEMP_SIGN_MINUS ->
					   ((-1) * (DefTemperatureHystReg#mcp980xTemperatureHystReg.bit_TempValue)#bitParam.defValue);
				   
				   ?MCP980x_TEMP_SIGN_PLUS ->
					   (DefTemperatureHystReg#mcp980xTemperatureHystReg.bit_TempValue)#bitParam.defValue
			   end,
	
	DefTemperatureLimitSetReg = #mcp980xTemperatureLimitSetReg{},
	TempLimit = case (DefTemperatureLimitSetReg#mcp980xTemperatureLimitSetReg.bit_Sign)#bitParam.defValue of
					?MCP980x_TEMP_SIGN_MINUS ->
						((-1) * (DefTemperatureLimitSetReg#mcp980xTemperatureLimitSetReg.bit_TempValue)#bitParam.defValue);
					
					?MCP980x_TEMP_SIGN_PLUS ->
						(DefTemperatureLimitSetReg#mcp980xTemperatureLimitSetReg.bit_TempValue)#bitParam.defValue
				end,
	
	init([HwAddress, OneShot, AdcRes, FaultQueue, AlertPol, Mode, Shutdown, TempLimit, TempHyst]);

init([HwAddress, AdcRes, FaultQueue, AlertPol, Mode, TempLimit, TempHyst]) ->
	DefCfgReg = #mcp980xConfigReg{},
	OneShot = (DefCfgReg#mcp980xConfigReg.bit_OneShot)#bitParam.defValue,
	Shutdown = (DefCfgReg#mcp980xConfigReg.bit_ShutDown)#bitParam.defValue,
	init([HwAddress, OneShot, AdcRes, FaultQueue, AlertPol, Mode, Shutdown, TempLimit, TempHyst]);

init([HwAddress, OneShot, AdcRes, FaultQueue, AlertPol, Mode, Shutdown, TempLimit, TempHyst]) ->
	%% Configure default setup in the device.
	R1 = do_set_oneshot_cfg_bit(HwAddress, OneShot),
	R2 = do_set_adcres_cfg_bit(HwAddress,  AdcRes),
	R3 = do_set_faultqueue_cfg_bit(HwAddress,  FaultQueue),
	R4 = do_set_alertpol_cfg_bit(HwAddress,  AlertPol),
	R5 = do_set_mode_cfg_bit(HwAddress,  Mode),
	R6 = do_set_shutdown_cfg_bit(HwAddress,  Shutdown),
	R7 = do_set_temperature_hysteresis_register(HwAddress, TempHyst),
	R8 = do_set_temperature_limit_set_register(HwAddress, TempLimit),

	case lists:usort([R1, R2, R3, R4, R5, R6, R7, R8]) of
		[ok] ->
			?DO_INFO("Temperature device has been configured with default value.",[]),
			ok;
		ER ->
			%% At least one bit was not able t oset
			?DO_ERR("Failed to configure temperature sensor with default value.",
					[{error, ER}])
	end,
	
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
	do_set_temperature_limit_register(HwAddress, TemperatureLimit, 
									  #mcp980xTemperatureLimitSetReg.length, 
									  #mcp980xTemperatureLimitSetReg{}, 
									  #mcp980xTemperatureLimitSetReg.address, 
									  #mcp980xTemperatureLimitSetReg.bit_Sign,
									  #mcp980xTemperatureLimitSetReg.bit_TempValue).

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
	do_get_temperature_limit_register(HwAddress, #mcp980xTemperatureLimitSetReg.length, 
									  #mcp980xTemperatureLimitSetReg{}, 
									  #mcp980xTemperatureLimitSetReg.address, 
									  #mcp980xTemperatureLimitSetReg.bit_Sign, 
									  #mcp980xTemperatureLimitSetReg.bit_TempValue).

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
	do_set_temperature_limit_register(HwAddress, TempHystValue, 
									  #mcp980xTemperatureHystReg.length, 
									  #mcp980xTemperatureHystReg{}, 
									  #mcp980xTemperatureHystReg.address, 
									  #mcp980xTemperatureHystReg.bit_Sign,
									  #mcp980xTemperatureHystReg.bit_TempValue).

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
	do_get_temperature_limit_register(HwAddress, #mcp980xTemperatureHystReg.length, 
									  #mcp980xTemperatureHystReg{}, 
									  #mcp980xTemperatureHystReg.address, 
									  #mcp980xTemperatureHystReg.bit_Sign, 
									  #mcp980xTemperatureHystReg.bit_TempValue).

%% ====================================================================
%% @doc
%% Set Temperature Limit-Set register.
%% Note: The register contains the 9 bit data in two's complement,
%%       So get/set this register can accepts the 9 bit resolution
%%       values. All other values will be rejected.
%% @end
-spec do_set_temperature_limit_register(
		HwAddress :: integer(),
		TemperatureLimit :: float(),
		DataLengthIdx :: integer(),
		RegisterRec :: tuple(),
		RegisterAddressIdx :: integer(),
		TempSignBitFieldIdx :: integer(),
		TempValueBitFieldIdx :: integer()) -> ok | {error, term()}.
%% ====================================================================
do_set_temperature_limit_register(HwAddress, TemperatureLimit, DataLengthIdx, RegisterRec, RegisterAddressIdx, TempSignBitFieldIdx, TempValueBitFieldIdx) ->
	%% Calculate the integer and fractionation part of the TemperatureLimit.
	%% To compute these values can be done by this formula: Code = TemperatureLimit / Multiplier_of_9bit_resolution.
	%% If no reminder of the result of division, the given TemperatureLimit value is valid, oherwise it does not.
	ADCRes = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
	case lists:keysearch(ADCRes, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_ID_KEYPOS, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
		{value, {ADCRes, Multiplier, _Label}} ->
			Code = TemperatureLimit / Multiplier,
			
			case (Code - trunc(Code)) == 0 of
				true ->
					%% No reminder of the division, the given temperature limit is valid.
					case get_temperature_sign(TemperatureLimit) of
						{ok, TempSign} ->
							case dev_common:bitfield_set(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, 
														 RegisterRec,
														 DataLengthIdx,
														 RegisterAddressIdx, 
														 TempSignBitFieldIdx, TempSign) of
								{ok, _} ->
									case dev_common:bitfield_set(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, 
																 RegisterRec,
																 DataLengthIdx,
																 RegisterAddressIdx,
																 TempValueBitFieldIdx, round(Code)) of
										{ok, _} ->
											ok;
										
										ER->ER
									end;
								ER -> ER
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
-spec do_get_temperature_limit_register(
		HwAddress :: integer(),
		DataLengthIdx :: integer(),
		RegisterRec :: tuple(),
		RegisterAddressIdx :: integer(),
		TempSignBitFieldIdx :: integer(),
		TempValueBitFieldIdx :: integer()) -> {ok, TemperatureLimit :: float()} | {error, term()}.
%% ====================================================================
do_get_temperature_limit_register(HwAddress, DataLengthIdx, RegisterRec, RegisterAddressIdx, TempSignBitFieldIdx, TempValueBitFieldIdx) ->
	case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress,
								 RegisterRec, 
								 DataLengthIdx,
								 {addrIdx, RegisterAddressIdx}, 
								 [TempSignBitFieldIdx, 
								  TempValueBitFieldIdx]) of
		
		[{TempSignBitFieldIdx, TempSign}, 
		 {TempValueBitFieldIdx, TempValue}] ->
			%% The 2 byte temperature register has been read. Now the temperature value needs to be compute from this value.
			ADCRes = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
			RegisterLength = erlang:element(DataLengthIdx, RegisterRec),
			TempSignMask = (erlang:element(TempSignBitFieldIdx, RegisterRec))#bitParam.mask,
			TempValueMask = (erlang:element(TempValueBitFieldIdx, RegisterRec))#bitParam.mask, 
			compute_temperature(ADCRes, RegisterLength, TempSign, TempSignMask, TempValue, TempValueMask);
		
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
			case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress,
										 RegisterRec, 
										 #mcp980xAmbientTemperatureReg.length,
										 {addrIdx, #mcp980xAmbientTemperatureReg.address}, 
										 [#mcp980xAmbientTemperatureReg.bit_Sign, 
										  #mcp980xAmbientTemperatureReg.bit_TempValue]) of
				
				[{#mcp980xAmbientTemperatureReg.bit_Sign, TempSign}, 
				 {#mcp980xAmbientTemperatureReg.bit_TempValue, TempValue}] ->
					%% The 2 byte temperature register has been read. Now the temperature value needs to be compute from this value.
					RegisterLength = erlang:element(#mcp980xAmbientTemperatureReg.length, #mcp980xAmbientTemperatureReg{}),
					TempSignMask = (erlang:element(#mcp980xAmbientTemperatureReg.bit_Sign, #mcp980xTemperatureHystReg{}))#bitParam.mask,
					TempValueMask = (erlang:element(#mcp980xAmbientTemperatureReg.bit_TempValue, #mcp980xAmbientTemperatureReg{}))#bitParam.mask, 
					compute_temperature(ADCRes, RegisterLength, TempSign, TempSignMask, TempValue, TempValueMask);
				
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
-spec do_set_adcres_cfg_bit(HwAddress :: integer(), AdcResolutionCfgBit :: adcres_cfg_bit() | string()) -> ok | {error, term()}.
%% @end
%% ====================================================================
do_set_adcres_cfg_bit(HwAddress, AdcResolutionCfgBitLabel) when is_list(AdcResolutionCfgBitLabel)->
	case get_adcres_cfg_bit(AdcResolutionCfgBitLabel) of
		{ok, AdcResolutionCfgBit} ->
			do_set_adcres_cfg_bit(HwAddress, AdcResolutionCfgBit);
		ER -> ER
	end;
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

	case dev_common:bitfield_get(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress, 
								 #mcp980xConfigReg{}, 
								 #mcp980xConfigReg.length,
								 {addrIdx, #mcp980xConfigReg.address}, 
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
	do_set_cfg_bit_loop(HwAddress, CfgData).

do_set_cfg_bit_loop(_HwAddress, []) ->
	ok;
do_set_cfg_bit_loop(HwAddress, [{CfgBitIdx, CfgBit} | T]) ->
	%% Set cfg bit in the device
	case dev_common:bitfield_set(?MCP980X_COMMUNICATION_DEVICENAME, HwAddress,
								 #mcp980xConfigReg{}, #mcp980xConfigReg.length, #mcp980xConfigReg.address,
								 CfgBitIdx, 
								 CfgBit) of
		{ok,_} ->
			do_set_cfg_bit_loop(HwAddress, T);
		ER->
			?DO_ERR("Failed to read/set cfg bit", [{reason, ER}]),
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
		RegisterLength :: integer(),
		TempSign :: temperature_sing(),
		TempSignMask :: integer(),
		TempValue :: integer(),
		TempValueMask :: integer()) -> {ok, TemperatureValue :: float()} | {error, term()}.
%% @end
%% ====================================================================
compute_temperature(ADCRes, RegisterLength, TempSign, TempSignMask, TempValue, TempValueMask) ->
	%% The TemperatureRegValue is a 16 bit wide value, what contains the upper and lower part of the temperature value.
	%% The formula to compute the temperature value is this:
	%% Ta = (Code * Multiplier), where
	%% 					Code is the value of 16 bit temp. register shifted to most left
	%%					Multiplier is the ADC Res. value. Not the bit value, but the associated resulution value. This can be take in
	%%					?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST define.
	
	TempSignMaskBitLength = bit_operations:byte_count_bit(TempSignMask, bit_operations:byte_get_max_value(RegisterLength), 16, 1),
	TempValueMaskBitLength = bit_operations:byte_count_bit(TempValueMask, bit_operations:byte_get_max_value(RegisterLength), 16, 1),
	
	case lists:keyfind(ADCRes, 1, ?CONFIGURATION_REG_ADC_RESOLUTION_BIT_LENGHT) of
		{ADCRes, ADCResBitLength} ->
			ShiftRight = TempValueMaskBitLength-(ADCResBitLength-TempSignMaskBitLength),
			
			%% Shift right the register value with N bit. The number of bit depends of the ADC res. value.
			Code = TempValue bsr ShiftRight,
			case lists:keysearch(ADCRes, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_ID_KEYPOS, ?CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST) of
				{value, {ADCRes, Multiplier, _Label}} ->
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
			end;
		false ->
			{error, {"Invalid ADC resolution", ADCRes}}
	end.

