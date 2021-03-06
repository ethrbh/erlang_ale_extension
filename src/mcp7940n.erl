%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module provides interface to able to control MCP7940n RTC device.
%% @end

-module(mcp7940n).
-behaviour(gen_server).

-export([start/3, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("dev_common.hrl").
-include("mcp7940n.hrl").
-include("ale_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(SERVER, ?MODULE).
-define(GEN_SERVER_CALL_TO, 5000).
-define(TIME_TO_WAIT_BEFORE_CHECK_OSCILLATOR_STATUS, 500).
-define(NUMBER_OF_BYTE_TO_READ, 1).

%% Use below method to get current time stamp in tuple if Erlang/OTP version is
%% 18.x or higher.
%%-define(GET_NOW, erlang:timestamp()).

%% Use below method to get current time stamp in tuple if Erlang/OTP version is
%% older than 18.0
-define(GET_NOW, erlang:timestamp()).

%% ====================================================================
%% API functions
%% ====================================================================

%% ====================================================================
%% Week day name/integer id conversation
%% ====================================================================
-export([get_week_day_name/1,
		 get_week_day_id/1]).

%% ====================================================================
%% Control register setting
%% ====================================================================
-export([ctrl_bit_out_set/1,
		 ctrl_bit_sqwen_set/1,
		 ctrl_bit_extosc_set/1,
		 ctrl_bit_crstrim_set/1,
		 ctrl_bit_sqwfs_set/1]).

%% ====================================================================
%% Power failure related functions
%% ====================================================================
-export([pwrfail_bit_read/0, 
		 pwrfail_bit_clear/0,
		 pwr_down_date_and_time_get/0,
		 pwr_up_date_and_time_get/0,
		 pwr_status_change_subscribe/0, pwr_status_change_subscribe/1, 
		 pwr_status_change_unsubscribe/1]).

%% ====================================================================
%% Battery backup related functions
%% ====================================================================
-export([vbaten_read/0,vbaten_set/1]).

%% ====================================================================
%% Alarm related functions
%% ====================================================================
-export([alarm_interrupt_enable_disable/2,
		 alarm_interrupt_is_enabled/1,
		 alarm_interrupt_flag_clear/1, 
		 alarm_interrupt_flag_check/1,
		 alarm_interrupt_flag_check/0,
		 alarm_configure/5,
		 alarm_date_and_time_get/1]).

%% ====================================================================
%% Oscillator related functions
%% ====================================================================
-export([oscillator_is_running/0,
		 oscillator_is_started/0,
		 oscillator_start/0, 
		 oscillator_stop/0,
		 oscillator_status_change_subscribe/0, oscillator_status_change_subscribe/1,
		 oscillator_status_change_unsubscribe/1]).

%% ====================================================================
%% RTC Date And Time related functions
%% ====================================================================
-export([date_and_time_set/1, 
		 date_and_time_get/0,
		 date_and_time_get_in_str/0, date_and_time_get_in_str/1,
		 date_get_in_str/1, time_get_in_str/1
		]).

%% ====================================================================
%% Functions for read/write any register in RTC device.
%% ====================================================================
-export([read_register/1,
		 write_register/2]).

%% ====================================================================
%% Internal exports. This is needed for elimante compilation warnings,
%% for functions what are executed with erlang:apply/3 only.
%% DO NOT USE THESE DIRECTRLY!!!
%% ====================================================================
-export([
%% ====================================================================
%% Control register setting
%% ====================================================================
		 do_ctrl_bit_sqwfs_set/1,
		 do_ctrl_bit_crstrim_set/1,
		 do_ctrl_bit_extosc_set/1,
		 do_ctrl_bit_sqwen_set/1,
		 do_ctrl_bit_out_set/1,
		 
%% ====================================================================
%% Power failure related functions
%% ====================================================================
		 do_pwrfail_bit_read/1,
		 do_pwrfail_bit_clear/0,
		 do_pwr_down_date_and_time_get/0,
		 do_pwr_up_date_and_time_get/0,
		 
%% ====================================================================
%% Battery backup related functions
%% ====================================================================
		 do_vbaten_read/0,
		 do_vbaten_set/1,
		 
%% ====================================================================
%% Alarm related functions
%% ====================================================================
		 do_alarm_mask_set/2,
		 do_alarm_interrupt_enable_disable/2,
		 do_alarm_interrupt_enable/1,
		 do_alarm_interrupt_disable/1,
		 do_alarm_interrupt_is_enabled/1,
		 do_alarm_interrupt_flag_clear/1,
		 do_alarm_interrupt_flag_check/1,
		 do_alarm_interrupt_flag_check/0,
		 do_alarm_interrupt_out_pol_set/2,
		 do_alarm_configure/5,
		 do_alarm_date_and_time_get/1,
		 
%% ====================================================================
%% Oscillator related functions
%% ====================================================================
		 do_oscillator_is_running/0,
		 do_oscillator_is_started/0,
		 do_oscillator_start/0,
		 do_oscillator_stop/0,
		 
%% ====================================================================
%% RTC Date And Time related functions
%% ====================================================================
		 do_date_and_time_get/0,
		 do_date_and_time_get_in_str/0, do_date_and_time_get_in_str/1,
		 do_date_get_in_str/1, do_time_get_in_str/1,
		 do_year_get/1,
		 do_month_get/1,
		 do_date_get/1,
		 do_hour_get/1,
		 do_minute_get/1,
		 do_second_get/1,
		 do_wday_get/1,
		 
		 do_date_and_time_set/1,
		 do_year_set/2,
		 do_month_set/2,
		 do_date_set/2,
		 do_hour_set/2,
		 do_minute_set/2,
		 do_second_set/2,
		 do_wday_set/2

		 ]).

-export([hour_convert_to_12h_format/1, hour_convert_to_24h_format/2]).

-define(PWR_STATUS_CHECK_INTERVAL,			1000).	%% Time interval in [msec], for check PWR status.
-define(OSCILLATOR_STATUS_CHECK_INTERVAL,	1000).	%% Time interval in [msec], for check OSCILLATOR status.

-record(state, {
				pwrStatusNotificationPidList = [],	%% List of pids, who has been ordered for notifications.
				pwrStatusCheckIntervalTref,			%% Timer reference for the timer to trigger PWR checking periodicaly.
				pwrStatusLastCheckTime,				%% The last check time of PWR status. It is ?GET_NOW.
				pwrStatus,							%% rtc_pwrfail()
				
				oscStatusNotificationPidList = [],	%% List of pids, who has been ordered for notifications.
				oscStatusCheckIntervalTref,			%% Timer reference for the timer to trigger OSCILLATOR checking periodicaly.
				oscStatusLastCheckTime,				%% The last check time of OSCILLATOR status. It is ?GET_NOW.
				oscStatus							%% rtc_oscrun()
				}).

%% ====================================================================
%% @doc
%% Start driver.
%% @end
-spec start(time_format(), rtc_vbaten(), boolean()) -> {ok, pid()} | {error, term()}.
%% ====================================================================
start(TimeFormat, VBatEn, ForceToSetDateAndTime) ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Already started
			{ok, Pid};
		_->	%% The supervisor does not started yet.
			gen_server:start({local, ?SERVER}, ?MODULE, [TimeFormat, VBatEn, ForceToSetDateAndTime], [{timeout, 5000}]) 
	end.

%% ====================================================================
%% @doc Stop driver.
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
%% Get the name of the week day by its week day integer id
-spec get_week_day_name(wday()) -> {ok, atom()} | {error, term()}.
%% ====================================================================
get_week_day_name(WDay) when WDay >= ?RTC_WDAY_MIN, WDay =< ?RTC_WDAY_MAX ->
	{ok, lists:nth(WDay, ?RTC_DAY_NAME_LIST)};
get_week_day_name(_WDay) ->
	{error, "Invalid wday number"}.
	
%% ====================================================================
%% Get the integer number of the week day by its name
-spec get_week_day_id(atom()) -> {ok, wday()} | {error, term()}.
%% ====================================================================
get_week_day_id(WDayName) ->
	case lists:member(WDayName, ?RTC_DAY_NAME_LIST) of
		true ->
			L = lists:zip(lists:seq(?RTC_WDAY_MIN, ?RTC_WDAY_MAX), ?RTC_DAY_NAME_LIST),
			{value, {WDay, WDayName}} = lists:keysearch(WDayName, 2, L),
			{ok, WDay};
		
		_->	{error, "Invalid wday name"}
	end.

%% ====================================================================
%% @doc
%% Configure CONTROL REG. OUT bit
%% @end
-spec ctrl_bit_out_set(rtc_ctrl_bit_out()) -> ok | {error, term()}.
%% ====================================================================
ctrl_bit_out_set(CtrlBitOut) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_ctrl_bit_out_set,[CtrlBitOut]}}).

%% ====================================================================
%% @doc
%% Configure CONTROL REG. SQWEN bit
%% @end
-spec ctrl_bit_sqwen_set(rtc_ctrl_bit_sqwen()) -> ok | {error, term()}.
%% ====================================================================
ctrl_bit_sqwen_set(Sqwen) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_ctrl_bit_sqwen_set,[Sqwen]}}).

%% ====================================================================
%% @doc
%% Configure CONTROL REG. EXTOSC bit
%% @end
-spec ctrl_bit_extosc_set(rtc_ctrl_bit_extosc()) -> ok | {error, term()}.
%% ====================================================================
ctrl_bit_extosc_set(Extosc) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_ctrl_bit_extosc_set,[Extosc]}}).

%% ====================================================================
%% @doc
%% Configure CONTROL REG. CRSTRIM bit
%% @end
-spec ctrl_bit_crstrim_set(rtc_ctrl_bit_crstrim()) -> ok | {error, term()}.
%% ====================================================================
ctrl_bit_crstrim_set(Crstrim) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_ctrl_bit_crstrim_set,[Crstrim]}}).

%% ====================================================================
%% @doc
%% Configure CONTROL REG. SQWFS bit
%% @end
-spec ctrl_bit_sqwfs_set(rtc_ctrl_bit_sqwfs()) -> ok | {error, term()}.
%% ====================================================================
ctrl_bit_sqwfs_set(Sqwfs) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_ctrl_bit_sqwfs_set,[Sqwfs]}}).

%% ====================================================================
%% @doc
%% Read PWRFAIL bit
%% @end
-spec pwrfail_bit_read() -> {ok, rtc_pwrfail()} | {error, term()}.
%% ====================================================================
pwrfail_bit_read() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_pwrfail_bit_read,[true]}}).

%% ====================================================================
%% @doc
%% Clear PWRFAIL bit
%% @end
-spec pwrfail_bit_clear() -> ok | {error, term()}.
%% ====================================================================
pwrfail_bit_clear() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_pwrfail_bit_clear,[]}}).

%% ====================================================================
%% @doc
%% Read PWR DOWN Date and Time in RTC device.
%% @end
-spec pwr_down_date_and_time_get() -> {ok, datetime()} | {error, term()}.
%% ====================================================================
pwr_down_date_and_time_get() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_pwr_down_date_and_time_get,[]}}).

%% ====================================================================
%% @doc
%% Read PWR UP Date and Time in RTC device.
%% @end
-spec pwr_up_date_and_time_get() -> {ok, datetime()} | {error, term()}.
%% ====================================================================
pwr_up_date_and_time_get() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_pwr_up_date_and_time_get,[]}}).

%% ====================================================================
%% @doc
%% Subscribe to PWR DOWN/UP events.
%% @end
-spec pwr_status_change_subscribe() -> ok | {error, term()}.
%% ====================================================================
pwr_status_change_subscribe() ->
	pwr_status_change_subscribe(self()).

%% ====================================================================
%% @doc
%% Subscribe to PWR DOWN/UP events.
%% @end
-spec pwr_status_change_subscribe(pid()) -> ok | {error, term()}.
%% ====================================================================
pwr_status_change_subscribe(PidToSendNotification) ->
	do_gen_server_call({pwr_status_change_subscribe, PidToSendNotification}).

%% ====================================================================
%% @doc
%% Unsubscribe to PWR DOWN/UP events.
%% @end
-spec pwr_status_change_unsubscribe(pid()) -> ok | {error, term()}.
%% ====================================================================
pwr_status_change_unsubscribe(PidToSendNotification) ->
	do_gen_server_call({pwr_status_change_unsubscribe, PidToSendNotification}).

%% ====================================================================
%% @doc
%% Read VBATEN bit
%% @end
-spec vbaten_read() -> {ok, rtc_vbaten()} | {error, term()}.
%% ====================================================================
vbaten_read() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_vbaten_read,[]}}).

%% ====================================================================
%% @doc
%% Set VBATEN bit
%% @end
-spec vbaten_set(rtc_vbaten()) -> ok | {error, term()}.
%% ====================================================================
vbaten_set(Vbaten) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_vbaten_set,[Vbaten]}}).

%% ====================================================================
%% @doc
%% Enable/Disable Alarm interrupt.
%% @end
-spec alarm_interrupt_enable_disable(rtc_alarm_id(),
                                     rtc_alarm_interrupt_en_status()) -> 
                                        ok  | {error, term()}.
%% ====================================================================
alarm_interrupt_enable_disable(AlarmId, AlarmInterruptEnableStatus) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_interrupt_enable_disable,[AlarmId, AlarmInterruptEnableStatus]}}).

%% ====================================================================
%% @doc
%% Read Alarm interrupt enabled bit.
%% @end
-spec alarm_interrupt_is_enabled(rtc_alarm_id()) -> {ok, rtc_alarm_interrupt_en_status()} | {error, term()}.
%% ====================================================================
alarm_interrupt_is_enabled(AlarmId) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_interrupt_is_enabled,[AlarmId]}}).

%% ====================================================================
%% @doc
%% Clear interrupt flag bit.
%% @end
-spec alarm_interrupt_flag_clear(rtc_alarm_id()) -> ok | {error, term()}.
%% ====================================================================
alarm_interrupt_flag_clear(AlarmId) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_interrupt_flag_clear,[AlarmId]}}).

%% ====================================================================
%% @doc
%% Check interrupt flag bit.
%% @end
-spec alarm_interrupt_flag_check(rtc_alarm_id()) -> {ok, rtc_alarm_interrupt_set()} | {ok, rtc_alarm_interrupt_clear()} | {error, term()}.
%% ====================================================================
alarm_interrupt_flag_check(AlarmId) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_interrupt_flag_check,[AlarmId]}}).

%% ====================================================================
%% @doc
%% Check interrupt flag status for all AlarmModules
%% @end
-spec alarm_interrupt_flag_check() -> {ok, list({rtc_alarm_id(), rtc_alarm_interrupt_set() | rtc_alarm_interrupt_clear()})} | {error, term()}.
%% ====================================================================
alarm_interrupt_flag_check() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_interrupt_flag_check,[]}}).

%% ====================================================================
%% @doc
%% Configure alarm.
%% NOTE: YEAR will be ignored in datetime(), because this attribute does not implemented in Alarm module in RTC,
%%		 but it should be set for keep the same logic as date_and_time_set/1 has.
%% @end
-spec alarm_configure(rtc_alarm_id(), datetime(), rtc_alarm_mask(), rtc_alarm_interrupt_en_status(), rtc_alarm_interrupt_out_pol()) -> ok | {error, term()}.
%% ====================================================================
alarm_configure(AlarmId, DateAndTime, Mask, InterruptEnStatus, InterruptOutPol) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_configure,[AlarmId, DateAndTime, Mask, InterruptEnStatus, InterruptOutPol]}}).

%% ====================================================================
%% @doc
%% Read the configured alarm date and time settings.
%% @end
-spec alarm_date_and_time_get(rtc_alarm_id()) -> {ok, datetime()} | {error, term()}.
%% ====================================================================
alarm_date_and_time_get(AlarmId) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_alarm_date_and_time_get,[AlarmId]}}).

%% ====================================================================
%% @doc
%% Read OSCRUN bit- This tells that oscillator is running or not.
%% @end
-spec oscillator_is_running() -> {ok, rtc_oscrun()} | {error, term()}.
%% ====================================================================
oscillator_is_running() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_oscillator_is_running,[]}}).

%% ====================================================================
%% @doc
%% Read ST bit- This tells that oscillator is started or not.
%% @end
-spec oscillator_is_started() -> {ok, rtc_osc_start()} | {error, term()}.
%% ====================================================================
oscillator_is_started() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_oscillator_is_started,[]}}).

%% ====================================================================
%% @doc
%% Start Oscillator.
%% @end
-spec oscillator_start() -> ok | {error, term()}.
%% ====================================================================
oscillator_start() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_oscillator_start,[]}}).

%% ====================================================================
%% @doc
%% Stop Oscillator.
%% @end
-spec oscillator_stop() -> ok | {error, term()}.
%% ====================================================================
oscillator_stop() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_oscillator_stop,[]}}).

%% ====================================================================
%% @doc
%% Subscribe to OSCILLATOR status change events.
%% @end
-spec oscillator_status_change_subscribe() -> ok | {error, term()}.
%% ====================================================================
oscillator_status_change_subscribe() ->
	oscillator_status_change_subscribe(self()).

%% ====================================================================
%% @doc
%% Subscribe to OSCILLATOR status change events.
%% @end
-spec oscillator_status_change_subscribe(pid()) -> ok | {error, term()}.
%% ====================================================================
oscillator_status_change_subscribe(PidToSendNotification) ->
	do_gen_server_call({oscillator_status_change_subscribe, PidToSendNotification}).

%% ====================================================================
%% @doc
%% Unsubscribe to OSCILLATOR status change events.
%% @end
-spec oscillator_status_change_unsubscribe(pid()) -> ok | {error, term()}.
%% ====================================================================
oscillator_status_change_unsubscribe(PidToSendNotification) ->
	do_gen_server_call({oscillator_status_change_unsubscribe, PidToSendNotification}).

%% ====================================================================
%% @doc
%% Setup Date and Time in RTC device.
%% @end
-spec date_and_time_set(datetime() | time_format()) -> ok | {error, term()}.
%% ====================================================================
date_and_time_set(DateTimeOrTimeFormat) ->
	do_gen_server_call({execute_mfa, {?MODULE, do_date_and_time_set,[DateTimeOrTimeFormat]}}).

%% ====================================================================
%% @doc
%% Read Date and Time in RTC device.
%% @end
-spec date_and_time_get() -> {ok, datetime()} | {error, term()}.
%% ====================================================================
date_and_time_get() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_date_and_time_get,[]}}).

%% ====================================================================
%% @doc
%% Read Date and Time in RTC device and convert it to string.
%% The timestamp string loks liek this: 2015-09-09 10:03:54
%% @end
-spec date_and_time_get_in_str() -> {ok, list()} | {error, term()}.
%% ====================================================================
date_and_time_get_in_str() ->
	do_gen_server_call({execute_mfa, {?MODULE, do_date_and_time_get_in_str,[]}}).

%% ====================================================================
%% @doc
%% Read Date and Time in RTC device and convert it to string.
%% The timestamp string looks liek this: 2015-09-09 10:03:54
%% @end
-spec date_and_time_get_in_str(datetime() | {ok, datetime()}) -> {ok, list()} | {error, term()}.
%% ====================================================================
date_and_time_get_in_str(DateAndTime) ->
	%%do_gen_server_call({execute_mfa, {?MODULE, do_date_and_time_get_in_str,[DateAndTime]}}).
	do_date_and_time_get_in_str(DateAndTime).

%% ====================================================================
%% @doc
%% Convert date to string.
%% The date string looks liek this: 2015-09-09
%% @end
-spec date_get_in_str(date() | {ok, date()}) -> {ok, list()} | {error, term()}.
%% ====================================================================
date_get_in_str(Date) ->
	do_date_get_in_str(Date).

%% ====================================================================
%% @doc
%% Convert time to string.
%% The date string looks liek this: 10:03:54
%% @end
-spec time_get_in_str(time() | {ok, time()}) -> {ok, list()} | {error, term()}.
%% ====================================================================
time_get_in_str(Time) ->
	do_time_get_in_str(Time).

%% ====================================================================
%% @doc
%% Read register
%% @end
-spec read_register(address()) -> {ok, int_data()} | {error, term()}. 
%% ====================================================================
read_register(Addr) ->
	read(Addr).

%% ====================================================================
%% @doc
%% Write register
%% @end
-spec write_register(address(), int_data()) -> ok | {error, term()}.
%% ====================================================================
write_register(Addr, Data) ->
	write(Addr, Data).

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
init([TimeFormat, Vbaten, ForceToSetDateAndTime]) ->
	case do_vbaten_read() of
		{ok, ?RTC_WKDAY_BIT_VBATEN_EN} ->
			%% Read PWRFAIL bit and decide full DATE and TIME configuration is needed or not.
			case do_pwrfail_bit_read(true) of
				?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST ->
					?DO_INFO("Power failure status", [{pwrfail, {?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST, "Primary power was lost"}}]),
					
					%% Clear PWRFAIL bit
					do_pwrfail_bit_clear(),
					
					%% Recunfigure DateAndTime. Good to know, that Erlang/OTP can serve time in 24H format.
					%% So if 12H format is the selected what RTC should use, the time should be prepared here before
					%% set in the device.
					do_init_set_date_and_time(TimeFormat);

				_->	
					case ForceToSetDateAndTime of
						true ->
							do_init_set_date_and_time(TimeFormat);
						_->	%% No need reconfigure Date and Time in RTC.
							ok
					end
			end,
			
			do_init(),
			
			%% Start timer for  check Main Power of RTC device
			{ok, TRef} = timer:send_interval(?PWR_STATUS_CHECK_INTERVAL, self(), {pwr_status_check}),

			?DO_INFO("RTC has been started", []),
			
			{ok, #state{pwrStatusCheckIntervalTref = TRef}};
		
		{ok, ?RTC_WKDAY_BIT_VBATEN_DIS} ->
			%% Configure the current Date and Time anyway.

			%% Recunfigure DateAndTime. Good to know, that Erlang/OTP can serve time in 24H format.
			%% So if 12H format is the selected what RTC should use, the time should be prepared here before
			%% set in the device.
			do_init_set_date_and_time(TimeFormat),
			
			%% Configure VBATEN
			do_vbaten_set(Vbaten),
			
			do_init(),
			
			%% Start timer for  check Main Power of RTC device
			{ok, TRef} = timer:send_interval(?PWR_STATUS_CHECK_INTERVAL, self(), {pwr_status_check}),
			
			?DO_INFO("RTC has been started", []),
			
			{ok, #state{pwrStatusCheckIntervalTref = TRef}};
		
		ER ->
			?DO_ERR("Faild to do_init RTC device.", [{reason, ER}]),
			{stop, ER}
	end.

do_init() ->
	%% Clear interrupt flags.
	do_alarm_interrupt_flag_clear(?RTC_ALARM_0_ID),
	do_alarm_interrupt_flag_clear(?RTC_ALARM_1_ID),
	
	%% Disable interrupt for both alarms.
	do_alarm_interrupt_disable(?RTC_ALARM_0_ID),
	do_alarm_interrupt_disable(?RTC_ALARM_1_ID),
	
	%% Start RTC oscillator
	do_oscillator_start(),
	
	%% Wait x msec after start oscillator, but before check its status.
	timer:sleep(?TIME_TO_WAIT_BEFORE_CHECK_OSCILLATOR_STATUS),
	
	%% Stay in the loop until oscillator is not running.
	do_init_loop().

do_init_loop() ->
	%% Check OSCRUN bit in a loop, and exit when it is SET
	case do_oscillator_is_running() of
		{ok, ?RTC_WKDAY_BIT_OSCRUN_EN} ->
			ok;
		_->	%% Oscillator is NOT yet running, stay in the loop.
			do_init_loop()
	end.

do_init_set_date_and_time(TimeFormat) ->
	DateAndTime = calendar:local_time(),
	
	DateAndTimeNew = case TimeFormat of
						 ?RTC_HOUR_BIT_TIME_FORMAT_12H ->
							 {Date, Time} = DateAndTime,
							 {Hour,Minute,Second} = Time,
							 
							 %% Convert 24H format hour to 12H format.
							 Hour12 = hour_convert_to_12h_format(Hour),
							 
							 {Date, {Hour12, Minute, Second}};
						 _-> DateAndTime
					 end,

	%% Configure DateAndTime in RTC
	do_date_and_time_set(DateAndTimeNew),
	
	%% Configure CONTROL register
	do_ctrl_bit_out_set(?RTC_CTRL_BIT_OUT_MFP_LOW),
	do_ctrl_bit_sqwen_set(?RTC_CTRL_BIT_SQWEN_DIS),
	do_ctrl_bit_extosc_set(?RTC_CTRL_BIT_EXTOSC_DIS),
	do_ctrl_bit_crstrim_set(?RTC_CTRL_BIT_CRSTRIM_DIS),
	do_ctrl_bit_sqwfs_set(?RTC_CTRL_BIT_SQWFS_1Hz).

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
handle_call({execute_mfa, {M,F,A}}, _From, State)->
    Reply = case catch erlang:apply(M, F, A) of
                {'EXIT', ER} ->
                    {error, ER};
                OtherResult ->
                    OtherResult
            end,
	{reply, Reply, State};

handle_call({pwr_status_change_subscribe, PidToSendNotification}, _From, State) ->
	%% Check that PidToSendNotification is in the list or not.
	case lists:member(PidToSendNotification, State#state.pwrStatusNotificationPidList) of
		true ->
			%% Pid is already subscribed to the PWR change notification.
			?DO_ERR("Pid is already subscribed to the PWR change notification", [{pid, PidToSendNotification}]),
			{reply, {error, {pid_already_subscribed_to_pwr_change_notification, PidToSendNotification}}, State};
		false ->
			%% Insert Pid into the list
			?DO_INFO("Pid has been subscribed to the PWR change notification", [{pid, PidToSendNotification}]),
			
			NewPwrStatusNotificationPidList = lists:append(State#state.pwrStatusNotificationPidList, [PidToSendNotification]),
			{reply, ok, State#state {pwrStatusNotificationPidList = NewPwrStatusNotificationPidList}}
	end;

handle_call({pwr_status_change_unsubscribe, PidToSendNotification}, _From, State) ->
	case lists:member(PidToSendNotification, State#state.pwrStatusNotificationPidList) of
		true ->
			?DO_INFO("Pid has been unsubscribed to the PWR change notification", [{pid, PidToSendNotification}]),
			
			NewPwrStatusNotificationPidList = lists:delete(PidToSendNotification, State#state.pwrStatusNotificationPidList),
			{reply, ok, State#state {pwrStatusNotificationPidList = NewPwrStatusNotificationPidList}};
		false ->
			?DO_ERR("Pid does not subscribed to the PWR change notification", [{pid, PidToSendNotification}]),
			{reply, {error, {pid_does_not_subscribed_to_pwr_change_notification, PidToSendNotification}}, State}
	end;

handle_call({oscillator_status_change_subscribe, PidToSendNotification}, _From, State) ->
	%% Check that PidToSendNotification is in the list or not.
	case lists:member(PidToSendNotification, State#state.oscStatusNotificationPidList) of
		true ->
			%% Pid is already subscribed to the OSCILLATOR change notification.
			?DO_ERR("Pid is already subscribed to the OSCILLATOR change notification", [{pid, PidToSendNotification}]),
			{reply, {error, {pid_already_subscribed_to_oscillator_change_notification, PidToSendNotification}}, State};
		false ->
			%% Insert Pid into the list
			?DO_INFO("Pid has been subscribed to the OSCILLATOR change notification", [{pid, PidToSendNotification}]),
			
			NewOscStatusNotificationPidList = lists:append(State#state.oscStatusNotificationPidList, [PidToSendNotification]),
			
			%% Start timer for periodic check, if not yet started.
			case State#state.oscStatusCheckIntervalTref of
				undefined ->
					%% Timer does not started. Do it.
					{ok, TRef} = timer:send_interval(?OSCILLATOR_STATUS_CHECK_INTERVAL, self(), {oscillator_status_check}),
					
					{reply, ok, State#state {oscStatusNotificationPidList = NewOscStatusNotificationPidList,
											 oscStatusCheckIntervalTref = TRef}};
				_->
					%% Timer already started.
					{reply, ok, State#state {oscStatusNotificationPidList = NewOscStatusNotificationPidList}}
			end
	end;

handle_call({oscillator_status_change_unsubscribe, PidToSendNotification}, _From, State) ->
	case lists:member(PidToSendNotification, State#state.oscStatusNotificationPidList) of
		true ->
			?DO_INFO("Pid has been unsubscribed to the PWR change notification", [{pid, PidToSendNotification}]),
			
			NewOscStatusNotificationPidList = lists:delete(PidToSendNotification, State#state.oscStatusNotificationPidList),
			
			%% Cancel timer if pidlist is empty.
			case NewOscStatusNotificationPidList of
				[] ->
					timer:cancel(State#state.oscStatusCheckIntervalTref),
					{reply, ok, State#state {oscStatusNotificationPidList = NewOscStatusNotificationPidList,
											 oscStatusCheckIntervalTref = undefined}};
				
				_->	{reply, ok, State#state {oscStatusNotificationPidList = NewOscStatusNotificationPidList}}
			end;
		false ->
			?DO_ERR("Pid does not subscribed to the PWR change notification", [{pid, PidToSendNotification}]),
			{reply, {error, {pid_does_not_subscribed_to_pwr_change_notification, PidToSendNotification}}, State}
	end;

handle_call({stop}, _From, State) ->
	case State#state.pwrStatusCheckIntervalTref of
		undefined ->
			ok;
		_->	%% Stop timer
			timer:cancel(State#state.pwrStatusCheckIntervalTref)
	end,
	
	%% Stop gen_server
	{stop, normal, ok, State#state{pwrStatusCheckIntervalTref = undefined}};
	
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
handle_info({pwr_status_check}, State) ->
	%% Check status of PWR.
	
	case do_pwrfail_bit_read(false) of
		{ok, Current_PWRFAIL_bit_status} ->
			case Current_PWRFAIL_bit_status of
				?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST ->
					%% Currently the power is not lost.
					
					%% Compare the current power status with previous known power status.
					case State#state.pwrStatus of
						undefined ->
							%% Initial case, when RTC driver was started.
							NewState = State#state{pwrStatus = ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST,
												   pwrStatusLastCheckTime = ?GET_NOW},
							?DO_INFO("RTC's power is initiated, power is OK.", [
															  {pwrStatus, State#state.pwrStatus, 'OK'},
															  {pwrStatusLastCheckTime, NewState#state.pwrStatusLastCheckTime}
															  ]),
							{noreply, NewState};
						
						?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST ->
							%% Power status has not been changed since the last time when it was checked.
							{noreply, State#state{pwrStatusLastCheckTime = ?GET_NOW}};
						
						?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST ->
							%% This case should not be happened!!!
							%%?DO_ERR("Unexpected case happened when handle RTC PWR check.", [{genServerStateRec, State}, {readPwrFailBit, Current_PWRFAIL_bit_status}, {lastKnownPwrStatus, State#state.pwrStatus}]),
							NewState = State#state{pwrStatus = ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST,
												   pwrStatusLastCheckTime = ?GET_NOW},
							?DO_INFO("RTC's power is back.", [
															  {pwrStatus, State#state.pwrStatus, 'OK'},
															  {pwrStatusLastCheckTime, NewState#state.pwrStatusLastCheckTime}
															  ]),
							{noreply, NewState}
					end;
				
				?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST ->
					case State#state.pwrStatus of
						?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST ->
							%% The PWRFAIL bit is ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST, but because I could read the device, 
							%% this means the main power returns back to operation. PWRFAIL bit should be cleared, and the notification should
							%% be send out.
							
							PwrDownTS = do_pwr_down_date_and_time_get(),
							PwrUpTS = do_pwr_up_date_and_time_get(),
							
							do_pwrfail_bit_clear(),
							
							?DO_INFO("Main power of RTC is back", []),
							
							[begin
								 Pid ! {?NOTIFICATION_PWR_IS_BACK, PwrDownTS, PwrUpTS} 
							 end || Pid <- State#state.pwrStatusNotificationPidList],
							
							{noreply, State#state{pwrStatus = ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST,
												  pwrStatusLastCheckTime = ?GET_NOW}};
						
						_->
							%% Last known power status is: not lost/undefined
							%% This means the power status has been changed from "not lost" to "lost".
							%% Do send notification about it.
							
							?DO_INFO("Main power of RTC is lost", []),
							
							[begin
								 Pid ! {?NOTIFICATION_PWR_IS_LOST} 
							 end || Pid <- State#state.pwrStatusNotificationPidList],
							
							NewState = State#state{pwrStatus = ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST,
												   pwrStatusLastCheckTime = ?GET_NOW},
							{noreply, NewState}
					end
			end;
		
		{error, _ER} ->
			%%?DO_ERR("Failed to read PWRFAIIL bit", [{reason, ER}]),
			
			%% Faild to read PWR status.
			%% I guess the whole RTC module is not available due to power failure.
			case State#state.pwrStatus of
				?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST ->
					%% Do nothing.
					{noreply, State#state{pwrStatusLastCheckTime = ?GET_NOW}};
				
				_->	%% Send notification about power failure.
					
					?DO_INFO("Main power of RTC is lost", []),
					
					[begin
						 Pid ! {?NOTIFICATION_PWR_IS_LOST} 
					 end || Pid <- State#state.pwrStatusNotificationPidList],
					
					NewState = State#state{pwrStatus = ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST,
										   pwrStatusLastCheckTime = ?GET_NOW},
					{noreply, NewState}
			end
	end;

handle_info({oscillator_status_check}, State) ->
	%% Do check OSCRUN bit and send notification if its state has been changes since last check.
	case do_oscillator_is_running() of
		{ok, ?RTC_WKDAY_BIT_OSCRUN_EN} ->
			%% Compare the new value to the latest known value.
			case State#state.oscStatus of
				undefined ->
					%% Initial value, change the notification anyway.
					
					?DO_INFO("RTC oscillator is running", []),
					
					[begin
						 Pid ! {?NOTIFICATION_OSCILLATOR_IS_RUNNING} 
					 end || Pid <- State#state.oscStatusNotificationPidList],
					
					{noreply, State#state{oscStatus = ?RTC_WKDAY_BIT_OSCRUN_EN,
										  oscStatusLastCheckTime = ?GET_NOW}};
				?RTC_WKDAY_BIT_OSCRUN_EN ->
					%% OSCILLATOR status has not changed. Do nothing.
					{noreply, State#state{oscStatusLastCheckTime = ?GET_NOW}};
				
				?RTC_WKDAY_BIT_OSCRUN_DIS ->
					%% OSCILLATOR status has been changed. Do send notification.
					
					?DO_INFO("RTC oscillator is running", []),
					
					[begin
						 Pid ! {?NOTIFICATION_OSCILLATOR_IS_RUNNING}
					 end || Pid <- State#state.oscStatusNotificationPidList],
					
					{noreply, State#state{oscStatus = ?RTC_WKDAY_BIT_OSCRUN_EN,
										  oscStatusLastCheckTime = ?GET_NOW}}
			end;
			
		{ok, ?RTC_WKDAY_BIT_OSCRUN_DIS} ->
			%% Compare the new value to the latest known value.
			case State#state.oscStatus of
				undefined ->
					%% Initial value, change the notification anyway.
					
					?DO_ERR("RTC oscillator is not running", []),
					
					ReasonInfo = case do_oscillator_is_started() of
									 {ok, ?RTC_WKDAY_BIT_OSCRUN_EN} ->
										 {'ST bit', {?RTC_WKDAY_BIT_OSCRUN_EN, osc_started}};
									 {ok, ?RTC_WKDAY_BIT_OSCRUN_DIS} ->
										 {'ST bit', {?RTC_WKDAY_BIT_OSCRUN_DIS, osc_not_started}};
									 ER ->
										 {'ST bit', {unexpected_error, ER}}
								 end,
					
					[begin
						 Pid ! ?NOTIFICATION_OSCILLATOR_IS_NOT_RUNNING(ReasonInfo)
					 end || Pid <- State#state.oscStatusNotificationPidList],
					
					{noreply, State#state{oscStatus = ?RTC_WKDAY_BIT_OSCRUN_DIS,
										  oscStatusLastCheckTime = ?GET_NOW}};
				?RTC_WKDAY_BIT_OSCRUN_EN ->
					%% OSCILLATOR status has been changed. Do send notification.
					
					?DO_ERR("RTC oscillator is not running", []),
					
					ReasonInfo = case do_oscillator_is_started() of
									 {ok, ?RTC_WKDAY_BIT_OSCRUN_EN} ->
										 {'ST bit', {?RTC_WKDAY_BIT_OSCRUN_EN, osc_started}};
									 {ok, ?RTC_WKDAY_BIT_OSCRUN_DIS} ->
										 {'ST bit', {?RTC_WKDAY_BIT_OSCRUN_DIS, osc_not_started}};
									 ER ->
										 {'ST bit', {unexpected_error, ER}}
								 end,
					
					[begin
						 Msg = ?NOTIFICATION_OSCILLATOR_IS_NOT_RUNNING(ReasonInfo),
%% 						 ?DO_INFO("RTC oscillator is not running. Send notification to pid.", [{pid, Pid},
%% 																							   {msg, Msg}]),
						 Pid ! Msg
					 end || Pid <- State#state.oscStatusNotificationPidList],
					
					{noreply, State#state{oscStatus = ?RTC_WKDAY_BIT_OSCRUN_DIS,
										  oscStatusLastCheckTime = ?GET_NOW}};
				
				?RTC_WKDAY_BIT_OSCRUN_DIS ->
					%% OSCILLATOR status has been changed. Do nothing.
					{noreply, State#state{oscStatusLastCheckTime = ?GET_NOW}}
			end;
		ER->
			?DO_ERR("Unexpected error when check RTC oscillator.", [{reason, ER}]),
			{noreply, State#state{oscStatusLastCheckTime = ?GET_NOW}}
	end;
	
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
%% Control register setting
%% ====================================================================

%% ====================================================================
%% @doc
%% Configure CONTROL REG. OUT bit
%% @end
-spec do_ctrl_bit_out_set(rtc_ctrl_bit_out()) -> ok | {error, term()}.
%% ====================================================================
do_ctrl_bit_out_set(CtrlBitOut) ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, #rtcControlReg.bit_out, 
								 CtrlBitOut) of
		{ok,_} ->
			%%?DO_INFO("CONTROL REG OUT bit has been set", []),
			ok;
		ER->
			?DO_ERR("Failed to set CONTROL REG OUT bit.", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Configure CONTROL REG. SQWEN bit
%% @end
-spec do_ctrl_bit_sqwen_set(rtc_ctrl_bit_sqwen()) -> ok | {error, term()}.
%% ====================================================================
do_ctrl_bit_sqwen_set(Sqwen) ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS,  
								 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, #rtcControlReg.bit_sqwEn, 
								 Sqwen) of
		{ok,_} ->
			%%?DO_INFO("CONTROL REG SQWEN bit has been set", []),
			ok;
		ER->
			?DO_ERR("Failed to set CONTROL REG SQWEN bit", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Configure CONTROL REG. EXTOSC bit
%% @end
-spec do_ctrl_bit_extosc_set(rtc_ctrl_bit_extosc()) -> ok | {error, term()}.
%% ====================================================================
do_ctrl_bit_extosc_set(Extosc) ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS,  
								 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, #rtcControlReg.bit_extOsc, 
								 Extosc) of
		{ok,_} ->
			%%?DO_INFO("CONTROL REG EXTOSC bit has been set", []),
			ok;
		ER->
			?DO_ERR("Failed to set CONTROL REG EXTOSC bit.", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Configure CONTROL REG. CRSTRIM bit
%% @end
-spec do_ctrl_bit_crstrim_set(rtc_ctrl_bit_crstrim()) -> ok | {error, term()}.
%% ====================================================================
do_ctrl_bit_crstrim_set(Crstrim) ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS,  
								 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, #rtcControlReg.bit_crsTrim, 
								 Crstrim) of
		{ok,_} ->
			%%?DO_INFO("CONTROL REG CRSTRIM bit has been set", []),
			ok;
		ER->
			?DO_ERR("Failed to set CONTROL REG CRSTRIM bit.", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Configure CONTROL REG. SQWFS bit
%% @end
-spec do_ctrl_bit_sqwfs_set(rtc_ctrl_bit_sqwfs()) -> ok | {error, term()}.
%% ====================================================================
do_ctrl_bit_sqwfs_set(Sqwfs) ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS,  
								 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, #rtcControlReg.bit_sqwfs, 
								 Sqwfs) of
		{ok,_} ->
			%%?DO_INFO("CONTROL REG SQWFS bit has been set", []),
			ok;
		ER->
			?DO_ERR("Failed to set CONTROL REG SQWFS bit.", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% Power failure related functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Read PWRFAIL bit
%% @end
-spec do_pwrfail_bit_read(boolean()) -> {ok, rtc_pwrfail()} | {error, term()}.
%% ====================================================================
do_pwrfail_bit_read(DoPrintErrorIfItIs) ->
	case catch bitfield_get(#rtcWkDayReg{}, #rtcWkDayReg.length, {addrIdx, #rtcWkDayReg.address}, #rtcWkDayReg.bit_pwrFail) of
		[{#rtcWkDayReg.bit_pwrFail, Pwrfail}] ->
			{ok, Pwrfail};
		ER ->
			case DoPrintErrorIfItIs of
				true ->
					?DO_ERR("Failed to read PWRFAIIL bit", [{reason, ER}]);
				_->	do_nothing
			end,
			ER
	end.

%% ====================================================================
%% @doc
%% Clear PWRFAIL bit
%% @end
-spec do_pwrfail_bit_clear() -> ok | {error, term()}.
%% ====================================================================
do_pwrfail_bit_clear() ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 #rtcWkDayReg{}, #rtcWkDayReg.length, #rtcWkDayReg.address, #rtcWkDayReg.bit_pwrFail, 
								 ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST) of
		{ok,_} ->
			?DO_INFO("PWRFAIL bit has been cleared", []),
			ok;
		ER->
			?DO_ERR("Failed to set PWRFAIIL bit", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Read PWR DOWN Date and Time in RTC device.
%% @end
-spec do_pwr_down_date_and_time_get() -> {ok, datetime()} | {error, term()}.
%% ====================================================================
do_pwr_down_date_and_time_get() ->
	case catch do_month_get(rtcPwrDNMonthReg) of
		{ok, Month} ->
			case catch do_date_get(rtcPwrDNDateReg) of
				{ok, Date} ->
					case catch do_hour_get(rtcPwrDNHourReg) of
						{ok, Hour} ->
							case catch do_minute_get(rtcPwrDNMinuteReg) of
								{ok, Minute} ->
									Time = {Hour,Minute,0},
									
									%% Just guess that power failure has happened this year.
									{{Year,_,_},_} = calendar:local_time(),
									FullDate = {Year,Month,Date},
									
									{ok, {FullDate, Time}};
								
								ER->ER
							end;
						ER->ER
					end;
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Read PWR UP Date and Time in RTC device.
%% @end
-spec do_pwr_up_date_and_time_get() -> {ok, datetime()} | {error, term()}.
%% ====================================================================
do_pwr_up_date_and_time_get() ->
	case catch do_month_get(rtcPwrUPMonthReg) of
		{ok, Month} ->
			case catch do_date_get(rtcPwrUPDateReg) of
				{ok, Date} ->
					case catch do_hour_get(rtcPwrUPHourReg) of
						{ok, Hour} ->
							case catch do_minute_get(rtcPwrUPMinuteReg) of
								{ok, Minute} ->
									Time = {Hour,Minute,0},
									
									%% Just guess that power failure has happened this year.
									{{Year,_,_},_} = calendar:local_time(),
									FullDate = {Year,Month,Date},
									
									{ok, {FullDate, Time}};
								
								ER->ER
							end;
						ER->ER
					end;
				ER->ER
			end;
		ER->ER
	end.


%% ====================================================================
%% Battery backup related functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Read VBATEN bit
%% @end
-spec do_vbaten_read() -> {ok, rtc_vbaten()} | {error, term()}.
%% ====================================================================
do_vbaten_read() ->
	case catch bitfield_get(#rtcWkDayReg{}, #rtcWkDayReg.length, {addrIdx, #rtcWkDayReg.address}, #rtcWkDayReg.bit_vBatEn) of
		[{#rtcWkDayReg.bit_vBatEn, Vbaten}] ->
			{ok, Vbaten};
		ER ->
			?DO_ERR("Failed to read VBATEN bit", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Set VBATEN bit
%% @end
-spec do_vbaten_set(rtc_vbaten()) -> ok | {error, term()}.
%% ====================================================================
do_vbaten_set(Vbaten) ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS,  
								 #rtcWkDayReg{}, #rtcWkDayReg.length, #rtcWkDayReg.address, #rtcWkDayReg.bit_vBatEn, 
								 Vbaten) of
		{ok,_} ->
			?DO_INFO("VBATEN bit has been set", []),
			ok;
		ER->
			?DO_ERR("Failed to set VBATEN bit", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Enable/Disable Alarm interrupt.
%% @end
-spec do_alarm_interrupt_enable_disable(rtc_alarm_id(), rtc_alarm_interrupt_en_status()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_interrupt_enable_disable(AlarmId, AlarmInterruptEnableStatus) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	case AlarmInterruptEnableStatus of
		?RTC_CTRL_BIT_ALM_Ax_EN ->
			do_alarm_interrupt_enable(AlarmId);
		_->	do_alarm_interrupt_disable(AlarmId)
	end;
do_alarm_interrupt_enable_disable(AlarmId, _AlarmInterruptEnableStatus) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Enable Alarm interrupt.
%% @end
-spec do_alarm_interrupt_enable(rtc_alarm_id()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_interrupt_enable(AlarmId) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	DoEnableAlarm = case catch do_alarm_interrupt_is_enabled(AlarmId) of
						{ok, ?RTC_CTRL_BIT_ALM_Ax_EN} ->
							alreadyEnabled;
						_->
							goAhead
					end,
	
	case DoEnableAlarm of
		alreadyEnabled ->
			ok;
		goAhead ->
			AlarmIdIdx = case AlarmId of
							 ?RTC_ALARM_0_ID ->
								 #rtcControlReg.bit_almA0;
							 ?RTC_ALARM_1_ID ->
								 #rtcControlReg.bit_almA1
						 end,
			case AlarmIdIdx of
				{error,ER} ->
					{error,ER};
				_->
					case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
												 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, AlarmIdIdx, 
												 ?RTC_CTRL_BIT_ALM_Ax_EN) of
						{ok,_} ->
							?DO_INFO("Alarm interrupt has been enabled", [{alarmId, AlarmId}]),
							ok;
						ER->
							?DO_ERR("Failed to enable alarm interrupt", [{alarmId, AlarmId},{reason, ER}]),
							ER
					end
			end
	end;
do_alarm_interrupt_enable(AlarmId) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Disable Alarm interrupt.
%% @end
-spec do_alarm_interrupt_disable(rtc_alarm_id()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_interrupt_disable(AlarmId) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	AlarmIdIdx = case AlarmId of
					 ?RTC_ALARM_0_ID ->
						 #rtcControlReg.bit_almA0;
					 ?RTC_ALARM_1_ID ->
						 #rtcControlReg.bit_almA1
				 end,
	
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 #rtcControlReg{}, #rtcControlReg.length, #rtcControlReg.address, AlarmIdIdx, 
								 ?RTC_CTRL_BIT_ALM_Ax_DIS) of
		{ok,_} ->
			%% Clear the IF bit too
			case catch do_alarm_interrupt_flag_clear(AlarmId) of
				ok ->
					?DO_INFO("Alarm interrupt has been disabled", [{alarmId, AlarmId}]),
					ok;
				ER ->
					?DO_ERR("Failed to clear alarm interrupt flag once the alarm was disabled", [{alarmId, AlarmId},{reason, ER}]),
					ER
			end;
		ER->
			?DO_ERR("Failed to disable alarm interrupt", [{alarmId, AlarmId},{reason, ER}]),
			ER
	end;
do_alarm_interrupt_disable(AlarmId) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Read Alarm interrupt enabled bit.
%% @end
-spec do_alarm_interrupt_is_enabled(rtc_alarm_id()) -> {ok, rtc_alarm_interrupt_en_status()} | {error, term()}.
%% ====================================================================
do_alarm_interrupt_is_enabled(AlarmId) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	AlarmIdIdx = case AlarmId of
					 ?RTC_ALARM_0_ID ->
						 #rtcControlReg.bit_almA0;
					 ?RTC_ALARM_1_ID ->
						 #rtcControlReg.bit_almA1
				 end,
	
	case catch bitfield_get(#rtcControlReg{}, #rtcControlReg.length, {addrIdx, #rtcControlReg.address}, [AlarmIdIdx]) of
		[{AlarmIdIdx, B0}] ->
			{ok, B0};
		ER->
			?DO_ERR("Failed to read CONTROL register", [{reason, ER}]),
			{error, ER}
	end;
do_alarm_interrupt_is_enabled(AlarmId) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Clear interrupt flag bit.
%% @end
-spec do_alarm_interrupt_flag_clear(rtc_alarm_id()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_interrupt_flag_clear(AlarmId) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitFieldIdx} = 
		case AlarmId of
			?RTC_ALARM_0_ID ->
				{#rtcAlm0WDayReg{}, #rtcAlm0WDayReg.length, #rtcAlm0WDayReg.address, #rtcAlm0WDayReg.bit_almIf};
			?RTC_ALARM_1_ID ->
				{#rtcAlm1WDayReg{}, #rtcAlm1WDayReg.length, #rtcAlm1WDayReg.address, #rtcAlm1WDayReg.bit_almIf}
		end,

	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, RegisterRec, DataLengthIdx, RegisterAddressIdx, BitFieldIdx, ?RTC_ALMxWKDAY_BIT_ALMxIF_CLEAR) of
		{ok, _} ->
			ok;
		{error, ER} ->
			{error, ER}
	end;
do_alarm_interrupt_flag_clear(AlarmId) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Check interrupt flag bit.
%% @end
-spec do_alarm_interrupt_flag_check(rtc_alarm_id()) -> {ok, rtc_alarm_interrupt_set()} | {ok, rtc_alarm_interrupt_clear()} | {error, term()}.
%% ====================================================================
do_alarm_interrupt_flag_check(AlarmId) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitFieldIdx} = 
		case AlarmId of
			?RTC_ALARM_0_ID ->
				{#rtcAlm0WDayReg{}, #rtcAlm0WDayReg.length, #rtcAlm0WDayReg.address, #rtcAlm0WDayReg.bit_almIf};
			?RTC_ALARM_1_ID ->
				{#rtcAlm1WDayReg{}, #rtcAlm1WDayReg.length, #rtcAlm1WDayReg.address, #rtcAlm1WDayReg.bit_almIf}
		end,
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, BitFieldIdx) of
		{error, ER} ->
			?DO_ERR("Failed to read Alarm interrupt flag", [{alamId, AlarmId},{reason, ER}]),
			{error, ER};
		[{BitFieldIdx, Data}] ->
			{ok, Data}
	end;
do_alarm_interrupt_flag_check(AlarmId) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Check interrupt flag bit.
%% @end
-spec do_alarm_interrupt_flag_check() -> {ok, list({rtc_alarm_id(), rtc_alarm_interrupt_set() | rtc_alarm_interrupt_clear()})} | {error, term()}.
%% ====================================================================
do_alarm_interrupt_flag_check() ->
	Result = [begin
				  do_alarm_interrupt_flag_check(AlarmId)
			  end || AlarmId <- [?RTC_ALARM_0_ID, ?RTC_ALARM_1_ID]],
	{ok, Result}.

%% ====================================================================
%% @doc
%% Configure alarm.
%% @end
-spec do_alarm_configure(rtc_alarm_id(), datetime(), rtc_alarm_mask(), rtc_alarm_interrupt_en_status(), rtc_alarm_interrupt_out_pol()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_configure(AlarmId, DateAndTime, Mask, InterruptEnStatus, InterruptOutPol) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	%% Prepare alarm SECOND, MINUTE, HOUR, WDAY, DATE and MATCH registers.
 	{FullDate, Time} = DateAndTime,
 	{_Year,Month,Date} = FullDate,
 	{Hour,Minute,Second} = Time,
	
	AlarmInterruptStatusSetFunc = case InterruptEnStatus of
									  ?RTC_CTRL_BIT_ALM_Ax_EN ->
										  do_alarm_interrupt_enable;
									  _-> do_alarm_interrupt_disable
								  end,

	{AlarmMonthRegister,
	 AlarmDateRegister,
	 AlarmWKDayRegister,
	 AlarmHourRegister,
	 AlarmMinuteRegister,
	 AlarmSecondRegister} = case AlarmId of
								?RTC_ALARM_0_ID ->
									{rtcAlm0MonthReg,
									 rtcAlm0DateReg,
									 rtcAlm0WDayReg,
									 rtcAlm0HourReg,
									 rtcAlm0MinReg,
									 rtcAlm0SecReg};
								
								?RTC_ALARM_1_ID ->
									{rtcAlm1MonthReg,
									 rtcAlm1DateReg,
									 rtcAlm1WDayReg,
									 rtcAlm1HourReg,
									 rtcAlm1MinReg,
									 rtcAlm1SecReg}
							end,
	
	Result = do_alarm_configure_loop([{?MODULE, do_alarm_interrupt_flag_clear, [AlarmId]},
									  {?MODULE, do_month_set, [AlarmMonthRegister, Month]},
									  {?MODULE, do_date_set, [AlarmDateRegister, Date]},
									  {?MODULE, do_wday_set, [AlarmWKDayRegister, calendar:day_of_the_week(FullDate)]},
									  {?MODULE, do_hour_set, [AlarmHourRegister, Hour]},
									  {?MODULE, do_minute_set, [AlarmMinuteRegister, Minute]},
									  {?MODULE, do_second_set, [AlarmSecondRegister, Second]},
									  {?MODULE, do_alarm_mask_set, [AlarmId, Mask]},
									  {?MODULE, do_alarm_interrupt_out_pol_set, [AlarmId, InterruptOutPol]},
									  {?MODULE, AlarmInterruptStatusSetFunc, [AlarmId]}], ok),
	case Result of
		ok ->
			?DO_INFO("Alarm interrupt has been configured", [{alarmId, AlarmId},
															 {alarmDate, DateAndTime},
															 {alarmMask, Mask},
															 {alarmPolarity, InterruptOutPol}]);
		ER->	
			?DO_ERR("Failed to configure Alarm", [{alamId, AlarmId},{reason, ER}])
	end,
	
	Result;
do_alarm_configure(AlarmId, _DateAndTime, _Mask, _InterruptStatus, _InterruptOutPol) ->
	{error, {invalid_alarm_id, AlarmId}}.

do_alarm_configure_loop([], Result) ->
	Result;
do_alarm_configure_loop([{M,F,A} | T], Result) ->
	case catch erlang:apply(M, F, A) of
		{ok,_} ->
			do_alarm_configure_loop(T,Result);
		ok ->
			do_alarm_configure_loop(T,Result);
		ER ->
			do_alarm_configure_loop([], ER)
	end.

%% ====================================================================
%% @doc
%% Read the configured alarm date and time settings.
%% @end
-spec do_alarm_date_and_time_get(rtc_alarm_id()) -> {ok, datetime()} | {error, term()}.
%% ====================================================================
do_alarm_date_and_time_get(AlarmId) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	{AlarmMonthRegister,
	 AlarmDateRegister,
	 _AlarmWKDayRegister,
	 AlarmHourRegister,
	 AlarmMinuteRegister,
	 AlarmSecondRegister} = case AlarmId of
								?RTC_ALARM_0_ID ->
									{rtcAlm0MonthReg,
									 rtcAlm0DateReg,
									 rtcAlm0WDayReg,
									 rtcAlm0HourReg,
									 rtcAlm0MinReg,
									 rtcAlm0SecReg};
								
								?RTC_ALARM_1_ID ->
									{rtcAlm1MonthReg,
									 rtcAlm1DateReg,
									 rtcAlm1WDayReg,
									 rtcAlm1HourReg,
									 rtcAlm1MinReg,
									 rtcAlm1SecReg}
							end,
	
	case catch do_year_get(rtcYearReg) of
		{ok, Year} ->
			case catch do_month_get(AlarmMonthRegister) of
				{ok, Month} ->
					case catch do_date_get(AlarmDateRegister) of
						{ok, Date} ->
							case catch do_hour_get(AlarmHourRegister) of
								{ok, Hour} ->
									case catch do_minute_get(AlarmMinuteRegister) of
										{ok, Minute} ->
											case catch do_second_get(AlarmSecondRegister) of
												{ok, Second} ->
													Time = {Hour,Minute,Second},
													FullDate = {Year,Month,Date},
													
													{ok, {FullDate, Time}};
													
												ER->ER
											end;
										ER->ER
									end;
								ER->ER
							end;
						ER->ER
					end;
				ER->ER
			end;
		ER->ER
	end;
do_alarm_date_and_time_get(AlarmId) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Setup Alarm Mask in RTC device.
%% @end
 -spec do_alarm_mask_set(rtc_alarm_id(), rtc_alarm_mask()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_mask_set(AlarmId, Mask) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	{RegisterRec, DataLengthIdx, RegisterAddr, BitFieldIdx} = 
		case AlarmId of
			?RTC_ALARM_0_ID ->
				{#rtcAlm0WDayReg{}, #rtcAlm0WDayReg.length, #rtcAlm0WDayReg.address, #rtcAlm0WDayReg.bit_almMask};
			?RTC_ALARM_1_ID ->
				{#rtcAlm1WDayReg{}, #rtcAlm1WDayReg.length, #rtcAlm1WDayReg.address, #rtcAlm1WDayReg.bit_almMask}
		end,
	
	%% Prepare register that write into the RTC device.
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 RegisterRec, 
								 DataLengthIdx,
								 RegisterAddr, 
								 [{BitFieldIdx, Mask}]) of
		{ok,_} ->
			ok;
		ER->ER
	end;
do_alarm_mask_set(AlarmId, _Mask) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% @doc
%% Setup Alarm Interrupt Output Polarity in RTC device.
%% @end
 -spec do_alarm_interrupt_out_pol_set(rtc_alarm_id(), rtc_alarm_interrupt_out_pol()) -> ok | {error, term()}.
%% ====================================================================
do_alarm_interrupt_out_pol_set(AlarmId, InterruptOutPol) when (AlarmId == ?RTC_ALARM_0_ID) or (AlarmId == ?RTC_ALARM_1_ID)->
	{RegisterRec, DataLengthIdx, RegisterAddr, BitFieldIdx} = 
		case AlarmId of
			?RTC_ALARM_0_ID ->
				{#rtcAlm0WDayReg{}, #rtcAlm0WDayReg.length, #rtcAlm0WDayReg.address, #rtcAlm0WDayReg.bit_almPol};
			?RTC_ALARM_1_ID ->
				{#rtcAlm1WDayReg{}, #rtcAlm1WDayReg.length, #rtcAlm1WDayReg.address, #rtcAlm1WDayReg.bit_almPol}
		end,
	
	%% Prepare register that write into the RTC device.
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 RegisterRec, 
								 DataLengthIdx,
								 RegisterAddr, 
								 [{BitFieldIdx, InterruptOutPol}]) of
		{ok,_} ->
			ok;
		ER->ER
	end;
do_alarm_interrupt_out_pol_set(AlarmId, _InterruptOutPol) ->
	{error, {invalid_alarm_id, AlarmId}}.

%% ====================================================================
%% Oscillator related functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Read OSCRUN bit- This tells that oscillator is running or not.
%% @end
-spec do_oscillator_is_running() -> {ok, rtc_oscrun()} | {error, term()}.
%% ====================================================================
do_oscillator_is_running() ->
	case catch bitfield_get(#rtcWkDayReg{}, #rtcWkDayReg.length, {addrIdx, #rtcWkDayReg.address}, #rtcWkDayReg.bit_oscRun) of
		[{#rtcWkDayReg.bit_oscRun, Oscrun}] ->
			{ok, Oscrun};
		ER ->
			ER
	end.

%% ====================================================================
%% @doc
%% Read ST bit- This tells that oscillator is strted or not.
%% @end
-spec do_oscillator_is_started() -> {ok, rtc_osc_start()} | {error, term()}.
%% ====================================================================
do_oscillator_is_started() ->
	case catch bitfield_get(#rtcSecondReg{}, #rtcSecondReg.length, {addrIdx, #rtcSecondReg.address}, #rtcSecondReg.bit_st) of
		[{#rtcSecondReg.bit_st, ST}] ->
			{ok, ST};
		ER ->
			ER
	end.

%% ====================================================================
%% @doc
%% Start Oscillator.
%% @end
-spec do_oscillator_start() -> ok | {error, term()}.
%% ====================================================================
do_oscillator_start() ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 #rtcSecondReg{}, 
								 #rtcSecondReg.length, 
								 #rtcSecondReg.address, 
								 #rtcSecondReg.bit_st, 
								 ?RTC_SECOND_BIT_ST_EN) of
		{ok,_} ->
			?DO_INFO("RTC oscillator has been started", []),
			ok;
		ER->
			?DO_ERR("Failed to start RTC oscillator", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Stop Oscillator.
%% @end
-spec do_oscillator_stop() -> ok | {error, term()}.
%% ====================================================================
do_oscillator_stop() ->
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 #rtcSecondReg{}, 
								 #rtcSecondReg.length, 
								 #rtcSecondReg.address, 
								 #rtcSecondReg.bit_st, 
								 ?RTC_SECOND_BIT_ST_DIS) of
		{ok,_} ->
			?DO_INFO("RTC oscillator has been stopped", []),
			ok;
		ER->
			?DO_ERR("Failed to stop RTC oscillator", [{reason, ER}]),
			ER
	end.

%% ====================================================================
%% RTC Date And Time related functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Setup Date and Time in RTC device.
%% @end
-spec do_date_and_time_set(datetime() | time_format()) -> ok | {error, term()}.
%% ====================================================================
do_date_and_time_set(TimeFormat) when ((TimeFormat == ?RTC_HOUR_BIT_TIME_FORMAT_12H) or (TimeFormat == ?RTC_HOUR_BIT_TIME_FORMAT_24H)) ->
	DateAndTime = calendar:local_time(),
	
	DateAndTimeNew = case TimeFormat of
						 ?RTC_HOUR_BIT_TIME_FORMAT_12H ->
							 {Date, Time} = DateAndTime,
							 {Hour,Minute,Second} = Time,
							 
							 %% Convert 24H format hour to 12H format.
							 Hour12 = hour_convert_to_12h_format(Hour),
							 
							 {Date, {Hour12, Minute, Second}};
						 _-> DateAndTime
					 end,
	do_date_and_time_set(DateAndTimeNew);
do_date_and_time_set(DateAndTime) when is_tuple(DateAndTime)->
	%% Stop RTC oscillator
	do_oscillator_stop(),
	
	%% Save ALARM ENABLED configuration bit
	ALM0_EN_BIT = do_alarm_interrupt_is_enabled(?RTC_ALARM_0_ID),
	ALM1_EN_BIT = do_alarm_interrupt_is_enabled(?RTC_ALARM_1_ID),
	
	%% Disable ALARM interrupts while configure DATE&TIME
	do_alarm_interrupt_disable(?RTC_ALARM_0_ID),
	do_alarm_interrupt_disable(?RTC_ALARM_1_ID),
	
	%% Prepare SECOND, MINUTE, HOUR, WKDAY, DATE, MONTH, YEAR registers
 	{FullDate, Time} = DateAndTime,
 	{Year,Month,Date} = FullDate,
 	{Hour,Minute,Second} = Time,
	
	Result = do_date_and_time_set_loop([
										{?MODULE, do_year_set, [rtcYearReg, Year]},
										{?MODULE, do_month_set, [rtcMonthReg, Month]},
										{?MODULE, do_date_set, [rtcDateReg, Date]},
										{?MODULE, do_wday_set, [rtcWkDayReg, calendar:day_of_the_week(FullDate)]},
										{?MODULE, do_hour_set, [rtcHourReg, Hour]},
										{?MODULE, do_minute_set, [rtcMinuteReg, Minute]},
										{?MODULE, do_second_set, [rtcSecondReg, Second]}], ok),
	
	%% Re-enable ALARM interrupt if those were enabled before start configure DATE&TIME
	do_alarm_interrupt_enable_disable(?RTC_ALARM_0_ID, ALM0_EN_BIT),
	do_alarm_interrupt_enable_disable(?RTC_ALARM_1_ID, ALM1_EN_BIT),
	
	case Result of
		ok ->
			%% Re-start RTC oscillator
			do_oscillator_start(),
			?DO_INFO("Date and Time has been configured in RTC", [{dateAndTime, DateAndTime},
																  {weekDay, calendar:day_of_the_week(FullDate)}]);
		ER-> 
			?DO_ERR("Failed to setup Date and Time in RTC", [{dateAndTime, DateAndTime},{reason, ER}])
	end,
	
	Result;
do_date_and_time_set(InvalidParameter) ->
	{error, {"Invalid parameter when configure Date and Time in RTC.", InvalidParameter}}.

do_date_and_time_set_loop([], Result) ->
	Result;
do_date_and_time_set_loop([{M,F,A} | T], Result) ->
	case catch erlang:apply(M, F, A) of
		{ok,_} ->
			do_date_and_time_set_loop(T,Result);
		ok ->
			do_date_and_time_set_loop(T,Result);
		ER ->
			do_date_and_time_set_loop([], ER)
	end.

%% ====================================================================
%% @doc
%% Setup Year in RTC device.
%% @end
 -spec do_year_set(atom(), year()) -> ok | {error, term()}.
%% ====================================================================
do_year_set(RegType, Year) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitYearTenIdx, BitYearOneIdx} =
		case RegType of
			rtcYearReg ->
				{#rtcYearReg{},
				 #rtcYearReg.length,
				 #rtcYearReg.address,
				 #rtcYearReg.bit_yearTen,
				 #rtcYearReg.bit_yearOne}
		end,
	
	%% Convert Year in BCD
    case catch bcd:encode(Year-?BASE_YEAR, 1) of
        <<B1:4, B0:4>> ->
            
            %% Prepare register that write into the RTC device
            case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
                                               RegisterRec, 
                                               DataLengthIdx, 
                                               RegisterAddressIdx, 
                                               [{BitYearTenIdx, B1}, {BitYearOneIdx, B0}]) of
                {ok,_} ->
                    %%?DO_INFO("Year has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Year in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end.

%% ====================================================================
%% @doc
%% Setup Month in RTC device.
%% @end
 -spec do_month_set(atom(), month()) -> ok | {error, term()}.
%% ====================================================================
 do_month_set(RegType, Month) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitMonthTenIdx, BitMonthOneIdx} = 
		case RegType of
			rtcMonthReg ->
				{#rtcMonthReg{},
				 #rtcMonthReg.length,
				 #rtcMonthReg.address,
				 #rtcMonthReg.bit_monthTen,
				 #rtcMonthReg.bit_monthOne};
			
			rtcAlm0MonthReg ->
				{#rtcAlm0MonthReg{},
				 #rtcAlm0MonthReg.length,
				 #rtcAlm0MonthReg.address,
				 #rtcAlm0MonthReg.bit_monthTen,
				 #rtcAlm0MonthReg.bit_monthOne};
			
			rtcAlm1MonthReg ->
				{#rtcAlm1MonthReg{},
				 #rtcAlm1MonthReg.length,
				 #rtcAlm1MonthReg.address,
				 #rtcAlm1MonthReg.bit_monthTen,
				 #rtcAlm1MonthReg.bit_monthOne}
		end,
	
    %% Convert Month in BCD
    case catch bcd:encode(Month, 1) of
        <<B1:4, B0:4>> ->
            %% Prepare register that write into the RTC device
            case dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS,  
                                         RegisterRec,
                                         DataLengthIdx,
                                         RegisterAddressIdx,
                                         [{BitMonthTenIdx, B1},{BitMonthOneIdx, B0}]) of
                {ok,_}  ->
                    %%?DO_INFO("Month has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Month in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end.

%% ====================================================================
%% @doc
%% Setup Date in RTC device.
%% @end
 -spec do_date_set(atom(), day()) -> ok | {error, term()}.
%% ====================================================================
 do_date_set(RegType, Day) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitDateTenIdx, BitDateOneIdx} = 
		case RegType of
			rtcDateReg ->
				{#rtcDateReg{},
				 #rtcDateReg.length,
				 #rtcDateReg.address,
				 #rtcDateReg.bit_dateTen,
				 #rtcDateReg.bit_dateOne};
			
			rtcAlm0DateReg ->
				{#rtcAlm0DateReg{},
				 #rtcAlm0DateReg.length,
				 #rtcAlm0DateReg.address,
				 #rtcAlm0DateReg.bit_dateTen,
				 #rtcAlm0DateReg.bit_dateOne};
			
			rtcAlm1DateReg ->
				{#rtcAlm1DateReg{},
				 #rtcAlm1DateReg.length,
				 #rtcAlm1DateReg.address,
				 #rtcAlm1DateReg.bit_dateTen,
				 #rtcAlm1DateReg.bit_dateOne}
		end,

	%% Convert Date in BCD
    case catch bcd:encode(Day, 1) of
        <<B1:4, B0:4>> ->
            %% Prepare register that write into the RTC device
            case dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
                                         RegisterRec, 
                                         DataLengthIdx,
                                         RegisterAddressIdx,
                                         [{BitDateTenIdx, B1},{BitDateOneIdx, B0}]) of
                {ok,_}  ->
                    %%?DO_INFO("Date has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Date in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end.

%% ====================================================================
%% @doc
%% Setup Hour in RTC device.
%% @end
 -spec do_hour_set(atom(), {hour_ampm_ind(), hour12()} | hour()) -> ok | {error, term()}.
%% ====================================================================
do_hour_set(RegType, {AMPM, Hour}) ->
	%% 12H format mode.
	
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitTimeFormatIdx, Bit12HAMPMIdx, Bit12HHrTenIdx, Bit12HHrOneIdx} = 
		case RegType of
			rtcHourReg ->
				{#rtcHourReg{},
				 #rtcHourReg.length,
				 #rtcHourReg.address,
				 #rtcHourReg.bit_timeFormat,
				 #rtcHourReg.bit_12h_ampm,
				 #rtcHourReg.bit_12h_hrTen,
				 #rtcHourReg.bit_12h_hrOne};
			
			rtcAlm0HourReg ->
				{#rtcAlm0HourReg{},
				 #rtcAlm0HourReg.length,
				 #rtcAlm0HourReg.address,
				 #rtcAlm0HourReg.bit_timeFormat,
				 #rtcAlm0HourReg.bit_12h_ampm,
				 #rtcAlm0HourReg.bit_12h_hrTen,
				 #rtcAlm0HourReg.bit_12h_hrOne};

			rtcAlm1HourReg ->
				{#rtcAlm1HourReg{},
				 #rtcAlm1HourReg.length,
				 #rtcAlm1HourReg.address,
				 #rtcAlm1HourReg.bit_timeFormat,
				 #rtcAlm1HourReg.bit_12h_ampm,
				 #rtcAlm1HourReg.bit_12h_hrTen,
				 #rtcAlm1HourReg.bit_12h_hrOne}
		end,
	
	%% Convert HourIn12HFormat to BCD
    case catch bcd:encode(Hour, 1) of
        <<B1:4, B0:4>> ->
            case dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
                                         RegisterRec, 
                                         DataLengthIdx,
                                         RegisterAddressIdx, 
                                         [{BitTimeFormatIdx, ?RTC_HOUR_BIT_TIME_FORMAT_12H},
                                          {Bit12HAMPMIdx, AMPM},
                                          {Bit12HHrTenIdx, B1},
                                          {Bit12HHrOneIdx, B0}
                                         ]) of
                {ok,_}  ->
                    %%?DO_INFO("Hour has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Hour in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end;
 do_hour_set(RegType, Hour) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitTimeFormatIdx, Bit24HHrTenIdx, Bit24HHrOneIdx} = 
		case RegType of
			rtcHourReg ->
				{#rtcHourReg{},
				 #rtcHourReg.length,
				 #rtcHourReg.address,
				 #rtcHourReg.bit_timeFormat,
				 #rtcHourReg.bit_24h_hrTen,
				 #rtcHourReg.bit_24h_hrOne};
			
			rtcAlm0HourReg ->
				{#rtcAlm0HourReg{},
				 #rtcAlm0HourReg.length,
				 #rtcAlm0HourReg.address,
				 #rtcAlm0HourReg.bit_timeFormat,
				 #rtcAlm0HourReg.bit_24h_hrTen,
				 #rtcAlm0HourReg.bit_24h_hrOne};
			
			rtcAlm1HourReg ->
				{#rtcAlm1HourReg{},
				 #rtcAlm1HourReg.length,
				 #rtcAlm1HourReg.address,
				 #rtcAlm1HourReg.bit_timeFormat,
				 #rtcAlm1HourReg.bit_24h_hrTen,
				 #rtcAlm1HourReg.bit_24h_hrOne}
		end,
	
	%% Convert Hour to BCD
    case catch bcd:encode(Hour, 1) of
        <<B1:4, B0:4>> ->
            case dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
                                         RegisterRec, 
                                         DataLengthIdx,
                                         RegisterAddressIdx, 
                                         [{BitTimeFormatIdx, ?RTC_HOUR_BIT_TIME_FORMAT_24H},
                                          {Bit24HHrTenIdx, B1},
                                          {Bit24HHrOneIdx, B0}
                                         ]) of
                {ok,_} ->
                    %%?DO_INFO("Hour has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Hour in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end.

%% ====================================================================
%% @doc
%% Setup Minute in RTC device.
%% @end
 -spec do_minute_set(atom(), minute()) -> ok | {error, term()}.
%% ====================================================================
do_minute_set(RegType, Minute) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitMinuteTenIdx, BitMinuteOneIdx} =
		case RegType of
			rtcMinuteReg ->
				{#rtcMinuteReg{},
				 #rtcMinuteReg.length,
				 #rtcMinuteReg.address,
				 #rtcMinuteReg.bit_minTen,
				 #rtcMinuteReg.bit_minOne};
			
			rtcAlm0MinReg ->
				{#rtcAlm0MinReg{},
				 #rtcAlm0MinReg.length,
				 #rtcAlm0MinReg.address,
				 #rtcAlm0MinReg.bit_minTen,
				 #rtcAlm0MinReg.bit_minOne};
			
			rtcAlm1MinReg ->
				{#rtcAlm1MinReg{},
				 #rtcAlm1MinReg.length,
				 #rtcAlm1MinReg.address,
				 #rtcAlm1MinReg.bit_minTen,
				 #rtcAlm1MinReg.bit_minOne}
		end,
	
	%% Convert Minute in BCD
    case catch bcd:encode(Minute, 1) of
        <<B1:4, B0:4>> ->
            %% Prepare register that write into the RTC device
            case dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
                                         RegisterRec, 
                                         DataLengthIdx,
                                         RegisterAddressIdx, 
                                         [{BitMinuteTenIdx, B1},
                                          {BitMinuteOneIdx, B0}]) of
                {ok,_} ->
                    %%?DO_INFO("Minute has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Minute in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end.

%% ====================================================================
%% @doc
%% Setup Second in RTC device.
%% @end
 -spec do_second_set(atom(), second()) -> ok | {error, term()}.
%% ====================================================================
do_second_set(RegType, Second) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitSecTenIdx, BitSecOneIdx} =
		case RegType of
			rtcSecondReg ->
				{#rtcSecondReg{},
				 #rtcSecondReg.length,
				 #rtcSecondReg.address,
				 #rtcSecondReg.bit_secTen,
				 #rtcSecondReg.bit_secOne};
			
			rtcAlm0SecReg ->
				{#rtcAlm0SecReg{},
				 #rtcAlm0SecReg.length,
				 #rtcAlm0SecReg.address,
				 #rtcAlm0SecReg.bit_secTen,
				 #rtcAlm0SecReg.bit_secOne};
			
			rtcAlm1SecReg ->
				{#rtcAlm1SecReg{},
				 #rtcAlm1SecReg.length,
				 #rtcAlm1SecReg.address,
				 #rtcAlm1SecReg.bit_secTen,
				 #rtcAlm1SecReg.bit_secOne}
		end,
	
	%% Convert Second in BCD
    case catch bcd:encode(Second, 1) of
        <<B1:4, B0:4>> ->
            %% Prepare register that write into the RTC device.
            case dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
                                         RegisterRec,
                                         DataLengthIdx, 
                                         RegisterAddressIdx, 
                                         [{BitSecTenIdx, B1},
                                          {BitSecOneIdx, B0}]) of
                {ok,_} ->
                    %%?DO_INFO("Second has been set in RTC", [{regType, RegType}]),
                    ok;
                ER->
                    ?DO_ERR("Faild to set Second in RTC", [{regType, RegType}, {reason, ER}]),
                    ER
            end;
        ER ->
            {error, ER}
    end.

%% ====================================================================
%% @doc
%% Setup Week Day in RTC device.
%% @end
 -spec do_wday_set(atom(), wday()) -> ok | {error, term()}.
%% ====================================================================
do_wday_set(RegType, WDay) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitWDayIdx} =
		case RegType of
			rtcWkDayReg ->
				{#rtcWkDayReg{},
				 #rtcWkDayReg.length,
				 #rtcWkDayReg.address,
				 #rtcWkDayReg.bit_wDay};
			
			rtcAlm0WDayReg ->
				{#rtcAlm0WDayReg{},
				 #rtcAlm0WDayReg.length,
				 #rtcAlm0WDayReg.address,
				 #rtcAlm0WDayReg.bit_wDay};
			
			rtcAlm1WDayReg ->
				{#rtcAlm1WDayReg{},
				 #rtcAlm1WDayReg.length,
				 #rtcAlm1WDayReg.address,
				 #rtcAlm1WDayReg.bit_wDay}
		end,
	
	%% Prepare register that write into the RTC device.
	case catch dev_common:bitfield_set(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
								 RegisterRec, 
								 DataLengthIdx,
								 RegisterAddressIdx, 
								 [{BitWDayIdx, WDay}]) of
		{ok,_} ->
			%%?DO_INFO("WDay has been set in RTC", [{regType, RegType}]),
			ok;
		ER->
			?DO_ERR("Faild to set WDay in RTC", [{regType, RegType}, {reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Read Date and Time in RTC device.
%% @end
-spec do_date_and_time_get() -> {ok, datetime()} | {error, term()}.
%% ====================================================================
do_date_and_time_get() ->
	case catch do_year_get(rtcYearReg) of
		{ok, Year} ->
			case catch do_month_get(rtcMonthReg) of
				{ok, Month} ->
					case catch do_date_get(rtcDateReg) of
						{ok, Date} ->
							case catch do_hour_get(rtcHourReg) of
								{ok, Hour} ->
									case catch do_minute_get(rtcMinuteReg) of
										{ok, Minute} ->
											case catch do_second_get(rtcSecondReg) of
												{ok, Second} ->
													Time = {Hour,Minute,Second},
													FullDate = {Year,Month,Date},
													
													{ok, {FullDate, Time}};
													
												ER->ER
											end;
										ER->ER
									end;
								ER->ER
							end;
						ER->ER
					end;
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Read Date and Time in RTC device and convert it to string
%% @end
-spec do_date_and_time_get_in_str() -> {ok, list()} | {error, term()}.
%% ====================================================================
do_date_and_time_get_in_str() ->
	case catch do_date_and_time_get() of
		{ok, {{Year,Month,Date}, {Hour,Minute,Second}}} ->
			do_date_and_time_get_in_str({{Year,Month,Date}, {Hour,Minute,Second}});
		{error, Reason} ->
			{error, Reason};
        ER ->
            {error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Date and Time in RTC device and convert it to string
%% @end
-spec do_date_and_time_get_in_str(datetime() | {ok, datetime()}) -> {ok, list()} | {error, term()}.
%% ====================================================================
do_date_and_time_get_in_str({ok, DateAndTime}) ->
	do_date_and_time_get_in_str(DateAndTime);
do_date_and_time_get_in_str(DateAndTime) ->
	case DateAndTime of
		{{Year,Month,Date}, {Hour,Minute,Second}} ->
			case catch do_date_get_in_str({Year,Month,Date}) of
				{ok, DateStr} ->
					case catch do_time_get_in_str({Hour,Minute,Second}) of
						{ok, TimeStr} ->
							DateTimeStr = concat_string([DateStr, TimeStr], ?DATE_AND_TIME_STR_DELIMITER),
							{ok, DateTimeStr};
						{error, ER} ->
							{error, ER}
					end;
				{error, ER} ->
					{error, ER};
                ER ->
                    {error, ER}
			end;
		_ ->
			{error, {"Invalid DateAndTime stamp", DateAndTime}}
	end.

%% ====================================================================
%% @doc
%% Convert date to string
%% @end
-spec do_date_get_in_str(date() | {ok, date()}) -> {ok, list()} | {error, term()}.
%% ====================================================================
do_date_get_in_str({ok, Date}) ->
	do_date_get_in_str(Date);
do_date_get_in_str(Date) ->
	case Date of
		{Year, Month, Day} ->
			DateStr = concat_string([integer_to_list(Year, 4, "0"), integer_to_list(Month, 2, "0"), integer_to_list(Day, 2, "0")], ?DATE_STR_DELIMITER),
			{ok, DateStr};
		_->	{error, {"Invalid Date", Date}}
	end.

%% ====================================================================
%% @doc
%% Convert time to string
%% @end
-spec do_time_get_in_str(time() | {ok, time()}) -> {ok, list()} | {error, term()}.
%% ====================================================================
do_time_get_in_str({ok, Time}) ->
	do_time_get_in_str(Time);
do_time_get_in_str(Time) ->
	case Time of
		{{_AmPm, Hour},Minute,Second} ->
			%% 12H mode
			TimeStr = concat_string([integer_to_list(Hour, 2, "0"), integer_to_list(Minute, 2, "0"), integer_to_list(Second, 2, "0")], ?TIME_STR_DELIMITER),
			{ok, TimeStr};
		
		{Hour,Minute,Second} ->
			%% 24H mode
			TimeStr = concat_string([integer_to_list(Hour, 2, "0"), integer_to_list(Minute, 2, "0"), integer_to_list(Second, 2, "0")], ?TIME_STR_DELIMITER),
			{ok, TimeStr};
		_->	{error, {"Invalid Time", Time}}
	end.
	
%% ====================================================================
%% @doc
%% Read Year in RTC device.
%% @end
 -spec do_year_get(atom()) -> {ok, year()} | {error, term()}.
%% ====================================================================
do_year_get(RegType) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitFieldIdx1, BitFieldIdx2} = 
		case RegType of
			rtcYearReg ->
				{#rtcYearReg{},
				 #rtcYearReg.length,
				 #rtcYearReg.address,
				 #rtcYearReg.bit_yearTen,
				 #rtcYearReg.bit_yearOne}
		end,
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitFieldIdx1, BitFieldIdx2]) of
		[{BitFieldIdx1, B1}, {BitFieldIdx2, B0}] ->
			{ok, bcd:decode(<<B1:4, B0:4>>, 1)+?BASE_YEAR};
		ER->
			?DO_ERR("Faild to read Year in RTC", [{regType, RegType}, {reason, ER}]),
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Month in RTC device.
%% @end
 -spec do_month_get(atom()) -> {ok, month()} | {error, term()}.
%% ====================================================================
do_month_get(RegType) ->
	{RegisterRec, 
	 DataLengthIdx,
	 RegisterAddressIdx, 
	 BitFieldIdx1, 
	 BitFieldIdx2} = case RegType of
						  rtcMonthReg ->
							  {#rtcMonthReg{},
							   #rtcMonthReg.length,
							   #rtcMonthReg.address,
							   #rtcMonthReg.bit_monthTen,
							   #rtcMonthReg.bit_monthOne};
						  
						  rtcPwrDNMonthReg ->
							  {#rtcPwrDNMonthReg{}, 
							   #rtcPwrDNMonthReg.length,
							   #rtcPwrDNMonthReg.address, 
							   #rtcPwrDNMonthReg.bit_monthTen, 
							   #rtcPwrDNMonthReg.bit_monthOne};
						  
						  rtcPwrUPMonthReg ->
							  {#rtcPwrUPMonthReg{}, 
							   #rtcPwrUPMonthReg.length,
							   #rtcPwrUPMonthReg.address, 
							   #rtcPwrUPMonthReg.bit_monthTen, 
							   #rtcPwrUPMonthReg.bit_monthOne};
						 
						 rtcAlm0MonthReg ->
							 {#rtcAlm0MonthReg{},
							  #rtcAlm0MonthReg.length,
							  #rtcAlm0MonthReg.address,
							  #rtcAlm0MonthReg.bit_monthTen,
							  #rtcAlm0MonthReg.bit_monthOne};
						 
						 rtcAlm1MonthReg ->
							 {#rtcAlm1MonthReg{},
							  #rtcAlm1MonthReg.length,
							  #rtcAlm1MonthReg.address,
							  #rtcAlm1MonthReg.bit_monthTen,
							  #rtcAlm1MonthReg.bit_monthOne}
					 end,
	
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitFieldIdx1, BitFieldIdx2]) of
		[{BitFieldIdx1, B1}, {BitFieldIdx2, B0}] ->
			{ok, bcd:decode(<<B1:4, B0:4>>, 1)};
		ER->
			?DO_ERR("Faild to read Month in RTC", [{regType, RegType}, {reason, ER}]),
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Date in RTC device.
%% @end
 -spec do_date_get(atom()) -> {ok, day()} | {error, term()}.
%% ====================================================================
do_date_get(RegType) ->
	{RegisterRec, 
	 DataLengthIdx,
	 RegisterAddressIdx, 
	 BitFieldIdx1, 
	 BitFieldIdx2} = case RegType of
						  rtcDateReg ->
							  {#rtcDateReg{},
							   #rtcDateReg.length,
							   #rtcDateReg.address,
							   #rtcDateReg.bit_dateTen,
							   #rtcDateReg.bit_dateOne};
						  
						  rtcPwrDNDateReg ->
							  {#rtcPwrDNDateReg{},
							   #rtcPwrDNDateReg.length, 
							   #rtcPwrDNDateReg.address, 
							   #rtcPwrDNDateReg.bit_dateTen, 
							   #rtcPwrDNDateReg.bit_dateOne};
						  
						  rtcPwrUPDateReg ->
							  {#rtcPwrUPDateReg{}, 
							   #rtcPwrUPDateReg.length,
							   #rtcPwrUPDateReg.address, 
							   #rtcPwrUPDateReg.bit_dateTen, 
							   #rtcPwrUPDateReg.bit_dateOne};
						 
						 rtcAlm0DateReg ->
							{#rtcAlm0DateReg{},
							 #rtcAlm0DateReg.length,
							 #rtcAlm0DateReg.address,
							 #rtcAlm0DateReg.bit_dateTen,
							 #rtcAlm0DateReg.bit_dateOne};
						 
						 rtcAlm1DateReg ->
							{#rtcAlm1DateReg{},
							 #rtcAlm1DateReg.length,
							 #rtcAlm1DateReg.address,
							 #rtcAlm1DateReg.bit_dateTen,
							 #rtcAlm1DateReg.bit_dateOne}
					  end,
	
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitFieldIdx1, BitFieldIdx2]) of
		[{BitFieldIdx1, B1}, {BitFieldIdx2, B0}] ->
			{ok, bcd:decode(<<B1:4, B0:4>>, 1)};
		ER->
			?DO_ERR("Faild to read Date in RTC", [{regType, RegType}, {reason, ER}]),
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Hour in RTC device.
%% @end
 -spec do_hour_get(atom()) -> {ok, {hour_am() | hour_pm(), hour12()}} | {ok, hour()} | {error, term()}.
%% ====================================================================
do_hour_get(RegType) ->
	{RegisterRec, 
	 DataLengthIdx,
	 RegisterAddressIdx, 
	 BitTimeFormatIdx, 
	 Bit12H_AMPM_Idx, 
	 Bit12H_HrTen_Idx, 
	 Bit12H_HrOne_Idx, 
	 Bit24H_HrTen_Idx, 
	 Bit24H_HrOne_Idx} =
		case RegType of
			rtcHourReg ->
				{#rtcHourReg{},
				 #rtcHourReg.length,
				 #rtcHourReg.address,
				 #rtcHourReg.bit_timeFormat,
				 #rtcHourReg.bit_12h_ampm,
				 #rtcHourReg.bit_12h_hrTen,
				 #rtcHourReg.bit_12h_hrOne,
				 #rtcHourReg.bit_24h_hrTen,
				 #rtcHourReg.bit_24h_hrOne};
			
			rtcPwrDNHourReg ->
				{#rtcPwrDNHourReg{},
				 #rtcPwrDNHourReg.length,
				 #rtcPwrDNHourReg.address,
				 #rtcPwrDNHourReg.bit_timeFormat,
				 #rtcPwrDNHourReg.bit_12h_ampm,
				 #rtcPwrDNHourReg.bit_12h_hrTen,
				 #rtcPwrDNHourReg.bit_12h_hrOne,
				 #rtcPwrDNHourReg.bit_24h_hrTen,
				 #rtcPwrDNHourReg.bit_24h_hrOne};
			
			rtcPwrUPHourReg ->
				{#rtcPwrUPHourReg{},
				 #rtcPwrUPHourReg.length,
				 #rtcPwrUPHourReg.address,
				 #rtcPwrUPHourReg.bit_timeFormat,
				 #rtcPwrUPHourReg.bit_12h_ampm,
				 #rtcPwrUPHourReg.bit_12h_hrTen,
				 #rtcPwrUPHourReg.bit_12h_hrOne,
				 #rtcPwrUPHourReg.bit_24h_hrTen,
				 #rtcPwrUPHourReg.bit_24h_hrOne};
			
			rtcAlm0HourReg ->
				{#rtcAlm0HourReg{},
				 #rtcAlm0HourReg.length,
				 #rtcAlm0HourReg.address,
				 #rtcAlm0HourReg.bit_timeFormat,
				 #rtcAlm0HourReg.bit_12h_ampm,
				 #rtcAlm0HourReg.bit_12h_hrTen,
				 #rtcAlm0HourReg.bit_12h_hrOne,
				 #rtcAlm0HourReg.bit_24h_hrTen,
				 #rtcAlm0HourReg.bit_24h_hrOne};

			rtcAlm1HourReg ->
				{#rtcAlm1HourReg{},
				 #rtcAlm1HourReg.length,
				 #rtcAlm1HourReg.address,
				 #rtcAlm1HourReg.bit_timeFormat,
				 #rtcAlm1HourReg.bit_12h_ampm,
				 #rtcAlm1HourReg.bit_12h_hrTen,
				 #rtcAlm1HourReg.bit_12h_hrOne,
				 #rtcAlm1HourReg.bit_24h_hrTen,
				 #rtcAlm1HourReg.bit_24h_hrOne}
		end,
	
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitTimeFormatIdx]) of
		[{BitTimeFormatIdx, TimeFormat}] ->
			case TimeFormat of
				?RTC_HOUR_BIT_TIME_FORMAT_12H ->
					case bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [Bit12H_AMPM_Idx, Bit12H_HrTen_Idx, Bit12H_HrOne_Idx]) of
						[{Bit12H_AMPM_Idx, B2}, {Bit12H_HrTen_Idx, B1}, {Bit12H_HrOne_Idx, B0}] ->
							{ok, {B2, bcd:decode(<<B1:4, B0:4>>, 1)}};
						ER->
							?DO_ERR("Faild to read Hour in RTC", [{regType, RegType}, {reason, ER}]),
							{error, ER}
					end;
				?RTC_HOUR_BIT_TIME_FORMAT_24H ->
					case bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [Bit24H_HrTen_Idx, Bit24H_HrOne_Idx]) of
						[{Bit24H_HrTen_Idx, B1}, {Bit24H_HrOne_Idx, B0}] ->
							{ok, bcd:decode(<<B1:4, B0:4>>, 1)};
						ER->
							?DO_ERR("Faild to read Hour in RTC", [{regType, RegType}, {reason, ER}]),
							{error, ER}
					end
			end;
		ER->
			?DO_ERR("Faild to read Hour in RTC", [{regType, RegType}, {reason, ER}]),
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Minute in RTC device.
%% @end
 -spec do_minute_get(atom()) -> {ok, minute()} | {error, term()}.
%% ====================================================================
do_minute_get(RegType) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitMinTenIdx, BitMinOneIdx} = 
		case RegType of
			rtcMinuteReg ->
				{#rtcMinuteReg{},
				 #rtcMinuteReg.length,
				 #rtcMinuteReg.address,
				 #rtcMinuteReg.bit_minTen,
				 #rtcMinuteReg.bit_minOne};
			
			rtcPwrDNMinuteReg ->
				{#rtcPwrDNMinuteReg{},
				 #rtcPwrDNMinuteReg.length,
				 #rtcPwrDNMinuteReg.address,
				 #rtcPwrDNMinuteReg.bit_minTen,
				 #rtcPwrDNMinuteReg.bit_minOne};
			
			rtcPwrUPMinuteReg ->
				{#rtcPwrUPMinuteReg{},
				 #rtcPwrUPMinuteReg.length,
				 #rtcPwrUPMinuteReg.address,
				 #rtcPwrUPMinuteReg.bit_minTen,
				 #rtcPwrUPMinuteReg.bit_minOne};
			
			rtcAlm0MinReg ->
				{#rtcAlm0MinReg{},
				 #rtcAlm0MinReg.length,
				 #rtcAlm0MinReg.address,
				 #rtcAlm0MinReg.bit_minTen,
				 #rtcAlm0MinReg.bit_minOne};
			
			rtcAlm1MinReg ->
				{#rtcAlm1MinReg{},
				 #rtcAlm1MinReg.length,
				 #rtcAlm1MinReg.address,
				 #rtcAlm1MinReg.bit_minTen,
				 #rtcAlm1MinReg.bit_minOne}
		end,
	
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitMinTenIdx, BitMinOneIdx]) of
		[{BitMinTenIdx, B1}, {BitMinOneIdx, B0}] ->
			{ok, bcd:decode(<<B1:4, B0:4>>, 1)};
		ER->
			?DO_ERR("Faild to read Minute in RTC", [{regType, RegType}, {reason, ER}]),
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Second in RTC device.
%% @end
 -spec do_second_get(atom()) -> {ok, second()} | {error, term()}.
%% ====================================================================
do_second_get(RegType) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitSecTenIdx, BitSecOneIdx} =
		case RegType of
			rtcSecondReg ->
				{#rtcSecondReg{},
				 #rtcSecondReg.length,
				 #rtcSecondReg.address,
				 #rtcSecondReg.bit_secTen,
				 #rtcSecondReg.bit_secOne};
			
			rtcAlm0SecReg ->
				{#rtcAlm0SecReg{},
				 #rtcAlm0SecReg.length,
				 #rtcAlm0SecReg.address,
				 #rtcAlm0SecReg.bit_secTen,
				 #rtcAlm0SecReg.bit_secOne};
			
			rtcAlm1SecReg ->
				{#rtcAlm1SecReg{},
				 #rtcAlm1SecReg.length,
				 #rtcAlm1SecReg.address,
				 #rtcAlm1SecReg.bit_secTen,
				 #rtcAlm1SecReg.bit_secOne}
		end,
	
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitSecTenIdx, BitSecOneIdx]) of
		[{BitSecTenIdx, B1}, {BitSecOneIdx, B0}] ->
			{ok, bcd:decode(<<B1:4, B0:4>>, 1)};
		ER->
			?DO_ERR("Faild to read Second in RTC", [{regType, RegType}, {reason, ER}]),
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read Week Day in RTC device.
%% @end
 -spec do_wday_get(atom()) -> {ok, wday()} | {error, term()}.
%% ====================================================================
do_wday_get(RegType) ->
	{RegisterRec, DataLengthIdx, RegisterAddressIdx, BitWDayIdx} =
		case RegType of
			rtcWkDayReg ->
				{#rtcWkDayReg{},
				 #rtcWkDayReg.length,
				 #rtcWkDayReg.address,
				 #rtcWkDayReg.bit_wDay};
			
			rtcPwrDNMonthReg ->
			   {#rtcPwrDNMonthReg{},
				#rtcPwrDNMonthReg.length,
				#rtcPwrDNMonthReg.address,
				#rtcPwrDNMonthReg.bit_wDay};
			
			rtcPwrUPMonthReg ->
			   {#rtcPwrUPMonthReg{},
				#rtcPwrUPMonthReg.length,
				#rtcPwrUPMonthReg.address,
				#rtcPwrUPMonthReg.bit_wDay};
			
			rtcAlm0WDayReg ->
				{#rtcAlm0WDayReg{},
				 #rtcAlm0WDayReg.length,
				 #rtcAlm0WDayReg.address,
				 #rtcAlm0WDayReg.bit_wDay};
			
			rtcAlm1WDayReg ->
				{#rtcAlm1WDayReg{},
				 #rtcAlm1WDayReg.length,
				 #rtcAlm1WDayReg.address,
				 #rtcAlm1WDayReg.bit_wDay}
		end,
	
	case catch bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, [BitWDayIdx]) of
		[{BitWDayIdx, B0}] ->
			{ok, B0};
		ER->
			?DO_ERR("Faild to read WDay in RTC", [{regType, RegType}, {reason, ER}]),
			ER
	end.

%% ====================================================================
%% @doc
%% Convert Hour in 24H time format to 12H format.
%% @end
-spec hour_convert_to_12h_format(hour()) -> {hour_ampm_ind(), hour12()}.
%% ====================================================================
hour_convert_to_12h_format(Hour) ->
	case Hour of
		H when H =< 11 ->
			{?RTC_HOUR_BIT_12H_AMPM_IND_AM, Hour};
		H when H == 12 ->
			{?RTC_HOUR_BIT_12H_AMPM_IND_PM, Hour};
		H ->
			{?RTC_HOUR_BIT_12H_AMPM_IND_PM, H-12}
	end.

%% ====================================================================
%% @doc
%% Convert Hour in 12H time format to 24H format.
%% @end
-spec hour_convert_to_24h_format(hour_ampm_ind(), hour12()) -> hour().
%% ====================================================================
hour_convert_to_24h_format(AMPM, Hour) ->
	case AMPM of
		?RTC_HOUR_BIT_12H_AMPM_IND_AM ->
			case Hour of
				H when H == 12 ->
					0;
				H ->
					H
			end;
		?RTC_HOUR_BIT_12H_AMPM_IND_PM ->
			case Hour of
				H when H == 12 ->
					12;
				H ->
					H+12
			end
	end.

%% ====================================================================
%% Read a register in RTC device
%% Input:
%%		RegAddr	:	byte
%% Output:
%%		{ok,Value} | {error, Reason}
%% ====================================================================
read(RegAddr) ->
	dev_common:i2c_read(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, RegAddr, ?NUMBER_OF_BYTE_TO_READ).

%% ====================================================================
%% Write a register in RTC device
%% Input:
%%		RegAddr	:	byte
%%		Value	:	byte
%% Output:
%%		ok | {error, Reason}
%% ====================================================================
write(RegAddr,Value) ->
	dev_common:i2c_write(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, RegAddr, Value).

%% ====================================================================
%% @doc
%% Init a gen_server call.
%% @end
-spec do_gen_server_call(tuple()) -> term().
%% ====================================================================
do_gen_server_call(MSG) ->
	case whereis(?SERVER) of
		P when is_pid(P) ->
			gen_server:call(?SERVER, MSG, ?GEN_SERVER_CALL_TO);
		ER->{error, ER}
	end.

%% ====================================================================
%% Concat strings.
%% Input:
%%		StringList	:	list(string())
%%		Delimiter	:	string()
%% Output:
%%		String		:	string()
%% ====================================================================
concat_string(StringList, Delimiter) ->
	concat_string_loop(StringList, Delimiter, "").

concat_string_loop([], _Delimiter, String) ->
	String;
concat_string_loop([Str | T], Delimiter, String) when String == "" ->
	%% Str is the 1st element in the list
	concat_string_loop(T, Delimiter, Str);
concat_string_loop([Str | T], Delimiter, String) ->
	Str1 = string:concat(String, Delimiter),
	Str2 = string:concat(Str1, Str),
	concat_string_loop(T, Delimiter, Str2).

%% ====================================================================
%% Convert integer to string with set the length of final string,
%% so if the converted string is "2" but the wanted length of string is
%% 2, the result will be "02".
-spec integer_to_list(integer(), integer(), list()) -> list().
%% ====================================================================
integer_to_list(N, Length, Pad) ->
	NStr = erlang:integer_to_list(N),
	PadLength = case string:len(NStr) of
					I when I < Length ->
						Length - I;
					_-> 0
				end,
	PrefixStr = string:copies(Pad,PadLength),
	string:concat(PrefixStr, NStr).

%% ====================================================================
%% Read specified bit field(s) in the device.
-spec bitfield_get(
		RegisterRec :: register_rec(),
		DataLengthIdx :: integer(),
		{regValue, int_data()} | {addrIdx, integer()}, 
		BitFieldIdx :: integer() | list(integer())) -> list({integer(), int_data()}) | {error, term()}.
%% ====================================================================
bitfield_get(RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, BitFieldIdx) ->
	dev_common:bitfield_get(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
							RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, BitFieldIdx);
bitfield_get(RegisterRec, DataLengthIdx, {regValue, RegisterValue}, BitFieldIdx) ->
	dev_common:bitfield_get(?RTC_COMMUNICATION_DEVICENAME, ?RTC_ADDRESS, 
							RegisterRec, DataLengthIdx, {regValue, RegisterValue}, BitFieldIdx).

