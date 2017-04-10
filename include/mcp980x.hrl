%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2016, Robert Balogh
%% @doc
%% This header file contains defines, records, ect.
%% for MCP980x Temperature sensor.
%% @end

-ifndef(MCP980X_HRL).
-define(MCP980X_HRL,true).

%% ====================================================================
%% Includes
%% ====================================================================
-include("ale_common.hrl").
-include("dev_common.hrl").

%% ====================================================================
%% Communicatoin device name to be used for data transfer
%% ====================================================================
-define(MCP980X_COMMUNICATION_DEVICENAME, ?I2C_DEVICE_CH1_NAME).

%% ====================================================================
%% Register addresses
%% ====================================================================
-define(REG_POINTER_TEMPARATURE_REG,		2#0000000).
-define(REG_POINTER_CONFIGURATION_REG,		2#0000001).
-define(REG_POINTER_TEMPERATURE_HYST_REG,	2#0000010).
-define(REG_POINTER_TEMPERATURE_LIMIT_REG,	2#0000011).

%% ====================================================================
%% Sign of temperature
%% ====================================================================
-define(TEMPERATURE_VAL_POSITIVE, 0).
-define(TEMPERATURE_VAL_NEGATIVE, 1).

%% ====================================================================
%% CONFIGURATION REGISTER
%% ====================================================================
-define(CONFIGURATION_REG_ONE_SHOT_EN,			1).
-define(CONFIGURATION_REG_ONE_SHOT_DIS,			0).
-define(CONFIGURATION_REG_ONE_SHOT,				[?CONFIGURATION_REG_ONE_SHOT_EN, ?CONFIGURATION_REG_ONE_SHOT_DIS]).
-define(CONFIGURATION_REG_ONE_SHOT_MASK,		2#10000000).
-type oneshot_cfg_bit()	::	?CONFIGURATION_REG_ONE_SHOT_EN | ?CONFIGURATION_REG_ONE_SHOT_DIS.

-define(CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,		2#00).
-define(CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C,	2#01).
-define(CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C,	2#10).
-define(CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C,	2#11).
-define(CONFIGURATION_REG_ADC_RESOLUTION,		[?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C, 
												 ?CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C,
												 ?CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C,
												 ?CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C]).
-define(CONFIGURATION_REG_ADC_RESOLUTION_BIT_LENGHT, [
													  {?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C, 9},
													  {?CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C, 10},
													  {?CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C, 11},
													  {?CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C, 12}
													  ]).

-type adcres_cfg_bit()	::	?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C | 
		  					?CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C |
		  					?CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C | 
		  					?CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C.

-define(CONFIGURATION_REG_ADC_RESOLUTION_MASK,	2#01100000).
-define(CONFIGURATION_REG_ADC_RESOLUTION_TEMP_ID_KEYPOS, 1).
-define(CONFIGURATION_REG_ADC_RESOLUTION_TEMP_MULTIPLIER_KEYPOS, 2).
-define(CONFIGURATION_REG_ADC_RESOLUTION_TEMP_LABEL_KEYPOS, 3).
-define(CONFIGURATION_REG_ADC_RESOLUTION_TEMP_VALUE_LIST, 
		[
		 {?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C, 0.5, "0.5"},
		 {?CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C, 0.25, "0.25"},
		 {?CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C, 0.125, "0.125"},
		 {?CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C, 0.0625, "0.0625"}
		 ]).

-define(CONFIGURATION_REG_FAULT_QUEUE_1,	2#00).
-define(CONFIGURATION_REG_FAULT_QUEUE_2,	2#01).
-define(CONFIGURATION_REG_FAULT_QUEUE_4,	2#10).
-define(CONFIGURATION_REG_FAULT_QUEUE_6,	2#11).
-type faultqueue_cfg_bit()	::	
		  ?CONFIGURATION_REG_FAULT_QUEUE_1 |
		  ?CONFIGURATION_REG_FAULT_QUEUE_2 |
		  ?CONFIGURATION_REG_FAULT_QUEUE_4 |
		  ?CONFIGURATION_REG_FAULT_QUEUE_6.
-define(CONFIGURATION_REG_FAULT_QUEUE,	[?CONFIGURATION_REG_FAULT_QUEUE_1,
										 ?CONFIGURATION_REG_FAULT_QUEUE_2,
										 ?CONFIGURATION_REG_FAULT_QUEUE_4,
										 ?CONFIGURATION_REG_FAULT_QUEUE_6]).
-define(CONFIGURATION_REG_FAULT_QUEUE_MASK,		2#00011000).
-define(CONFIGURATION_REG_FAULT_QUEUE_ID_KEYPOS, 1).
-define(CONFIGURATION_REG_FAULT_QUEUE_LABEL_KEYPOS, 2).
-define(CONFIGURATION_REG_FAULT_QUEUE_LIST, 
		[
		 {?CONFIGURATION_REG_FAULT_QUEUE_1, "1"},
		 {?CONFIGURATION_REG_FAULT_QUEUE_2, "2"},
		 {?CONFIGURATION_REG_FAULT_QUEUE_4, "4"},
		 {?CONFIGURATION_REG_FAULT_QUEUE_6, "6"}
		 ]).

-define(CONFIGURATION_REG_ALERT_POL_ACT_HIGH_LBL,	"active-high").
-define(CONFIGURATION_REG_ALERT_POL_ACT_LOW_LBL,	"active-low").
-define(CONFIGURATION_REG_ALERT_POL_ACT_HIGH,	1).
-define(CONFIGURATION_REG_ALERT_POL_ACT_LOW,	0).
-define(CONFIGURATION_REG_ALERT_POL,			[?CONFIGURATION_REG_ALERT_POL_ACT_HIGH,
												 ?CONFIGURATION_REG_ALERT_POL_ACT_LOW]).
-define(CONFIGURATION_REG_ALERT_POL_MASK,		2#00000100).
-type alertpol_cfg_bit()	:: ?CONFIGURATION_REG_ALERT_POL_ACT_HIGH | ?CONFIGURATION_REG_ALERT_POL_ACT_LOW.

-define(CONFIGURATION_REG_INT_MODE_LBL,			"interrupt").
-define(CONFIGURATION_REG_COMP_MODE_LBL,		"comparator").
-define(CONFIGURATION_REG_INT_MODE,				1).
-define(CONFIGURATION_REG_COMP_MODE,			0).
-define(CONFIGURATION_REG_MODE,					[?CONFIGURATION_REG_INT_MODE,
												 ?CONFIGURATION_REG_COMP_MODE]).
-define(CONFIGURATION_REG_MODE_MASK,			2#00000010).
-type mode_cfg_bit()	:: ?CONFIGURATION_REG_INT_MODE | ?CONFIGURATION_REG_COMP_MODE.

-define(CONFIGURATION_REG_SHUTDOWN_EN,			1).
-define(CONFIGURATION_REG_SHUTDOWN_DIS,			0).
-define(CONFIGURATION_REG_SHUTDOWN,				[?CONFIGURATION_REG_SHUTDOWN_EN,
												 ?CONFIGURATION_REG_SHUTDOWN_DIS]).
-define(CONFIGURATION_REG_SHUTDOWN_MASK,		2#00000001).
-type shutdown_cfg_bit()	::	?CONFIGURATION_REG_SHUTDOWN_EN | ?CONFIGURATION_REG_SHUTDOWN_DIS.

-define(MCP980x_TEMP_SIGN_MINUS, 1).
-define(MCP980x_TEMP_SIGN_PLUS, 0).
-type temperature_sing_minus() 	::	?MCP980x_TEMP_SIGN_MINUS.
-type temperature_sing_plus()	::	?MCP980x_TEMP_SIGN_PLUS.
-type temperature_sing()		::	{temperature_sing_minus(), temperature_sing_plus()}.


-define(MCP980x_TEMP_MAX_MINUS_VALUE, 55). %% The max ambient temperature when the device is powered ON in unit [C]
-define(MCP980x_TEMP_MAX_PLUS_VALUE, 125). %% The max ambient temperature when the device is powered ON in unit [C]
-type temperature_range_sign_minus():: lists:reverse(lists:seq(0, ?MCP980x_TEMP_MAX_MINUS_VALUE)).
-type temperature_range_sign_plus()	:: lists:seq(0, ?MCP980x_TEMP_MAX_PLUS_VALUE).
-type temperature_value()			:: temperature_range_sign_minus() | temperature_range_sign_plus().

-record(mcp980xConfigReg, 
		{
		 address = ?REG_POINTER_CONFIGURATION_REG,
		 length = 1,
         bit_OneShot = #bitParam{value = ?CONFIGURATION_REG_ONE_SHOT, defValue = ?CONFIGURATION_REG_ONE_SHOT_DIS, mask = ?CONFIGURATION_REG_ONE_SHOT_MASK, doshiftvalue = true},
		 bit_AdcResolution = #bitParam{value = ?CONFIGURATION_REG_ADC_RESOLUTION, defValue = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C, mask = ?CONFIGURATION_REG_ADC_RESOLUTION_MASK, doshiftvalue = true},
         bit_FaultQueue = #bitParam{value = ?CONFIGURATION_REG_FAULT_QUEUE, defValue = 1, mask = ?CONFIGURATION_REG_FAULT_QUEUE_MASK, doshiftvalue = true},
		 bit_AlertPol = #bitParam{value = ?CONFIGURATION_REG_ALERT_POL, defValue = ?CONFIGURATION_REG_ALERT_POL_ACT_HIGH, mask = ?CONFIGURATION_REG_ALERT_POL_MASK, doshiftvalue = true},
		 bit_Mode = #bitParam{value = ?CONFIGURATION_REG_MODE, defValue = ?CONFIGURATION_REG_COMP_MODE, mask = ?CONFIGURATION_REG_MODE_MASK, doshiftvalue = true},
		 bit_ShutDown = #bitParam{value = ?CONFIGURATION_REG_SHUTDOWN, defValue = ?CONFIGURATION_REG_SHUTDOWN_DIS, mask = ?CONFIGURATION_REG_SHUTDOWN_MASK, doshiftvalue = true}
		}).

-record(mcp980xAmbientTemperatureReg, 
		{
		 address = ?REG_POINTER_TEMPARATURE_REG,
		 length = 2,	%% [byte]
		 bit_Sign = #bitParam{value = {?TEMPERATURE_VAL_POSITIVE, ?TEMPERATURE_VAL_NEGATIVE}, mask = 2#1000000000000000, doshiftvalue = true},
		 bit_TempValue =  #bitParam{value = {0,2047}, mask = 2#0111111111110000, doshiftvalue = true}
		}).

-record(mcp980xTemperatureHystReg, 
		{
		 address = ?REG_POINTER_TEMPERATURE_HYST_REG,
		 length = 2,
		 bit_Sign = #bitParam{value = {?TEMPERATURE_VAL_POSITIVE, ?TEMPERATURE_VAL_NEGATIVE}, defValue = ?TEMPERATURE_VAL_POSITIVE, mask = 2#1000000000000000, doshiftvalue = true},
		 bit_TempValue =  #bitParam{value = {0,511}, defValue = 35, mask = 2#0111111110000000, doshiftvalue = true}
		}).

-define(MCP980x_TEMP_LIMIT_SET_INT_VAL_MIN, 0).
-define(MCP980x_TEMP_LIMIT_SET_INT_VAL_MAX, 127).
-define(MCP980x_TEMP_LIMIT_SET_FRACT_VAL_MIN, 0).
-define(MCP980x_TEMP_LIMIT_SET_FRACT_VAL_MAX, 1).
-record(mcp980xTemperatureLimitSetReg, 
		{
		 address = ?REG_POINTER_TEMPERATURE_LIMIT_REG,
		 length = 2,
		 bit_Sign = #bitParam{value = {?TEMPERATURE_VAL_POSITIVE, ?TEMPERATURE_VAL_NEGATIVE}, defValue = ?TEMPERATURE_VAL_POSITIVE, mask = 2#1000000000000000, doshiftvalue = true},
		 bit_TempValue =  #bitParam{value = {0,511}, defValue = 40, mask = 2#0111111110000000, doshiftvalue = true}
		}).

-endif.