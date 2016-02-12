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

-define(CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,		2#00).
-define(CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C,	2#01).
-define(CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C,	2#10).
-define(CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C,	2#11).
-define(CONFIGURATION_REG_ADC_RESOLUTION,		[?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C, 
												 ?CONFIGURATION_REG_ADC_RESOLUTION_10BIT_025C,
												 ?CONFIGURATION_REG_ADC_RESOLUTION_11BIT_0125C,
												 ?CONFIGURATION_REG_ADC_RESOLUTION_12BIT_00625C]).
-define(CONFIGURATION_REG_ADC_RESOLUTION_MASK,	2#01100000).

-define(CONFIGURATION_REG_FAULT_QUEUE,			[1,2,4,6]).
-define(CONFIGURATION_REG_FAULT_QUEUE_MASK,		2#00011000).

-define(CONFIGURATION_REG_ALERT_POL_ACT_HIGH,	1).
-define(CONFIGURATION_REG_ALERT_POL_ACT_LOW,	0).
-define(CONFIGURATION_REG_ALERT_POL,			[?CONFIGURATION_REG_ALERT_POL_ACT_HIGH,
												 ?CONFIGURATION_REG_ALERT_POL_ACT_LOW]).
-define(CONFIGURATION_REG_ALERT_POL_MASK,		2#00000100).

-define(CONFIGURATION_REG_INT_MODE,				1).
-define(CONFIGURATION_REG_COMP_MODE,			0).
-define(CONFIGURATION_REG_MODE,					[?CONFIGURATION_REG_INT_MODE,
												 ?CONFIGURATION_REG_COMP_MODE]).
-define(CONFIGURATION_REG_MODE_MASK,			2#00000010).

-define(CONFIGURATION_REG_SHUTDOWN_EN,			1).
-define(CONFIGURATION_REG_SHUTDOWN_DIS,			0).
-define(CONFIGURATION_REG_SHUTDOWN,				[?CONFIGURATION_REG_SHUTDOWN_EN,
												 ?CONFIGURATION_REG_SHUTDOWN_DIS]).
-define(CONFIGURATION_REG_SHUTDOWN_MASK,		2#00000001).


-record(mcp980xConfigReg, 
		{
		 address = ?REG_POINTER_CONFIGURATION_REG,
		 length = 1,
         bit_OneShot = #bitParam{value = ?CONFIGURATION_REG_ONE_SHOT, mask = ?CONFIGURATION_REG_ONE_SHOT_MASK, doshiftvalue = true},
		 bit_AdcResolution = #bitParam{value = ?CONFIGURATION_REG_ADC_RESOLUTION, mask = ?CONFIGURATION_REG_ADC_RESOLUTION_MASK, doshiftvalue = true},
         bit_FaultQueue = #bitParam{value = ?CONFIGURATION_REG_FAULT_QUEUE, mask = ?CONFIGURATION_REG_FAULT_QUEUE_MASK, doshiftvalue = true},
		 bit_AlertPol = #bitParam{value = ?CONFIGURATION_REG_ALERT_POL, mask = ?CONFIGURATION_REG_ALERT_POL_MASK, doshiftvalue = true},
		 bit_Mode = #bitParam{value = ?CONFIGURATION_REG_MODE, mask = ?CONFIGURATION_REG_MODE_MASK, doshiftvalue = true},
		 bit_ShutDown = #bitParam{value = ?CONFIGURATION_REG_SHUTDOWN, mask = ?CONFIGURATION_REG_SHUTDOWN_MASK, doshiftvalue = true}
		}).

-record(mcp980xAmbientTemperatureReg, 
		{
		 address = ?REG_POINTER_TEMPARATURE_REG,
		 length = 2,
		 bit_Sign =  #bitParam{value = {?TEMPERATURE_VAL_POSITIVE, ?TEMPERATURE_VAL_NEGATIVE}, mask = 2#10000000, doshiftvalue = true},
		 bit_IntegerValue = #bitParam{value = {0,127}, mask = 2#01111111, doshiftvalue = true},
		 bit_FractionalValue = #bitParam{value = {0,15}, mask = 2#11110000, doshiftvalue = true}
		}).

-record(mcp980xTemperatureHystReg, 
		{
		 address = ?REG_POINTER_TEMPERATURE_HYST_REG,
		 length = 2,
		 bit_Sign =  #bitParam{value = {?TEMPERATURE_VAL_POSITIVE, ?TEMPERATURE_VAL_NEGATIVE}, mask = 2#10000000, doshiftvalue = true},
         bit_IntegerValue = #bitParam{value = {0,127}, mask = 2#01111111, doshiftvalue = true},
		 bit_FractionalValue = #bitParam{value = {0,1}, mask = 2#10000000, doshiftvalue = true}
		}).

-record(mcp980xTemperatureLimitSetReg, 
		{
		 address = ?REG_POINTER_TEMPERATURE_LIMIT_REG,
		 length = 2,
		 bit_Sign =  #bitParam{value = {?TEMPERATURE_VAL_POSITIVE, ?TEMPERATURE_VAL_NEGATIVE}, mask = 2#10000000, doshiftvalue = true},
         bit_IntegerValue = #bitParam{value = {0,127}, mask = 2#01111111, doshiftvalue = true},
		 bit_FractionalValue = #bitParam{value = {0,1}, mask = 2#10000000, doshiftvalue = true}
		}).

-define(DEF_TEMPERATURE_SETTING, [
								  {mcp980xConfigReg, #mcp980xConfigReg{bit_OneShot = ?CONFIGURATION_REG_ONE_SHOT_DIS,
																	   bit_AdcResolution = ?CONFIGURATION_REG_ADC_RESOLUTION_9BIT_05C,
																	   bit_FaultQueue = 1,
																	   bit_AlertPol = ?CONFIGURATION_REG_ALERT_POL_ACT_HIGH,
																	   bit_Mode = ?CONFIGURATION_REG_COMP_MODE,
																	   bit_ShutDown = ?CONFIGURATION_REG_SHUTDOWN_DIS}},
								  {temperatureLimiSet, 80},	%% 80 C
								  {temperatureHyst, 75}		%% 75 C
								  ]).

-endif.