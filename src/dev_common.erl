%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2016, Robert Balogh
%% @doc
%% This module contains common function to be used for handle supported devices.
%% @end


-module(dev_common).

%% ====================================================================
%% API functions
%% ====================================================================
-export([i2c_read/4, i2c_write/3, i2c_write/4]).

%% ====================================================================
%% Read a register in I2C device
-spec i2c_read(
		CommDeviceName :: string(),
		HwAddress :: integer(),
		RegAddr :: integer(),
		NumberOfByteToRead :: integer()) -> {ok, Value :: integer()} | {error, term()}.
%% ====================================================================
i2c_read(CommDeviceName, HwAddress, RegAddr, NumberOfByteToRead) ->
	ale_handler:i2c_write(CommDeviceName, HwAddress, erlang:list_to_binary([RegAddr])),
	timer:sleep(10),
	case ale_handler:i2c_read(CommDeviceName, HwAddress, NumberOfByteToRead) of
		{ok, <<Data>>} ->
			{ok, Data};
		ER->ER
	end.

%% ====================================================================
%% Write a register in I2C device
-spec i2c_write(
		CommDeviceName :: string(),
		HwAddress :: integer(),
		RegAddr :: integer()) -> ok | {error, term()}.
%% ====================================================================
i2c_write(CommDeviceName, HwAddress, RegAddr) ->
	ale_handler:i2c_write(CommDeviceName, HwAddress, erlang:list_to_binary([RegAddr])).

%% ====================================================================
%% Write a register in I2C device
-spec i2c_write(
		CommDeviceName :: string(),
		HwAddress :: integer(),
		RegAddr :: integer(),
		Value :: integer()) -> ok | {error, term()}.
%% ====================================================================
i2c_write(CommDeviceName, HwAddress, RegAddr, Value) ->
	ale_handler:i2c_write(CommDeviceName, HwAddress, erlang:list_to_binary([RegAddr, Value])).

%% ====================================================================
%% Internal functions
%% ====================================================================


