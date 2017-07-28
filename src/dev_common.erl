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
-export([bitfield_set/6, bitfield_set/7, bitfield_get/6]).
-export([bit_set/5, bit_get/4]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("dev_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(SLEEP_AFTER_I2C_WRITE_FOR_READING, 100).

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
	timer:sleep(?SLEEP_AFTER_I2C_WRITE_FOR_READING),
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
		Value :: integer() | binary()) -> ok | {error, term()}.
%% ====================================================================
i2c_write(CommDeviceName, HwAddress, RegAddr, Value) ->
	ale_handler:i2c_write(CommDeviceName, HwAddress, erlang:list_to_binary([RegAddr, Value])).

%% ====================================================================
%% @doc
%% Set bitfield in a byte.
%% @end
-spec bitfield_set(
		CommDeviceName :: string(),
		HwAddress :: integer(),
		RegisterRec :: register_rec(),
		DataLengthIdx :: integer(), 
		RegisterAddressIdx :: integer(), 
		BitfieldList :: list({integer(),int_data()})
				  ) -> {ok, int_data()} | {error, term()}.
%% ====================================================================
bitfield_set(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, BitfieldList) ->
	bitfield_set_loop(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, BitfieldList, "").

bitfield_set_loop(_CommDeviceName, _HwAddress, _RegisterRec, _DataLengthIdx, _RegisterAddressIdx, [], {error, ER}) ->
	{error, ER};
bitfield_set_loop(_CommDeviceName, _HwAddress, _RegisterRec, _DataLengthIdx, _RegisterAddressIdx, [], RegisterNewValue) ->
	{ok, RegisterNewValue};
bitfield_set_loop(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, [{BitFieldIdx, BitFieldNewValue} | T], _RegisterValue) ->
	case bitfield_set(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, BitFieldIdx, BitFieldNewValue) of
		{ok, RegisterNewValue} ->
			bitfield_set_loop(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, T, RegisterNewValue);
		ER->
			bitfield_set_loop(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, [], ER)
	end.
	
%% ====================================================================
%% @doc
%% Set bitfield in a byte.
%% @end
-spec bitfield_set(
		CommDeviceName :: string(),
		HwAddress :: integer(),
		RegisterRec :: register_rec(),
		DataLengthIdx :: integer(),
		RegisterAddressIdx :: integer(), 
		BitFieldIdx :: integer(), 
		BitFieldNewValue :: int_data()) -> {ok, int_data()} | {error, term()}.
%% ====================================================================
bitfield_set(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, RegisterAddressIdx, BitFieldIdx, BitFieldNewValue) ->
	DataLength = erlang:element(DataLengthIdx, RegisterRec),
	
	RegAddr = erlang:element(RegisterAddressIdx, RegisterRec),
	
	%% Find BitParam record in RegisterRec
	BitParam = erlang:element(BitFieldIdx, RegisterRec),
	
	%% Validate BitFieldNewValue
	case bitfield_validate(BitFieldNewValue, BitParam#bitParam.value) of
		true ->
			%% Read the register for getting the current value before update
			case i2c_read(CommDeviceName, HwAddress, RegAddr, DataLength) of
				{ok, RegisterValue} ->
					
					%% If the length of the byte to be read more than 1, the result should be convert into
					%% a integer number. In the case when NumberOfByteToRead > 1, the result will be
					%% << B1, B2, .. >> depending of the NumberOfByteToRead.
					RegisterCurrentValueModified = bit_operations:byte_list_to_integer(RegisterValue),
					
					%% New BitFieldNewValue value looks good.
					RegisterNewValue = bit_set(RegisterCurrentValueModified, DataLength, BitFieldNewValue, BitParam#bitParam.mask, BitParam#bitParam.doshiftvalue),
					RegisterNewValueModified = bit_operations:integer_to_byte_list(RegisterNewValue),
					
					%% Write new value of regoster into the device
					case i2c_write(CommDeviceName, HwAddress, RegAddr, RegisterNewValueModified) of
						ok ->
							{ok, RegisterNewValue};
						ER2->
							ER2
					end;
				ER -> ER
			end;
			
		_-> {error, {"Invalid bitfield value when call bitfield_set/7.", {{module, ?MODULE},
																		  {line, ?LINE},
																		  {registerRec, RegisterRec},
																		  {bitfieldIdx, BitFieldIdx},
																		  {bitfieldValue, BitFieldNewValue},
																		  {valid_bitfield_values, BitParam#bitParam.value}
																		  }}}
	end.

%% ====================================================================
%% @doc
%% Get bitfield in a byte.
%% @end
-spec bitfield_get(
		CommDeviceName :: string(),
		HwAddress :: integer(),
		RegisterRec :: register_rec(),
		DataLengthIdx :: integer(),
		{regValue, int_data()} | {addrIdx, integer()}, 
		BitFieldIdx :: integer() | list(integer())) -> list({integer(), int_data()}) | {error, term()}.
%% ====================================================================
bitfield_get(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, {addrIdx, RegisterAddressIdx}, BitFieldIdx) ->
	%% Read register value
	NumberOfByteToRead = erlang:element(DataLengthIdx, RegisterRec),
	case i2c_read(CommDeviceName, HwAddress, erlang:element(RegisterAddressIdx, RegisterRec), NumberOfByteToRead) of
		{ok, RegisterValue} ->
			
			%% If the length of the byte to be read more than 1, the result should be convert into
			%% a integer number. In the case when NumberOfByteToRead > 1, the result will be
			%% << B1, B2, .. >> depending of the NumberOfByteToRead.
			%%io:format("@@ Start convert binary to integer, ~p~n",[RegisterValue]),
			RegisterValueModified = bit_operations:byte_list_to_integer(RegisterValue),
			%%io:format("@@ Start convert binary to integer - DONE, ~p~n",[{RegisterValue, RegisterValueModified}]),
			
			bitfield_get(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, {regValue, RegisterValueModified}, BitFieldIdx);
		
		ER->ER
	end;
bitfield_get(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, {regValue, RegisterCurrentValue}, BitFieldIdx) when is_integer(BitFieldIdx)->
	bitfield_get(CommDeviceName, HwAddress, RegisterRec, DataLengthIdx, {regValue, RegisterCurrentValue}, [BitFieldIdx]);
bitfield_get(_CommDeviceName, _HwAddress, RegisterRec, DataLengthIdx, {regValue, RegisterCurrentValue}, BitFieldIdxList) when is_list(BitFieldIdxList)->
	bitfield_get_loop(RegisterRec, DataLengthIdx, {regValue, RegisterCurrentValue}, BitFieldIdxList, []).

bitfield_get_loop(_RegisterRec, _DataLengthIdx, {regValue, _RegisterCurrentValue}, [], Result) ->
	Result;
bitfield_get_loop(RegisterRec, DataLengthIdx, {regValue, RegisterCurrentValue}, [BitFieldIdx | T], Result) ->
	%% Find BitParam record in RegisterRec
	BitParam = erlang:element(BitFieldIdx, RegisterRec),
	
	NumberOfByteToRead = erlang:element(DataLengthIdx, RegisterRec),
	BitFieldValue = bit_get(RegisterCurrentValue, NumberOfByteToRead, BitParam#bitParam.mask, BitParam#bitParam.doshiftvalue),
	
	bitfield_get_loop(RegisterRec, DataLengthIdx, {regValue, RegisterCurrentValue}, T, lists:append(Result, [{BitFieldIdx, BitFieldValue}])).

%% ====================================================================
%% Validate bit value.
%% Input:
%%		BitParam	:	record of #bitParam{}
%%		Value		:	integer, new value of the bit
%% Output:
%%		boolean
-spec bitfield_validate(int_data(), list() | {integer(), integer()}) -> boolean().
%% ====================================================================
bitfield_validate(Value, PossibleValue) ->
	case PossibleValue of
		PossibleValue_T when is_tuple(PossibleValue_T) ->
			%% This is the min and max value of bit. Must shift the CtrlBitValue in the byte value of register,
			%% according to the mask of the bit.
			%% Make an integer list by the min/max possible values, than verify the given value is member of this list.
			{Min,Max} = PossibleValue_T,
			lists:member(Value, lists:seq(Min,Max));
			
		PossibleValue_T when is_list(PossibleValue_T) ->
			%% This list contains the possible values of bit. The CtrlBitValue is already shifted to the right position in the byte.
			lists:member(Value, PossibleValue_T)
	end.

%% ====================================================================
%% @doc
%% Set the Value specified by Mask in RegValue. Shift the given Value to left
%% if DoShiftValue==true.
%% @end
-spec bit_set(int_data(), int_data(), bitfield_value(), bitfield_mask(), boolean()) -> int_data().
%% ====================================================================
bit_set(RegValue, DataLength, BitFieldValue, BitFieldMask, DoShiftValue) ->
	case DoShiftValue of
		true ->
			bit_operations:bit_set(RegValue, DataLength, BitFieldValue, BitFieldMask, doShiftValueBeforeSet);
		false ->
			bit_operations:bit_set(RegValue, BitFieldValue, BitFieldMask)
	end.

%% ====================================================================
%% @doc
%% Get the Value specified by Mask in RegValue. Shift the given Value to right
%% if DoShiftValue==true.
%% @end
-spec bit_get(int_data(), int_data(), bitfield_mask(), boolean()) -> int_data().
%% ====================================================================
bit_get(RegValue, DataLength, BitFieldMask, DoShiftValue) ->
	case DoShiftValue of
		true ->
			bit_operations:bit_get(RegValue, DataLength, BitFieldMask, doShiftValueAfterGet);
		false ->
			bit_operations:bit_get(RegValue, BitFieldMask)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


