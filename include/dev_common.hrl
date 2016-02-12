%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2016, Robert Balogh
%% @doc
%% This header file contains common defines, records, ect.
%% for all supported devices by Erlang ALE Extension.
%% @end

-ifndef(DEV_COMMON_HRL).
-define(DEV_COMMON_HRL,true).

%% ====================================================================
%% Register addresses of device
%% Structure of register:
%% -record(NAME, {				
%% 				address = <ADDRESS OF REGISTER>,
%% 				bit_x = bit_parameter_record}).
%% where bit_parameter_record
-record(bitParam, {	
					value,			%% {Min,Max} | [ListOfPossibleValues]
					mask,			%% mask of the bit in the byte
					doshiftvalue	%% boolean
									%% If value is tuple, it must be true. This means the new value of bit must shift 
									%% according to its mask before set, and after read from the device. This is
									%% because the {Min,Max} values in the tuple gives the "normal" possible values of the bit
									%% and this value need to shift according to the mask of the byte. 
  					}).
%% ====================================================================


-endif.