%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module provides interface to able to control MCP23S17 and MCP23017
%% IO expander devices.
%% @end


-module(mcp23x17).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("mcp23x17.hrl").
-include("ale_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(DUMMY_BYTE_FOR_READ, 0).
-define(SERVER, ?MODULE).
-define(GEN_SERVER_CALL_TO, 5000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 %% General Interrupt related functions
		 interrupt_polarity_setup/3, interrupt_pin_mirror_setup/3,
		 
		 %% GPIO Interrupt related functions
		 gpio_interrupt_setup/8,
		 gpio_interrupt_enable/4, gpio_interrupt_disable/4,
		 gpio_interrupt_read/2, gpio_interrupt_read/3, gpio_interrupt_get_pin_list_with_int_enabled/3,
		 gpio_interrupt_on_change_ctrl_setup/5, gpio_default_compare_reg_for_int_on_change_setup/5,
		 
		 %% GPIO IO direction related functions
		 gpio_io_direction_setup/5, gpio_io_direction_get/3,
		 
		 %% GPIO logical level related functions
		 gpio_io_logical_level_setup/5, gpio_io_logical_level_get/3, gpio_io_logical_level_get/4,
		 
		 %% GPIO Pull up resistor related functions
		 gpio_pull_up_resistor_setup/5,
		 
		 %% GPIO Input polarirty of IO pin 
		 gpio_input_polarity_setup/5
		]).

-export([register_read/3]).

-export([i2c_driver_stop/2, spi_driver_stop/1]).
-export([set_hwaddr_for_write/1, set_hwaddr_for_read/1]).

%% ====================================================================
%% This export is needed if SPI-CS should be done by an MCP23x17 device.
%% ====================================================================
-export([do_gpio_io_logical_level_setup/5]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% ====================================================================
%% Start/Stop Gen server 
%% ====================================================================

%% ====================================================================
%% @doc
%% Start MCP23x17 driver.
%% @end
-spec start_link() -> ok | {error, term()}.
%% ====================================================================
start_link() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Already started
			{ok, Pid};
		_->	%% The supervisor does not started yet.
			gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, ?GEN_SERVER_CALL_TO}]) 
	end.

%% ====================================================================
%% @doc Stop MCP23x17 driver.
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
%% Driver functions 
%% ====================================================================

%% ====================================================================
%% @doc
%% Setup interrupt pin polarity
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 IntPol		: 1 = Active-high | 0 = Active-low.
%% Output:
%%	 -
%% @end
-spec interrupt_polarity_setup(mcp23x17_comm_type(), hw_addr(),
                               mcp23x17_int_pol()) -> 
                                  ok  | {error, term()}.
%% ====================================================================
interrupt_polarity_setup(CommType, HwAddr, IntPol) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({interrupt_polarity_setup, CommType, HwAddr, IntPol}).

%% ====================================================================
%% @doc
%% Setup interrupt mirroring
%% Input:
%%	 CommType		: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr			: HW address of MCP chip
%%	 IntPinMirror	: 1 = The INT pins are internally connected
%%					  0 = The INT pins are not connected. INTA is associated with PortA and INTB is associated with PortB
%% Output:
%%	 -
%% @end
-spec interrupt_pin_mirror_setup(mcp23x17_comm_type(), hw_addr(),
                                 mcp23x17_int_pin_mirror()) -> 
                                    ok  | {error, term()}.
%% ====================================================================
interrupt_pin_mirror_setup(CommType, HwAddr, IntPinMirror) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({interrupt_pin_mirror_setup, CommType, HwAddr, IntPinMirror}).

%% ====================================================================
%% Setup and enable interrupt on Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 IPol		: input polarity register/bit
%%				 	This register allows the user to configure the polarity on
%%				 	the corresponding GPIO Port bits.
%%				 	If a bit is set, the corresponding GPIO register bit will
%%				 	reflect the inverted value on the Pin.
%%				 	1 = GPIO register bit will reflect the opposite logic state of the input Pin.
%%				 	0 = GPIO register bit will reflect the same logic state of the input Pin.
%%	 PullUpRes	: Pull-Up Resistor
%%				   1: Pull-up enabled
%%				   0: Pull-up disabled
%%	 DefComp	: Default Compare value
%%				 	The default comparison value is configured in the
%%				 	DEFVAL register. If enabled (via GPINTEN and
%%				 	INTCON) to compare against the DEFVAL register, an
%%				 	opposite value on the associated Pin will cause an
%%				 	interrupt to occur.
%%				 	If the associated Pin level is the opposite from the register bit, an interrupt occurs.
%%	 IntCtrl	: interrupt control
%%					The INTCON register controls how the associated Pin
%%					value is compared for the interrupt-on-change feature.
%%				 	If a bit is set, the corresponding I/O Pin is compared
%%				 	against the associated bit in the DEFVAL register. If a
%%				 	bit value is clear, the corresponding I/O Pin is compared
%%				 	against the previous value.
%%				   	1 = Controls how the associated Pin value is compared for interrupt-on-change.
%%				   	0 = Pin value is compared against the previous Pin value.
-spec gpio_interrupt_setup(mcp23x17_comm_type(), hw_addr(),
                           mcp23x17_port(),
                           mcp23x17_pin()  | [mcp23x17_pin()]
                            | {mcp23x17_pin(), mcp23x17_pin()},
                           mcp23x17_in_pol(), mcp23x17_pull_up_res(),
                           mcp23x17_def_comp_val(), mcp23x17_ioc_comp_val()) -> 
                              ok  | {error, term()}.
%% ====================================================================
gpio_interrupt_setup(CommType, HwAddr, Port, Pin, IPol, PullUpRes, DefComp, IntCtrl) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_setup_interrupt, CommType, HwAddr, Port, Pin, IPol, PullUpRes, DefComp, IntCtrl}).

%% ====================================================================
%% @doc
%% Enable interrupt on Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	 -
%% @end
-spec gpio_interrupt_enable(mcp23x17_comm_type(), hw_addr(),
                            mcp23x17_port(),
                            mcp23x17_pin()  | [mcp23x17_pin()]
                             | {mcp23x17_pin(), mcp23x17_pin()}) -> 
                               ok  | {error, term()}.
%% ====================================================================
gpio_interrupt_enable(CommType, HwAddr, Port, Pin) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_enable_interrupt, CommType, HwAddr, Port, Pin}).

%% ====================================================================
%% @doc
%% Disable interrupt on Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	 -
%% @end
-spec gpio_interrupt_disable(mcp23x17_comm_type(), hw_addr(),
                             mcp23x17_port(),
                             mcp23x17_pin()  | [mcp23x17_pin()]
                              | {mcp23x17_pin(), mcp23x17_pin()}) -> 
                                ok  | {error, term()}.
%% ====================================================================
gpio_interrupt_disable(CommType, HwAddr, Port, Pin) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_interrupt_disable, CommType, HwAddr, Port, Pin}).

%% ====================================================================
%% @doc
%% Read corresponding register if interrupt occurred
%% @end
-spec gpio_interrupt_read(mcp23x17_comm_type(), hw_addr()) -> {ok, list({mcp23x17_port(), {reg_value(), list(mcp23x17_pin())}})} | {error, term()}.
%% ====================================================================
gpio_interrupt_read(CommType, HwAddr) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_interrupt_read, CommType, HwAddr}).

%% ====================================================================
%% @doc
%% Read corresponding register if interrupt occurred
%% @end
-spec gpio_interrupt_read(mcp23x17_comm_type(), hw_addr(), mcp23x17_port()) -> {ok, list({mcp23x17_port(), {reg_value(), list(mcp23x17_pin())}})} | {error, term()}.
%% ====================================================================
gpio_interrupt_read(CommType, HwAddr, Port) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_interrupt_read, CommType, HwAddr, Port}).

%% ====================================================================
%% @doc
%% Give the list of PINs where interrupt is enabled.
%% @end
-spec gpio_interrupt_get_pin_list_with_int_enabled(mcp23x17_comm_type(),
                                                   hw_addr(),
                                                   mcp23x17_port()) -> 
                                                      {ok, [mcp23x17_pin()]}
                                                       | {error, term()}.
%% ====================================================================
gpio_interrupt_get_pin_list_with_int_enabled(CommType, HwAddr, Port) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_interrupt_get_pin_list_with_int_enabled, CommType, HwAddr, Port}).

%% ====================================================================
%% @doc
%% Setup interrupt-on-change control of Port/Pin.
%% @end
-spec gpio_interrupt_on_change_ctrl_setup(mcp23x17_comm_type(),
                                          hw_addr(), mcp23x17_port(),
                                          mcp23x17_pin()  | [mcp23x17_pin()]
                                           | {mcp23x17_pin(), mcp23x17_pin()},
                                          mcp23x17_ioc_comp_val()) -> 
                                             ok  | {error, term()}.
%% ====================================================================
gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, Pin, IntCtrl) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_interrupt_on_change_ctrl_setup, CommType, HwAddr, Port, Pin, IntCtrl}).

%% ====================================================================
%% @doc
%% Setup default compare register for interrupt-on-change event of Port/Pin.
%% @end
-spec gpio_default_compare_reg_for_int_on_change_setup(mcp23x17_comm_type(),
                                                       hw_addr(),
                                                       mcp23x17_port(),
                                                       mcp23x17_pin()
                                                        | [mcp23x17_pin()]
                                                        | {mcp23x17_pin(),
                                                           mcp23x17_pin()},
                                                       mcp23x17_def_comp_val()) -> 
                                                          ok  | {error, term()}.
%% ====================================================================
gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, Pin, DefComp) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_default_compare_reg_for_int_on_change_setup, CommType, HwAddr, Port, Pin, DefComp}).

%% ====================================================================
%% @doc
%% Setup IO direction of Port/Pin.
%% @end
-spec gpio_io_direction_setup(mcp23x17_comm_type(), hw_addr(),
                              mcp23x17_port(),
                              mcp23x17_pin()  | [mcp23x17_pin()]
                               | {mcp23x17_pin(), mcp23x17_pin()},
                              mcp23x17_io_dir()) -> 
                                 ok  | {error, term()}.
%% ====================================================================
gpio_io_direction_setup(CommType, HwAddr, Port, Pin, IODir) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_io_direction_setup, CommType, HwAddr, Port, Pin, IODir}).

%% ====================================================================
%% @doc
%% Read IO direction of Port/Pin.
%% @end
-spec gpio_io_direction_get(mcp23x17_comm_type(), hw_addr(), mcp23x17_port()) -> {ok, list(mcp23x17_io_dir())} | {error, term()}.
%% ====================================================================
gpio_io_direction_get(CommType, HwAddr, Port) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_io_direction_get, CommType, HwAddr, Port}).

%% ====================================================================
%% @doc
%% Setup IO logical level of Port/Pin. So set to LOW or HIGH the pin.
%% Input:
%%	 CommType		: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr			: HW address of MCP chip
%%	 Port			: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin			: the Pin id of Port
%%	 LogicalLevel	: mcp23x17_io_logical_level()
%% Output:
%%	 ok | {error, Reason}
%% @end
-spec gpio_io_logical_level_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}, mcp23x17_io_logical_level()) -> ok | {error, term()}.
%% ====================================================================
gpio_io_logical_level_setup(CommType, HwAddr, Port, Pin, LogicalLevel) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_io_logical_level_setup, CommType, HwAddr, Port, Pin, LogicalLevel}).

%% ====================================================================
%% @doc
%% Get IO logical level of Port/Pin.
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%% Output:
%%	 {ok, list(mcp23x17_io_logical_level())} | {error, Reason}
%% @end
-spec gpio_io_logical_level_get(mcp23x17_comm_type(), hw_addr(), mcp23x17_port()) -> {ok, list(mcp23x17_io_logical_level())} | {error, term()}.
%% ====================================================================
gpio_io_logical_level_get(CommType, HwAddr, Port) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_io_logical_level_get, CommType, HwAddr, Port}).

%% ====================================================================
%% @doc
%% Get IO logical level of Port/Pin.
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	 {ok, mcp23x17_io_logical_level()} | {error, Reason}
%% @end
-spec gpio_io_logical_level_get(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin())) -> {ok, list(mcp23x17_io_logical_level())} | {error, term()}.
%% ====================================================================
gpio_io_logical_level_get(CommType, HwAddr, Port, Pin) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_io_logical_level_get, CommType, HwAddr, Port, Pin}).
	
%% ====================================================================
%% @doc
%% Setup pull-up resistor of Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 PullUpRes	: mcp23x17_pull_up_res_en() | mcp23x17_pull_up_res_dis()
%% Output:
%%	 -
%% @end
-spec gpio_pull_up_resistor_setup(mcp23x17_comm_type(), hw_addr(),
                                  mcp23x17_port(),
                                  mcp23x17_pin()  | [mcp23x17_pin()]
                                   | {mcp23x17_pin(), mcp23x17_pin()},
                                  mcp23x17_pull_up_res()) -> 
                                     ok  | {error, term()}.
%% ====================================================================
gpio_pull_up_resistor_setup(CommType, HwAddr, Port, Pin, PullUpRes) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({gpio_pull_up_resistor_setup, CommType, HwAddr, Port, Pin, PullUpRes}).

%% ====================================================================
%% @doc
%% Setup input polarity of Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 IPol		: mcp23x17_in_pol()
%% Output:
%%	 -
%% @end
-spec gpio_input_polarity_setup(mcp23x17_comm_type(), hw_addr(),
                                mcp23x17_port(),
                                mcp23x17_pin()  | [mcp23x17_pin()]
                                 | {mcp23x17_pin(), mcp23x17_pin()},
                                mcp23x17_in_pol()) -> 
                                   ok  | {error, term()}.
%% ====================================================================
gpio_input_polarity_setup(CommType, HwAddr, Port, Pin, IPol) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({input_polarity_setup, CommType, HwAddr, Port, Pin, IPol}).

%% ====================================================================
%% @doc
%% Read register in MCP23x17 device.
%% @end
-spec register_read(mcp23x17_comm_type(), hw_addr(), reg_addr()) -> ok | {error, term()}.
%% ====================================================================
register_read(CommType, HwAddr, RegAddress) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({register_read, CommType, HwAddr, RegAddress}).

%% ====================================================================
%% @doc
%% Stop I2C driver.
%% @end
-spec i2c_driver_stop(mcp23x17_comm_type(), hw_addr()) -> ok | {error, term()}.
%% ====================================================================
i2c_driver_stop(CommType, HwAddr) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({i2c_driver_stop, CommType, HwAddr}).
 
%% ====================================================================
%% @doc
%% Stop SPI driver.
%% @end
-spec spi_driver_stop(mcp23x17_comm_type()) -> ok | {error, term()}.
%% ====================================================================
spi_driver_stop(CommType) ->
	%% Start server if not yet started.
	start_link(),
	
	do_gen_server_call({spi_driver_stop, CommType}).

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
init([]) ->
	ets:new(?MCP23X17_PIN_TABLE, [ordered_set,public,named_table,{keypos,?MCP23X17_PIN_TABLE_KEYPOS}]),
	{ok, #state{}}.


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
handle_call({gpio_setup_interrupt, CommType, HwAddr, Port, Pin, IPol, PullUpRes, DefComp, IntCtrl}, _From, State) ->
	Reply = do_gpio_interrupt_setup(CommType, HwAddr, Port, Pin, IPol, PullUpRes, DefComp, IntCtrl),
	{reply, Reply, State};

handle_call({gpio_enable_interrupt, CommType, HwAddr, Port, Pin}, _From, State) ->
	Reply = do_gpio_enable_interrupt(CommType, HwAddr, Port, Pin),
	{reply, Reply, State};

handle_call({gpio_interrupt_disable, CommType, HwAddr, Port, Pin}, _From, State) ->
	Reply = do_gpio_disable_interrupt(CommType, HwAddr, Port, Pin),
	{reply, Reply, State};

handle_call({interrupt_polarity_setup, CommType, HwAddr, IntPol}, _From, State) ->
	Reply = do_interrupt_polarity_setup(CommType, HwAddr, IntPol),
	{reply, Reply, State};

handle_call({interrupt_pin_mirror_setup, CommType, HwAddr, IntPinMirror}, _From, State) ->
	Reply = do_interrupt_pin_mirror_setup(CommType, HwAddr, IntPinMirror),
	{reply, Reply, State};

handle_call({gpio_interrupt_read, CommType, HwAddr}, _From, State) ->
	Reply = do_gpio_interrupt_read(CommType, HwAddr),
	{reply, Reply, State};

handle_call({gpio_interrupt_read, CommType, HwAddr, Port}, _From, State) ->
	Reply = do_gpio_interrupt_read(CommType, HwAddr, Port),
	{reply, Reply, State};

handle_call({gpio_interrupt_get_pin_list_with_int_enabled, CommType, HwAddr, Port}, _From, State) ->
	GPINTENRegAddr = case Port of
						 ?MCP23X17_PORT_A ->
							 ?GPINTENA_ADDR;
						 ?MCP23X17_PORT_B ->
							 ?GPINTENB_ADDR
					end,
	case read(CommType, HwAddr, GPINTENRegAddr) of
		{ok, GPINTENRegValue} ->
			%% Filter the PINs where interrupt is enabled
			{ok, PinWithInterruptEnabledList} = do_gpio_interrupt_get_pin_list_with_int_enabled(GPINTENRegValue),
			
			{reply, PinWithInterruptEnabledList, State};
		
		ER->{reply, ER, State}
	end;

handle_call({gpio_interrupt_on_change_ctrl_setup, CommType, HwAddr, Port, Pin, IntCtrl}, _From, State) ->
	Reply = do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, Pin, IntCtrl),
	{reply, Reply, State};

handle_call({gpio_default_compare_reg_for_int_on_change_setup, CommType, HwAddr, Port, Pin, DefComp}, _From, State) ->
	Reply = do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, Pin, DefComp),
	{reply, Reply, State};

handle_call({gpio_io_direction_setup, CommType, HwAddr, Port, Pin, IODir}, _From, State) ->
	Reply = do_gpio_io_direction_setup(CommType, HwAddr, Port, Pin, IODir),
	{reply, Reply, State};

handle_call({gpio_io_direction_get, CommType, HwAddr, Port}, _From, State) ->
	Reply = do_gpio_io_direction_get(CommType, HwAddr, Port),
	{reply, Reply, State};
	
handle_call({gpio_io_logical_level_setup, CommType, HwAddr, Port, Pin, LogicalLevel}, _From, State) ->
	Reply = do_gpio_io_logical_level_setup(CommType, HwAddr, Port, Pin, LogicalLevel),
	{reply, Reply, State};

handle_call({gpio_io_logical_level_get, CommType, HwAddr, Port}, _From, State) ->
	Reply = do_gpio_io_logical_level_get(CommType, HwAddr, Port, lists:seq(?MCP23X17_PIN0, ?MCP23X17_PIN7)),
	{reply, Reply, State};

handle_call({gpio_io_logical_level_get, CommType, HwAddr, Port, Pin}, _From, State) ->
	Reply = do_gpio_io_logical_level_get(CommType, HwAddr, Port, Pin),
	{reply, Reply, State};

handle_call({gpio_pull_up_resistor_setup, CommType, HwAddr, Port, Pin, PullUpRes}, _From, State) ->
	Reply = do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, Pin, PullUpRes),
	{reply, Reply, State};

handle_call({input_polarity_setup, CommType, HwAddr, Port, Pin, IPol}, _From, State) ->
	Reply = do_gpio_input_polarity_setup(CommType, HwAddr, Port, Pin, IPol),
	{reply, Reply, State};

handle_call({register_read, CommType, HwAddr, RegAddress}, _From, State) ->
	Reply = read(CommType, HwAddr, RegAddress),
	{reply, Reply, State};
	
handle_call({i2c_driver_stop, CommType, HwAddr}, _From, State) ->
	Reply = ale_handler:i2c_stop(get_comm_devicename(CommType), HwAddr),
	{reply, Reply, State};

handle_call({spi_driver_stop, CommType}, _From, State) ->
	Reply = ale_handler:spi_stop(get_comm_devicename(CommType)),
	{reply, Reply, State};

handle_call({stop}, _From, State) ->
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
%% Setup and enable interrupt on Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 IPol		: input polarity register/bit
%%				 	This register allows the user to configure the polarity on
%%				 	the corresponding GPIO Port bits.
%%				 	If a bit is set, the corresponding GPIO register bit will
%%				 	reflect the inverted value on the Pin.
%%				 	1 = GPIO register bit will reflect the opposite logic state of the input Pin.
%%				 	0 = GPIO register bit will reflect the same logic state of the input Pin.
%%	 PullUpRes	: Pull-Up Resistor
%%				   1: Pull-up enabled
%%				   0: Pull-up disabled
%%	 DefComp	: Default Compare value
%%				 	The default comparison value is configured in the
%%				 	DEFVAL register. If enabled (via GPINTEN and
%%				 	INTCON) to compare against the DEFVAL register, an
%%				 	opposite value on the associated Pin will cause an
%%				 	interrupt to occur.
%%				 	If the associated Pin level is the opposite from the register bit, an interrupt occurs.
%%	 IntCtrl	: interrupt control
%%					The INTCON register controls how the associated Pin
%%					value is compared for the interrupt-on-change feature.
%%				 	If a bit is set, the corresponding I/O Pin is compared
%%				 	against the associated bit in the DEFVAL register. If a
%%				 	bit value is clear, the corresponding I/O Pin is compared
%%				 	against the previous value.
%%				   	1 = Controls how the associated Pin value is compared for interrupt-on-change.
%%				   	0 = Pin value is compared against the previous Pin value.
-spec do_gpio_interrupt_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}, mcp23x17_in_pol(), mcp23x17_pull_up_res(), mcp23x17_def_comp_val(), mcp23x17_ioc_comp_val()) -> ok | {error, term()}.
%% ====================================================================
do_gpio_interrupt_setup(CommType, HwAddr, Port, Pin, IPol, PullUpRes, DefComp, IntCtrl) ->
	%% Setup direction of Port/Pin to input
	case do_gpio_io_direction_setup(CommType, HwAddr, Port, Pin, ?MCP23X17_IO_INPUT) of
		ok ->
			%% Setup input polarity
			case do_gpio_input_polarity_setup(CommType, HwAddr, Port, Pin, IPol) of
				ok ->
					%% Setup Pull-Up resistor
					case do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, Pin, PullUpRes) of
						ok ->
							%% Setup default compare register for interrupt-on-change.
							case do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, Pin, DefComp) of
								ok -> 
									%% Setup INTCON (Interrupt-On-Change Control) register.
									%% By default interrupt will occurred when IO Pin logic level
									%% matches with value of corresponding bit in DEFVAL register.
									case do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, Pin, IntCtrl) of
										ok ->
											%% Enable interrupt on PIN
											case do_gpio_enable_interrupt(CommType, HwAddr, Port, Pin) of
												ok->
													?DO_INFO("Interrupt for a PIN on MCP23x17 device has been configured and enabled successfully.",
															 [{commType, CommType},
															 {hwAddr, HwAddr},
															 {port, Port},
															 {pin, Pin},
															 {inputPolarity, IPol},
															 {pullUpResistor, PullUpRes},
															 {defaultComparationValue_DEFVAL, DefComp},
															 {iocValue, IntCtrl}]),
													ok;
												ER->
													?DO_ERR("Interrupt for a PIN on MCP23x17 device has been configured and enabled successfully.",
															[{commType, CommType},
															{hwAddr, HwAddr},
															{port, Port},
															{pin, Pin},
															{inputPolarity, IPol},
															{pullUpResistor, PullUpRes},
															{defaultComparationValue_DEFVAL, DefComp},
															{iocValue, IntCtrl},
															{reason, ER}]),
													ER
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
%% Enable interrupt on Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	 -
%% @end
-spec do_gpio_enable_interrupt(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}) -> ok | {error, term()}.
%% ====================================================================
do_gpio_enable_interrupt(CommType, HwAddr, Port, Pin) ->
	do_gpio_interrupt_en_dis(CommType, HwAddr, Port, Pin, en).

%% ====================================================================
%% @doc
%% Disable interrupt on Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	 -
%% @end
-spec do_gpio_disable_interrupt(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}) -> ok | {error, term()}.
%% ====================================================================
do_gpio_disable_interrupt(CommType, HwAddr, Port, Pin) ->
	do_gpio_interrupt_en_dis(CommType, HwAddr, Port, Pin, dis).

%% ====================================================================
%% @doc
%% Setup interrupt pin polarity
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 IntPol		: 1 = Active-high | 0 = Active-low.
%% Output:
%%	 -
%% @end
-spec do_interrupt_polarity_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_int_pol()) -> ok | {error, term()}.
%% ====================================================================
do_interrupt_polarity_setup(CommType, HwAddr, IntPol) ->
	case read(CommType, HwAddr, ?IOCON_ADDR) of
		{ok, IOCONRegValue} ->
			NewIOCONRegValue = case IntPol of
								   ?MCP23X17_INT_POL_ACTIVE_HIGH ->
									   bit_operations:bit_set(IOCONRegValue, ?IOCON_INTPOL);
								   ?MCP23X17_INT_POL_ACTIVE_LOW ->
									   bit_operations:bit_clear(IOCONRegValue, ?IOCON_INTPOL)
							   end,
			write(CommType, HwAddr, ?IOCON_ADDR, NewIOCONRegValue);
		{error, ER} ->
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Setup interrupt mirroring
%% Input:
%%	 CommType		: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr			: HW address of MCP chip
%%	 IntPinMirror	: 1 = The INT pins are internally connected
%%					  0 = The INT pins are not connected. INTA is associated with PortA and INTB is associated with PortB
%% Output:
%%	 -
%% @end
-spec do_interrupt_pin_mirror_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_int_pin_mirror()) -> ok | {error, term()}.
%% ====================================================================
do_interrupt_pin_mirror_setup(CommType, HwAddr, IntPinMirror) ->
	case read(CommType, HwAddr, ?IOCON_ADDR) of
		{ok, IOCONRegValue} ->
			NewIOCONRegValue = case IntPinMirror of
								   ?MCP23X17_INT_PINS_MIRRORED ->
									   bit_operations:bit_set(IOCONRegValue, ?IOCON_MIRROR);
								   ?MCP23X17_INT_PINS_NOT_MIRRORED ->
									   bit_operations:bit_clear(IOCONRegValue, ?IOCON_MIRROR)
							   end,
			write(CommType, HwAddr, ?IOCON_ADDR, NewIOCONRegValue);
		{error, ER} ->
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Read corresponding register if interrupt occurred.
%% @end
-spec do_gpio_interrupt_read(mcp23x17_comm_type(), hw_addr()) -> {ok, list({mcp23x17_port(), {reg_value(), list(mcp23x17_pin())}})} | {error, term()}.
%% ====================================================================
do_gpio_interrupt_read(CommType, HwAddr) ->
	do_gpio_interrupt_read_loop(CommType, HwAddr, [?MCP23X17_PORT_A, ?MCP23X17_PORT_B], []).

do_gpio_interrupt_read_loop(_CommType, _HwAddr, [], Result)->
	case Result of
		{error,Reason} ->
			{error,Reason};
		_->	{ok, Result}
	end;
do_gpio_interrupt_read_loop(CommType, HwAddr, [Port | T], Result)->
	case do_gpio_interrupt_read(CommType, HwAddr, Port) of
		{ok, [{Port, {RegValue, GpioList}}]} ->
			do_gpio_interrupt_read_loop(CommType, HwAddr, T, lists:append(Result, [{Port, {RegValue, GpioList}}]));
		ER->do_gpio_interrupt_read_loop(CommType, HwAddr, [], ER)
	end.
  
%% ====================================================================
%% @doc
%% Read corresponding register if interrupt occurred
%% @end
-spec do_gpio_interrupt_read(mcp23x17_comm_type(), hw_addr(), mcp23x17_port()) -> {ok, list({mcp23x17_port(), {reg_value(), list(mcp23x17_pin())}})} | {error, term()}.
%% ====================================================================
do_gpio_interrupt_read(CommType, HwAddr, Port) ->
	%% Read interrupt source register
	{INTFRegAddr, GPIORegAddr, INTCAPRegAddr} = case Port of
													?MCP23X17_PORT_A ->
														{?INTFA_ADDR, ?GPIOA_ADDR, ?INTCAPA_ADDR};
													?MCP23X17_PORT_B ->
														{?INTFB_ADDR, ?GPIOB_ADDR, ?INTCAPB_ADDR}
												end,
	case read(CommType, HwAddr, INTFRegAddr) of
		{ok, INTFRegValue} ->
			%% Clear interrupt condition
			case read(CommType, HwAddr, GPIORegAddr) of
				{ok,_} ->
					case read(CommType, HwAddr, INTCAPRegAddr) of
						{ok,_} ->
							
							%% Check interrupt is enabled on which PIN, and returns with the list of PINs where interrupt is
							%% enabled and interrupt has been occurred too. 
							GPINTENRegAddr = case Port of
												 ?MCP23X17_PORT_A ->
													 ?GPINTENA_ADDR;
												 ?MCP23X17_PORT_B ->
													 ?GPINTENB_ADDR
											end,
							case read(CommType, HwAddr, GPINTENRegAddr) of
								{ok, GPINTENRegValue} ->
									%% Filter the PINs where interrupt is enabled
									{ok, PinWithInterruptEnabledList} = do_gpio_interrupt_get_pin_list_with_int_enabled(GPINTENRegValue),
									
									%% Check interrupt flags
									PinWithInterruptList = lists:append([begin
										 case bit_operations:bit_test(INTFRegValue, Pin) of
											 1 ->
												 %% Interrupt has been occurred on the PIN
												 [Pin];
											 0 ->
												 %% No interrupt on the PIN
												 []
										 end
									 end || Pin <- PinWithInterruptEnabledList]),
									
									{ok, [{Port, {INTFRegValue, PinWithInterruptList}}]};
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
%% Setup interrupt-on-change control of Port/Pin.
%% @end
-spec do_gpio_interrupt_on_change_ctrl_setup(mcp23x17_comm_type(),
                                             hw_addr(), mcp23x17_port(),
                                             mcp23x17_pin()  | [mcp23x17_pin()]
                                              | {mcp23x17_pin(),
                                                 mcp23x17_pin()},
                                             mcp23x17_ioc_comp_val()) -> 
                                                ok  | {error, term()}.
%% ====================================================================
do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, Pin, IntCtrl) when is_tuple(Pin) ->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_ -> do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), IntCtrl)
	end;
do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, Pin, IntCtrl) when is_integer(Pin) ->
	do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, [Pin], IntCtrl);
do_gpio_interrupt_on_change_ctrl_setup(CommType, HwAddr, Port, Pin, IntCtrl) when is_list(Pin) ->
	%% Setup INTCON (Interrupt-On-Change Control) register.
	%% By default interrupt will occurred when IO Pin logic level
	%% matches with value of corresponding bit in DEFVAL register.
	INTCONRegAddr = case Port of
						?MCP23X17_PORT_A ->
							?INTCONA_ADDR;
						?MCP23X17_PORT_B ->
							?INTCONB_ADDR
					end,
	case read(CommType, HwAddr, INTCONRegAddr) of
		{ok, INTCONRegValue} ->
			NewINTCONRegValue = do_interrupt_on_change_ctrl_setup_loop(IntCtrl, INTCONRegValue, Pin),
			
			%% Write changes in the register
			write(CommType, HwAddr, INTCONRegAddr, NewINTCONRegValue);
		ER->ER
	end.

do_interrupt_on_change_ctrl_setup_loop(_IntCtrl, INTCONRegValue, []) ->
	INTCONRegValue;
do_interrupt_on_change_ctrl_setup_loop(IntCtrl, INTCONRegValue, [Pin | T]) ->
	NewINTCONRegValue = case IntCtrl of
							?MCP23X17_IOC_COMP_WITH_DEF_COMP_VALUE ->
								bit_operations:bit_set(INTCONRegValue, Pin);
							?MCP23X17_IOC_COMP_WITH_PREV_PIN_VALUE ->
								bit_operations:bit_clear(INTCONRegValue, Pin)
						end,
	do_interrupt_on_change_ctrl_setup_loop(IntCtrl, NewINTCONRegValue, T).

%% ====================================================================
%% @doc
%% Setup default compare register for interrupt-on-change event of Port/Pin.
%% @end
-spec do_gpio_default_compare_reg_for_int_on_change_setup(mcp23x17_comm_type(),
                                                          hw_addr(),
                                                          mcp23x17_port(),
                                                          mcp23x17_pin()
                                                           | [mcp23x17_pin()]
                                                           | {mcp23x17_pin(),
                                                              mcp23x17_pin()},
                                                          mcp23x17_def_comp_val()) -> 
                                                             ok
                                                              | {error, term()}.
%% ====================================================================
do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, Pin, DefComp) when is_tuple(Pin) ->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_ -> do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), DefComp)
	end;
do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, Pin, DefComp) when is_integer(Pin) ->
	do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, [Pin], DefComp);
do_gpio_default_compare_reg_for_int_on_change_setup(CommType, HwAddr, Port, Pin, DefComp) when is_list(Pin) ->
	%% Setup default compare register for interrupt-on-change.
	DEFVALRegAddr = case Port of
						?MCP23X17_PORT_A ->
							?DEFVALA_ADDR;
						?MCP23X17_PORT_B ->
							?DEFVALB_ADDR
	end,
	
	case read(CommType, HwAddr, DEFVALRegAddr) of
		{ok, DEFVALRegValue} ->
			NewDEFVALRegValue = do_default_compare_reg_for_int_on_change_setup_loop(DefComp, DEFVALRegValue, Pin),
			
			%% Write new value in the register.
			write(CommType, HwAddr, DEFVALRegAddr, NewDEFVALRegValue);
		
		ER->ER
	end.

do_default_compare_reg_for_int_on_change_setup_loop(_DefComp, DEFVALRegValue, []) ->
	DEFVALRegValue;
do_default_compare_reg_for_int_on_change_setup_loop(DefComp, DEFVALRegValue, [Pin | T]) ->
	NewDEFVALRegValue = case DefComp of
							?MCP23X17_DEF_COMP_VALUE_HIGH ->
								bit_operations:bit_set(DEFVALRegValue, Pin);
							?MCP23X17_DEF_COMP_VALUE_LOW ->
								bit_operations:bit_clear(DEFVALRegValue, Pin)
						end,
	do_default_compare_reg_for_int_on_change_setup_loop(DefComp, NewDEFVALRegValue, T).

%% ====================================================================
%% @doc
%% Setup IO direction of Port/Pin.
%% @end
-spec do_gpio_io_direction_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}, mcp23x17_io_dir()) -> ok | {error, term()}.
%% ====================================================================
do_gpio_io_direction_setup(CommType, HwAddr, Port, Pin, IODir) when is_tuple(Pin)->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_->	do_gpio_io_direction_setup(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), IODir)
	end;
do_gpio_io_direction_setup(CommType, HwAddr, Port, Pin, IODir) when is_integer(Pin)->
	do_gpio_io_direction_setup(CommType, HwAddr, Port, [Pin], IODir);
do_gpio_io_direction_setup(CommType, HwAddr, Port, PinList, IODir) when is_list(PinList)->
	%% Sort out the PIN wher the IO direction is already set.
	PinRecList = do_gpio_io_direction_setup_loop1(CommType, HwAddr, Port, PinList, IODir, []),
	
	case PinRecList of
		[] ->
			%% All PIN's IO direction already set.
			ok;
		_->
			%% Read register in the device.
			IODirRegAddr = case Port of
							   ?MCP23X17_PORT_A ->
								   ?IODIRA_ADDR;
							   ?MCP23X17_PORT_B ->
								   ?IODIRB_ADDR
							end,
			
			case read(CommType, HwAddr, IODirRegAddr) of
				{ok, IODirRegValue} ->
					NewIODirRegValue = do_gpio_io_direction_setup_loop2(IODir, IODirRegValue, PinRecList),
					
					%% Write new value in the register.
					case write(CommType, HwAddr, IODirRegAddr, NewIODirRegValue) of
						ok ->
							%% Update ETS table accordingly
							[begin
								 pin_rec_set(PinRec#rMCP23x17_PORT_PIN{direction = IODir})
							 end || PinRec <- PinRecList],
							
							ok;
						ER->ER
					end;
				ER->ER
			end
	end.

do_gpio_io_direction_setup_loop1(_CommType, _HwAddr, _Port, [], _IODir, PinRecList) ->
	PinRecList;
do_gpio_io_direction_setup_loop1(CommType, HwAddr, Port, [Pin | T], IODir, PinRecList) ->
	%% Sort out the PIN wher the IO direction is already set.
	case pin_rec_get(CommType, HwAddr, Port, Pin) of
		{ok, Rec} ->
			case Rec#rMCP23x17_PORT_PIN.direction of
				IODir ->
					%% IO direction of the Pin is already set. Do nothing.
					do_gpio_io_direction_setup_loop1(CommType, HwAddr, Port, T, IODir, PinRecList);
					
				_-> %% IO direction is already set, but does not matches with the required.
					%% Must change the IO direction.
					do_gpio_io_direction_setup_loop1(CommType, HwAddr, Port, T, IODir, lists:append([{Pin, {false, Rec}}], PinRecList))
			end;
		_->	%% IO direction does not set yet.
			Rec = #rMCP23x17_PORT_PIN{pinId = {CommType, HwAddr, Port, Pin},
									direction = undefined},
			do_gpio_io_direction_setup_loop1(CommType, HwAddr, Port, T, IODir, lists:append([Rec], PinRecList))
	end.

do_gpio_io_direction_setup_loop2(_IODir, IODirRegValue, []) ->
	IODirRegValue;
do_gpio_io_direction_setup_loop2(IODir, IODirRegValue, [PinRec | T]) ->
	{_CommType, _HwAddr, _Port, Pin} = PinRec#rMCP23x17_PORT_PIN.pinId,
	NewIODirRegValue = case IODir of
						   ?MCP23X17_IO_INPUT ->
							   bit_operations:bit_set(IODirRegValue, Pin);
						   ?MCP23X17_IO_OUTPUT ->
							   bit_operations:bit_clear(IODirRegValue, Pin)
					   end,
	do_gpio_io_direction_setup_loop2(IODir, NewIODirRegValue, T).

%% ====================================================================
%% @doc
%% @end
-spec do_gpio_io_direction_get(mcp23x17_comm_type(), hw_addr(), mcp23x17_port()) -> {ok, list(mcp23x17_io_dir())} | {error, term()}.
%% ====================================================================
do_gpio_io_direction_get(CommType, HwAddr, Port) ->
	%% Read register in the device.
	IODirRegAddr = case Port of
					   ?MCP23X17_PORT_A ->
						   ?IODIRA_ADDR;
					   ?MCP23X17_PORT_B ->
						   ?IODIRB_ADDR
					end,
	
	case read(CommType, HwAddr, IODirRegAddr) of
		{ok, IODirRegValue} ->
			{ok, [begin
					  bit_operations:bit_test(IODirRegValue, Pin)
				  end || Pin <- lists:reverse(lists:seq(?MCP23X17_PIN0, ?MCP23X17_PIN7))]};
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Setup IO logical level of Port/Pin. So set to LOW or HIGH the pin.
%% Input:
%%	 CommType		: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr			: HW address of MCP chip
%%	 Port			: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin			: the Pin id of Port
%%	 LogicalLevel	: mcp23x17_io_logical_level()
%% Output:
%%	 ok | {error, Reason}
%% @end
-spec do_gpio_io_logical_level_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}, mcp23x17_io_logical_level()) -> ok | {error, term()}.
%% ====================================================================
do_gpio_io_logical_level_setup(CommType, HwAddr, Port, Pin, LogicalLevel) when is_tuple(Pin)->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_->	do_gpio_io_logical_level_setup(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), LogicalLevel)
	end;
do_gpio_io_logical_level_setup(CommType, HwAddr, Port, Pin, LogicalLevel) when is_integer(Pin)->
	do_gpio_io_logical_level_setup(CommType, HwAddr, Port, [Pin], LogicalLevel);
do_gpio_io_logical_level_setup(CommType, HwAddr, Port, Pin, LogicalLevel) when is_list(Pin)->
	%% Setup IO direction to OUTPUT of Port/Pin first
	case do_gpio_io_direction_setup(CommType, HwAddr, Port, Pin, ?MCP23X17_IO_OUTPUT) of
		ok ->
			%% Set logical level of Port/Pin
			OLATRegAddr = case Port of
						  ?MCP23X17_PORT_A ->
							  ?OLATA_ADDR;
						  ?MCP23X17_PORT_B ->
							  ?OLATB_ADDR
			end,
			
			%% Set/Clear port/pin
			case read(CommType, HwAddr, OLATRegAddr) of
				{ok, OLATRegValue} ->
					NewOLATRegValue = do_setup_io_logical_level_loop(LogicalLevel, OLATRegValue, Pin),
					
					%% Write changes in the register
					write(CommType, HwAddr, OLATRegAddr, NewOLATRegValue);
				ER->ER
			end;
		ER->ER
	end.

do_setup_io_logical_level_loop(_LogicalLevel, OLATRegValue, []) ->
	OLATRegValue;
do_setup_io_logical_level_loop(LogicalLevel, OLATRegValue, [Pin | T]) ->
	NewOLATRegValue = case LogicalLevel of
						  ?MCP23X17_IO_LOGICAL_LOW ->
							  bit_operations:bit_clear(OLATRegValue, Pin);
						  ?MCP23X17_IO_LOGICAL_HIGH ->
							  bit_operations:bit_set(OLATRegValue, Pin)
					  end,
	do_setup_io_logical_level_loop(LogicalLevel, NewOLATRegValue, T).

%% ====================================================================
%% @doc
%% Get IO logical level of Port/Pin.
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	 {ok, mcp23x17_io_logical_level()} | {error, Reason}
%% @end
-spec do_gpio_io_logical_level_get(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin())) -> {ok, list(mcp23x17_io_logical_level())} | {error, term()}.
%% ====================================================================
do_gpio_io_logical_level_get(CommType, HwAddr, Port, Pin) when is_integer(Pin)->
	do_gpio_io_logical_level_get(CommType, HwAddr, Port, [Pin]);
do_gpio_io_logical_level_get(CommType, HwAddr, Port, PinList) when is_list(PinList)->
	OLATRegAddr = case Port of
					  ?MCP23X17_PORT_A ->
						  ?GPIOA_ADDR;
					  ?MCP23X17_PORT_B ->
						  ?GPIOB_ADDR
	end,
	
	%% Get port/pin
	case read(CommType, HwAddr, OLATRegAddr) of
		{ok, OLATRegValue} ->
			{ok, [begin
					  bit_operations:bit_test(OLATRegValue, Pin)
				  end || Pin <- lists:reverse(PinList)]};
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Setup pull-up resistor of Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 PullUpRes	: mcp23x17_pull_up_res_en() | mcp23x17_pull_up_res_dis()
%% Output:
%%	 -
%% @end
-spec do_gpio_pull_up_resistor_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}, mcp23x17_pull_up_res()) -> ok | {error, term()}.
%% ====================================================================
do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, Pin, PullUpRes) when is_tuple(Pin)->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_->	do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), PullUpRes)
	end;
do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, Pin, PullUpRes) when is_integer(Pin)->
	do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, [Pin], PullUpRes);
do_gpio_pull_up_resistor_setup(CommType, HwAddr, Port, Pin, PullUpRes) when is_list(Pin)->
	%% Setup Pull-Up resistor
	GPURegAddr = case Port of
				  ?MCP23X17_PORT_A ->
					  ?GPUA_ADDR;
				  ?MCP23X17_PORT_B ->
					  ?GPUB_ADDR
	end,
	case read(CommType, HwAddr, GPURegAddr) of
		{ok, GPURegValue} ->
			NewGPURegValue = do_gpio_pull_up_resistor_setup_loop(PullUpRes, GPURegValue, Pin),
			
			%% Write changes in register.
			write(CommType, HwAddr, GPURegAddr, NewGPURegValue);
		
		ER->ER
	end.

do_gpio_pull_up_resistor_setup_loop(_PullUpRes, GPURegValue, []) ->
	GPURegValue;
do_gpio_pull_up_resistor_setup_loop(PullUpRes, GPURegValue, [Pin | T]) ->
	NewGPURegValue = case PullUpRes of
						 ?MCP23X17_PULL_UP_RES_ENABLED ->
							 bit_operations:bit_set(GPURegValue, Pin);
						 ?MCP23X17_PULL_UP_RES_DISABLED ->
							 bit_operations:bit_clear(GPURegValue, Pin)
					 end,
	do_gpio_pull_up_resistor_setup_loop(PullUpRes, NewGPURegValue, T).

%% ====================================================================
%% @doc
%% Setup input polarity of Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 IPol		: mcp23x17_in_pol()
%% Output:
%%	 -
%% @end
-spec do_gpio_input_polarity_setup(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin() | list(mcp23x17_pin()) | {mcp23x17_pin(), mcp23x17_pin()}, mcp23x17_in_pol()) -> ok | {error, term()}.
%% ====================================================================
do_gpio_input_polarity_setup(CommType, HwAddr, Port, Pin, IPol) when is_tuple(Pin)->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_->	do_gpio_input_polarity_setup(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), IPol)
	end;
do_gpio_input_polarity_setup(CommType, HwAddr, Port, Pin, IPol) when is_integer(Pin)->
	do_gpio_input_polarity_setup(CommType, HwAddr, Port, [Pin], IPol);
do_gpio_input_polarity_setup(CommType, HwAddr, Port, Pin, IPol) when is_list(Pin)->
	%% Setup input polarity
	IPolRegAddr = case Port of 
				  ?MCP23X17_PORT_A ->
					  ?IPOLA_ADDR;
				  ?MCP23X17_PORT_B ->
					  ?IPOLB_ADDR
	end,
	case read(CommType, HwAddr, IPolRegAddr) of
		{ok, IPolRegValue} ->
			NewIPolRegValue = do_gpio_input_polarity_setup(IPol, IPolRegValue, Pin),
			
			%% Write changes in regoster
			write(CommType, HwAddr, IPolRegAddr, NewIPolRegValue);
		
		ER->ER
	end.

do_gpio_input_polarity_setup(_IPol, IPolRegValue, []) ->
	IPolRegValue;
do_gpio_input_polarity_setup(IPol, IPolRegValue, [Pin | T]) ->
	NewIPolRegValue = case IPol of
						  ?MCP23X17_INPUT_POL_SAME_OF_PIN_LOGIC ->
							  bit_operations:bit_clear(IPolRegValue, Pin);
						  ?MCP23X17_INPUT_POL_OPPOSITE_OF_PIN_LOGIC ->
							  bit_operations:bit_set(IPolRegValue, Pin)
					  end,
	do_gpio_input_polarity_setup(IPol, NewIPolRegValue, T).

%% ====================================================================
%% @doc
%% Read MCP register
%% Input:
%%	 CommType	: type of serial communication. It can be MCP23X17_COMM_TYPE_SPI0 | MCP23X17_COMM_TYPE_I2C1
%%	 HwAddr		: HW address of MCP chip
%%	 RegAddr	: address of MCP register to be read
%% Output:
%%	 RegValue : value of register
%% @end
-spec read(mcp23x17_comm_type(), hw_addr(), reg_addr()) -> {ok, reg_value()} | {error, term()}.
%% ====================================================================
read(CommType, HwAddr, RegAddr) ->
	case CommType of
		?MCP23X17_COMM_TYPE_SPI0 ->
			%% CS leg is controlled by Raspberry Pi independently.
			case ale_handler:spi_transfer(get_comm_devicename(CommType), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_read(HwAddr), RegAddr, ?DUMMY_BYTE_FOR_READ])) of
				{ok, <<_,_,RegValue>>} ->
					{ok, RegValue};
				ER-> ER
			end;
		
		{?MCP23X17_COMM_TYPE_SPI0, SelectSPIByCS, UnselectSPIByCS} ->
			%% CS leg is controlled by SelectSPIByCS and UnselectSPIByCS MFAs.
			
			%% Select SPI device by CS
			spi_cs(SelectSPIByCS),
			
			case ale_handler:spi_transfer(get_comm_devicename(?MCP23X17_COMM_TYPE_SPI0), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_read(HwAddr), RegAddr, ?DUMMY_BYTE_FOR_READ])) of
				{ok, <<_,_,RegValue>>} ->
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS),
					
					{ok, RegValue};
				ER-> 
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS),
					ER
			end;
		
		?MCP23X17_COMM_TYPE_I2C1 ->
			ale_handler:i2c_write(get_comm_devicename(CommType), HwAddr, erlang:list_to_binary([RegAddr])),
			timer:sleep(10),
			case catch ale_handler:i2c_read(get_comm_devicename(CommType), HwAddr, 1) of
				{ok, <<Data>>} ->
					{ok, Data};
				ER->ER
			end
	end.

%% ====================================================================
%% @doc
%% Write MCP register
%% Input:
%%	 CommType : type of serial communication. It can be MCP23X17_COMM_TYPE_SPI0 | MCP23X17_COMM_TYPE_I2C1
%%	 HwAddr	  : HW address of MCP chip
%%	 RegAddr  : address of MCP register to be read
%%	 RegValue : value of register
%% Output:
%%	 -
%% @end
-spec write(mcp23x17_comm_type(), hw_addr(), reg_addr(), reg_value()) -> ok | {error, term()}.
%% ====================================================================
write(CommType, HwAddr, RegAddr, RegValue) ->
	case CommType of
		?MCP23X17_COMM_TYPE_SPI0 ->
			%% CS leg is controlled by Raspberry Pi independently.
			case ale_handler:spi_transfer(get_comm_devicename(CommType), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_write(HwAddr), RegAddr, RegValue])) of
				{error, Reason} ->
					{error, Reason};
				_->	ok
			end;
		
		{?MCP23X17_COMM_TYPE_SPI0, SelectSPIByCS, UnselectSPIByCS} ->
			%% CS leg is controlled by SelectSPIByCS and UnselectSPIByCS MFAs.
			
			%% Select SPI device by CS
			spi_cs(SelectSPIByCS),
			
			case catch ale_handler:spi_transfer(get_comm_devicename(?MCP23X17_COMM_TYPE_SPI0), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_write(HwAddr), RegAddr, RegValue])) of
				{ok,_} ->
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS);
				ER->
					%% Unselect SPI device by CS anyway.
					spi_cs(UnselectSPIByCS),
					{error, ER}
			end;
		
		?MCP23X17_COMM_TYPE_I2C1 ->
			ale_handler:i2c_write(get_comm_devicename(CommType), HwAddr, erlang:list_to_binary([RegAddr, RegValue]))
	end.

%% ====================================================================
%% Give the I2C or SPI device name.
%% Input:
%%	CommType	:	atom, type of serial communication. It can be MCP23X17_COMM_TYPE_SPI0 | MCP23X17_COMM_TYPE_I2C1
%% Output:
%%	CommChData	:	string
%% ====================================================================
get_comm_devicename(CommType) ->
	case CommType of
		?MCP23X17_COMM_TYPE_I2C1 ->
			?I2C_DEVICE_CH1_NAME;
		?MCP23X17_COMM_TYPE_SPI0 ->
			?SPI_DEVICE_CH0_NAME
	end.

%% ====================================================================
%% Enable/Disable Interrupt on the Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%%	 EnDis		: en | dis
%% Output:
%%	ok | {error, Reason}
%% ====================================================================
do_gpio_interrupt_en_dis(CommType, HwAddr, Port, Pin, EnDis) when is_tuple(Pin)->
	%% Make Pin range and recall the function.
	{FromPin, ToPin} = Pin,
	case FromPin of
		P when P > ToPin ->
			%% Invalid pin range.
			{error, {invalid_pin_range, Pin}};
		_->	do_gpio_interrupt_en_dis(CommType, HwAddr, Port, lists:seq(FromPin, ToPin), EnDis)
	end;
do_gpio_interrupt_en_dis(CommType, HwAddr, Port, Pin, EnDis) when is_integer(Pin)->
	do_gpio_interrupt_en_dis(CommType, HwAddr, Port, [Pin], EnDis);
do_gpio_interrupt_en_dis(CommType, HwAddr, Port, Pin, EnDis) when is_list(Pin)->
	GPINTENRegAddr = case Port of
						 ?MCP23X17_PORT_A ->
							 ?GPINTENA_ADDR;
						 ?MCP23X17_PORT_B ->
							 ?GPINTENB_ADDR
					end,
	case read(CommType, HwAddr, GPINTENRegAddr) of
		{ok, GPINTENRegValue} ->
			NewGPINTENRegValue = do_interrupt_en_dis_loop(EnDis, GPINTENRegValue, Pin),
			
			%% Write changes in the register
			write(CommType, HwAddr, GPINTENRegAddr, NewGPINTENRegValue);
		ER->ER
	end.

do_interrupt_en_dis_loop(_EnDis, GPINTENRegValue, []) ->
	GPINTENRegValue;
do_interrupt_en_dis_loop(EnDis, GPINTENRegValue, [Pin | T]) ->
	NewGPINTENRegValue = case EnDis of
							 en ->
								 bit_operations:bit_set(GPINTENRegValue, Pin);
							 dis ->
								 bit_operations:bit_clear(GPINTENRegValue, Pin)
						 end,
	do_interrupt_en_dis_loop(EnDis, NewGPINTENRegValue, T).


%% ====================================================================
%% Give the record (rMCP23x17_PORT_PIN) of PIN.
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port
%% Output:
%%	{ok, Rec} | {error, Reason}
%% ====================================================================
pin_rec_get(CommType, HwAddr, Port, Pin) ->
	%% Find pin in the ETS table
	case ets:lookup(?MCP23X17_PIN_TABLE, {CommType, HwAddr, Port, Pin}) of
		[Rec] when is_record(Rec, rMCP23x17_PORT_PIN) ->
			{ok, Rec};
		_->	{error, "Pin does not found."}
	end.

%% ====================================================================
%% Set Record of Pin in ETS table.
%% Input:
%%	Rec	:	record of rMCP23x17_PORT_PIN
%% Output:
%%	{ok, Rec} | {error, Reason}
%% ====================================================================
pin_rec_set(Rec) when is_record(Rec, rMCP23x17_PORT_PIN)->
	case ets:insert(?MCP23X17_PIN_TABLE, Rec) of
		true ->
			ok;
		ER ->
			{error, ER}
	end.

%% ====================================================================
%% Manipulate HW address for Read/Write operation. According to the
%% protocol description, this is required only for I2C, but the I2C driver
%% does this, so here in MCP23x17 level no need to do.
%% This is not needed for SPI protocol, BUT this is required for MCP23x17,
%% according to its documentation. This means, the 1st bit on HW address should be
%% changed accordingly.
%% ====================================================================
set_hwaddr_for_read(HwAddr) ->
	%% Shift left the address with 1 bit first.
	NewHwAddr = HwAddr bsl 1,
	
	%% Set 1st bit of NewHwAddr
	bit_operations:bit_set(NewHwAddr, 0).
	
set_hwaddr_for_write(HwAddr) ->
	%% Shift left the address with 1 bit first.
	NewHwAddr = HwAddr bsl 1,
	
	%% Set 1st bit of NewHwAddr
	bit_operations:bit_clear(NewHwAddr, 0).

%% ====================================================================
%% Select/Unselect SPI by CS leg. This is used when NOT the Pi handles
%% the CS independently, but other logic network controls that.
%% Input:
%%		CS_MFA	:	MFA tuple for set/clear CS
%% Output:
%%		ok | {error, Reason}
%% ====================================================================
spi_cs({M,F,A}) ->
	%% Special case if MFA belongs to this module.
	case {M,F} of
		{?MODULE, gpio_io_logical_level_setup} ->
			erlang:apply(M, do_gpio_io_logical_level_setup, A);
		_->	erlang:apply(M, F, A)
	end.

%% ====================================================================
%% @doc
%% Filter the PINs where interrupt is enabled.
%% @end
-spec do_gpio_interrupt_get_pin_list_with_int_enabled(data()) -> {ok, list(mcp23x17_pin())}.
%% ====================================================================
do_gpio_interrupt_get_pin_list_with_int_enabled(GPINTENRegValue) ->
	%% Filter the PINs where interrupt is enabled
	PinWithInterruptEnabledList = [begin
									   case bit_operations:bit_test(GPINTENRegValue, Pin) of
										   1 ->
											   %% Interrupt is enabled on the PIN
											   [Pin];
										   0 ->
											   %% Interrupt is NOT enabled on the PIN
											   []
									   end
								   end || Pin <- lists:seq(?MCP23X17_PIN0, ?MCP23X17_PIN7)],
	{ok, lists:append(PinWithInterruptEnabledList)}.

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

