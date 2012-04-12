%%%-------------------------------------------------------------------
%%% @author  <Daniel Parnell>
%%% @copyright (C) 2012, 
%%% @doc
%%%     DCPU16 core
%%% @end
%%% Created : 8 Apr 2012 by  <Daniel Parnell>
%%%-------------------------------------------------------------------

-module(dcpu16_core).

-import(array, [new/2, get/2, set/3]).

-export([init/0, ram/2, ram/3, get_reg/2, set_reg/3, cycle/1, cycle/2, print/1]).

%% everything about the CPU is in the cpu record
-record(cpu, { a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 16#ffff, overflow = 0, skip = 0, w = [] }).

%% Set up a new DCPU-16 instance with everything we need
init() ->
    { #cpu{}, array:new(16#10000, {default, 0}), 0, [] }.

%% Get the value in a RAM location
ram(State, Address) ->
    {_, Ram, _, _} = State,
    array:get(Address, Ram).

%% Set the value in a RAM location
ram(State, Address, [H|T]) ->
    { Cpu, Ram, Cycles, Operation } = State,
    NewRam = array:set(Address, H, Ram),
    ram({Cpu, NewRam, Cycles, Operation}, Address + 1, T);

ram(State, _, []) ->
    State;

ram(State, Address, Value) ->
    ram(State, Address, [Value]).

get_reg(State, Reg) ->
    { Cpu, _, _, _ } = State,
    reg(Cpu, Reg).

set_reg(State, Reg, Value) ->
    { Cpu, Ram, Cycles, Operation } = State,

    { reg(Cpu, Reg, Value), Ram, Cycles, Operation }.

reg(Cpu, Reg) ->

    case Reg of
	a -> Cpu#cpu.a;
	b -> Cpu#cpu.b;
	c -> Cpu#cpu.c;
	x -> Cpu#cpu.x;
	y -> Cpu#cpu.y;
	z -> Cpu#cpu.z;
	i -> Cpu#cpu.i;
	j -> Cpu#cpu.j;
	pc -> Cpu#cpu.pc;
	sp -> Cpu#cpu.sp;
	overflow -> Cpu#cpu.overflow;
	skip -> Cpu#cpu.skip
    end.

reg(Cpu, Reg, Value) ->

    case Reg of
	a -> Cpu#cpu{a = Value};
	b -> Cpu#cpu{b = Value};
	c -> Cpu#cpu{c = Value};
	x -> Cpu#cpu{x = Value};
	y -> Cpu#cpu{y = Value};
	z -> Cpu#cpu{z = Value};
	i -> Cpu#cpu{i = Value};
	j -> Cpu#cpu{j = Value};
	pc -> Cpu#cpu{pc = Value};
	sp -> Cpu#cpu{sp = Value};
	overflow -> Cpu#cpu{overflow = Value};
	skip -> Cpu#cpu{skip = Value}
    end.


%% produce the micro operations for a read into the working stack
decode_read(Source) ->
    case Source of
	 0 -> { read_reg, a };
	 1 -> { read_reg, b };
	 2 -> { read_reg, c };
	 3 -> { read_reg, x };
	 4 -> { read_reg, y };
	 5 -> { read_reg, z };
	 6 -> { read_reg, i };
	 7 -> { read_reg, j };
	 8 -> { read_ind, a };
	 9 -> { read_ind, b };
	10 -> { read_ind, c };
	11 -> { read_ind, x };
	12 -> { read_ind, y };
	13 -> { read_ind, z };
	14 -> { read_ind, i };
	15 -> { read_ind, j };
	16 -> { read_next_ind, a };
	17 -> { read_next_ind, b };
	18 -> { read_next_ind, c };
	19 -> { read_next_ind, x };
	20 -> { read_next_ind, y };
	21 -> { read_next_ind, z };
	22 -> { read_next_ind, i };
	23 -> { read_next_ind, j };
	24 -> read_pop;
	25 -> read_peek;
	26 -> read_push;
	27 -> read_sp;
	28 -> read_pc;
	29 -> read_o;
	30 -> read_next_ind;
	31 -> read_next_literal;
	 _ -> {lit, Source band 31}
    end.

%% produce the micro operations for a write from the working stack
decode_write(Destination) ->
    case Destination of
	 0 -> { write_reg, a };
	 1 -> { write_reg, b };
	 2 -> { write_reg, c };
	 3 -> { write_reg, x };
	 4 -> { write_reg, y };
	 5 -> { write_reg, z };
	 6 -> { write_reg, i };
	 7 -> { write_reg, j };
	 8 -> { write_ind, a };
 	 9 -> { write_ind, b };
	10 -> { write_ind, c };
	11 -> { write_ind, x };
	12 -> { write_ind, y };
	13 -> { write_ind, z };
	14 -> { write_ind, i };
	15 -> { write_ind, j };
	16 -> { write_next_ind, a };
	17 -> { write_next_ind, b };
	18 -> { write_next_ind, c };
	19 -> { write_next_ind, x };
	20 -> { write_next_ind, y };
	21 -> { write_next_ind, z };
	22 -> { write_next_ind, i };
	23 -> { write_next_ind, j };
	24 -> write_pop;
	25 -> write_peek;
	26 -> write_push;
	27 -> write_sp;
	28 -> write_pc;
	29 -> write_o;
	30 -> write_next_ind;
	_ -> nop
    end.

%% decode a non-basic opcode
decode_nonbasic_opcode(Opcode, A) ->
    case Opcode of
	1 -> [decode_read(A), nop, jsr];
	_ -> [reserved]
    end.

calculate_pc_usage(Source) ->
    case Source of
	16 -> 1;
	17 -> 1;
	18 -> 1;
	19 -> 1;
	20 -> 1;
	21 -> 1;
	22 -> 1;
	23 -> 1;
	30 -> 1;
  	 _ -> 0
    end.

%% When there are no micro operations to perform we need to fetch a new instruction and decode it into micro-operations
cycle(Cpu, Ram, Cycles, []) ->
%    io:fwrite("Cpu = ~p~nRam = ~p~n", [Cpu, Ram]),

    Instruction = array:get(Cpu#cpu.pc, Ram),
    <<B:6, A:6, Opcode:4>> = <<Instruction:16>>,
%    io:fwrite("Opcode = ~p A = ~p B = ~p~n", [Opcode, A, B]),

    case Cpu#cpu.skip of
	0 ->
	    Micro_ops = case Opcode of
			     0 -> decode_nonbasic_opcode(A, B);
			     1 -> [decode_read(B), nop, decode_write(A)];
			     2 -> [decode_read(B), decode_read(A), nop, add, decode_write(A)];
			     3 -> [decode_read(B), decode_read(A), nop, sub, decode_write(A)];
			     4 -> [decode_read(B), decode_read(A), nop, mul, decode_write(A)];
			     5 -> [decode_read(B), decode_read(A), nop, nop, divide, decode_write(A)];
			     6 -> [decode_read(B), decode_read(A), nop, nop, mod, decode_write(A)];
			     7 -> [decode_read(B), decode_read(A), nop, shl, decode_write(A)];
			     8 -> [decode_read(B), decode_read(A), nop, shr, decode_write(A)];
			     9 -> [decode_read(B), decode_read(A), logical_and, decode_write(A)];
			    10 -> [decode_read(B), decode_read(A), logical_or, decode_write(A)];
			    11 -> [decode_read(B), decode_read(A), logical_xor, decode_write(A)];
			    12 -> [decode_read(B), decode_read(A), ife];
			    13 -> [decode_read(B), decode_read(A), ifn];
			    14 -> [decode_read(B), decode_read(A), ifg];
			    15 -> [decode_read(B), decode_read(A), ifb];
			    _ -> error
			end,

	    cycle(Cpu#cpu{pc = Cpu#cpu.pc + 1}, Ram, Cycles, Micro_ops);
	1 -> %% we want to skip the instruction
	    case Opcode of
		0 -> {Cpu#cpu{pc = Cpu#cpu.pc + 1 + calculate_pc_usage(B), skip = 0}, Ram, Cycles + 1, []};
		_ -> {Cpu#cpu{pc = Cpu#cpu.pc + 1 + calculate_pc_usage(A) + calculate_pc_usage(B), skip = 0}, Ram, Cycles + 1, []}
	    end
    end;		    
	 
%% We have micro operations so we need to perform them
cycle(Cpu, Ram, Cycles, [Micro_op|Micro_ops]) ->
%    io:fwrite("~p ~p ~p ~p ~p~n", [Cpu, Ram, Cycles, Micro_op, Micro_ops]),
%    io:fwrite("~p~n", [Micro_op]),

    {NewCpu, NewRam, Cost } = case Micro_op of
			   nop -> { Cpu, Ram, 1 };
			   { read_reg, Reg } -> { Cpu#cpu{w = lists:append([reg(Cpu, Reg)], Cpu#cpu.w)}, Ram, 0};
			   { write_reg, Reg } -> [Value|T] = Cpu#cpu.w,
						 Temp = reg(Cpu, Reg, Value),
						 { Temp#cpu{w = T}, Ram, 0 };
			   read_next_literal -> Literal = array:get(Cpu#cpu.pc, Ram),
						{ Cpu#cpu{ pc = Cpu#cpu.pc + 1, w = lists:append([Literal], Cpu#cpu.w)}, Ram, 1 };
				  add -> [A, B] = Cpu#cpu.w,
					 Sum = A + B,
					 Overflow = (Sum band 16#10000) bsr 16,
					 { Cpu#cpu{ w = [Sum band 16#ffff], overflow = Overflow  }, Ram, 1 } 
						
		       end,

    case Cost of
	0 when length(Micro_ops) > 0 -> cycle(NewCpu, NewRam, Cycles, Micro_ops);
	1 when length(Micro_ops) > 0 ->
	    [Next_Op|_] = Micro_ops,
	    NextCost = micro_op_cost(Next_Op),

	    if 
		NextCost == 0 -> cycle(NewCpu, NewRam, Cycles, Micro_ops);
		true -> {NewCpu, NewRam, Cycles + Cost, Micro_ops}
	    end;
	_ -> {NewCpu, NewRam, Cycles + Cost, Micro_ops}
    end.

micro_op_cost(Operation) ->
%    io:fwrite("Operation = ~p~n", [Operation]),
    case Operation of
	nop -> 1;
	{ read_reg, _ } -> 0;
	{ write_reg, _ } -> 0;
	read_next_literal -> 1;
	add -> 1
    end.

cycle(State) ->
%    io:fwrite("State = ~p~n", [State]),

    { Cpu, Ram, Cycles, Operations } = State,
    cycle(Cpu, Ram, Cycles, Operations).

cycle(A, Count) ->
%    io:fwrite("~B ~p~n", [Count, A]),
    if
	Count > 0 -> cycle(cycle(A), Count - 1);
	true -> A
    end.
			   
print(State) ->
    {Cpu, _, Cycles, _} = State,

    io:fwrite("~B ~p~n", [Cycles, Cpu]).
