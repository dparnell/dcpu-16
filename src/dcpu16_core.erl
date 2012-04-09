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

-export([init/1, cycle/1]).

%% everything about the CPU is in the cpu record
-record(cpu, { a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 16#ffff, overflow = 0, skip = 0, w = 0, w2 = 0 }).

%% Set up a new DCPU-16 instance with everything we need
init([]) ->
    { #cpu{}, array:new(16#10000, {default,0}), 0, [] }.

%% decode a non-basic opcode
decode_nonbasic_opcode(Cpu, Ram, Opcode, A) ->
    [nop].

decode_read(Value) ->
    [nop].

decode_read2(Value) ->
    [nop].

decode_write(Value) ->
    [nop].

%% When there are no micro operations to perform we need to fetch a new instruction and decode it into micro-operations
cycle({Cpu, Ram, Cycles, []}) ->
    case Cpu#cpu.skip of
	0 ->
	    Instruction = array:get(Ram, Cpu#cpu.pc),
	    <<Opcode:4, A:6, B:6>> = Instruction,

	    Micro_ops = case Opcode of
			    0 -> decode_nonbasic_opcode(Cpu, Ram, A, B);
			    1 -> [decode_read(B), nop, decode_write(A)];
			    2 -> [decode_read(B), decode_read2(A), nop, add, decode_write(A)];
			    3 -> [decode_read(B), decode_read2(A), nop, sub, decode_write(A)];
			    4 -> [decode_read(B), decode_read2(A), nop, mul, decode_write(A)];
			    5 -> [decode_read(B), decode_read2(A), nop, nop, divide, decode_write(A)];
			    6 -> [decode_read(B), decode_read2(A), nop, nop, mod, decode_write(A)];
			    7 -> [decode_read(B), decode_read2(A), nop, shl, decode_write(A)];
			    8 -> [decode_read(B), decode_read2(A), nop, shr, decode_write(A)];
			    9 -> [decode_read(B), decode_read2(A), logical_and, decode_write(A)];
			    10 -> [decode_read(B), decode_read2(A), logical_or, decode_write(A)];
			    11 -> [decode_read(B), decode_read2(A), logical_xor, decode_write(A)];
			    12 -> [decode_read(B), decode_read2(A), ife];
			    13 -> [decode_read(B), decode_read2(A), ifn];
			    14 -> [decode_read(B), decode_read2(A), ifg];
			    15 -> [decode_read(B), decode_read2(A), ifb];
			    _ -> error
			end,

	    {Cpu, Ram, Cycles + 1, Micro_ops};
	1 -> %% we want to skip the instruction
	    not_yet_implemented
    end;

%% We have micro operations so we need to perform them
cycle({Cpu, Ram, Cycles, [Micro_op|Micro_ops]}) ->
    Status = case Micro_op of
	nop -> ok;
	_ -> error
    end,

    case Status of
	ok -> {Cpu, Ram, Cycles + 1, Micro_ops};
	_ -> error
    end.
       
