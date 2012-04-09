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
-record(cpu, { a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 16#ffff, overflow = 0, skip = 0, w = [] }).

%% Set up a new DCPU-16 instance with everything we need
init([]) ->
    { #cpu{}, array:new(16#10000, {default,0}), 0, [] }.

%% produce the micro operations for a read into the working stack
decode_read(Source) ->
    case Source of
	 0 -> read_a;
	 1 -> read_b;
	 2 -> read_c;
	 3 -> read_x;
	 4 -> read_y;
	 5 -> read_z;
	 6 -> read_i;
	 7 -> read_j;
	 8 -> read_ind_a;
	 9 -> read_ind_b;
	10 -> read_ind_c;
	11 -> read_ind_x;
	12 -> read_ind_y;
	13 -> read_ind_z;
	14 -> read_ind_i;
	15 -> read_ind_j;
	16 -> read_next_ind_a;
	17 -> read_next_ind_b;
	18 -> read_next_ind_c;
	19 -> read_next_ind_x;
	20 -> read_next_ind_y;
	21 -> read_next_ind_z;
	22 -> read_next_ind_i;
	23 -> read_next_ind_j;
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
	 0 -> write_a;
	 1 -> write_b;
	 2 -> write_c;
	 3 -> write_x;
	 4 -> write_y;
	 5 -> write_z;
	 6 -> write_i;
	 7 -> write_j;
	 8 -> write_ind_a;
 	 9 -> write_ind_b;
	10 -> write_ind_c;
	11 -> write_ind_x;
	12 -> write_ind_y;
	13 -> write_ind_z;
	14 -> write_ind_i;
	15 -> write_ind_j;
	16 -> write_next_ind_a;
	17 -> write_next_ind_b;
	18 -> write_next_ind_c;
	19 -> write_next_ind_x;
	20 -> write_next_ind_y;
	21 -> write_next_ind_z;
	22 -> write_next_ind_i;
	23 -> write_next_ind_j;
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
cycle({Cpu, Ram, Cycles, []}) ->
    Instruction = array:get(Ram, Cpu#cpu.pc),
    <<Opcode:4, A:6, B:6>> = Instruction,

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

	    cycle({Cpu#cpu{pc = Cpu#cpu.pc + 1}, Ram, Cycles + 1, Micro_ops});
	1 -> %% we want to skip the instruction
	    case Opcode of
		0 -> {Cpu#cpu{pc = Cpu#cpu.pc + 1 + calculate_pc_usage(B), skip = 0}, Ram, Cycles + 1, []};
		_ -> {Cpu#cpu{pc = Cpu#cpu.pc + 1 + calculate_pc_usage(A) + calculate_pc_usage(B), skip = 0}, Ram, Cycles + 1, []}
	    end
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
