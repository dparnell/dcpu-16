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
-record(cpu, { a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 0, overflow = 0, skip = false, w = [], target = none }).

%% Set up a new DCPU-16 instance with everything we need
init() ->
    { #cpu{}, array:new(16#10000, {default, 0}), 0, [] }.

debug(Format, Values) ->
    io:fwrite(Format, Values).

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
	27 -> { read_reg, sp };
	28 -> { read_reg, pc };
	29 -> { read_reg, overflow };
	30 -> read_next_ind;
	31 -> read_next_literal;
	 _ -> {lit, Source band 31}
    end.

decode_target(Source) ->
    case Source of
	 0 -> { target_reg, a };
	 1 -> { target_reg, b };
	 2 -> { target_reg, c };
	 3 -> { target_reg, x };
	 4 -> { target_reg, y };
	 5 -> { target_reg, z };
	 6 -> { target_reg, i };
	 7 -> { target_reg, j };
	 8 -> { target_ind, a };
	 9 -> { target_ind, b };
	10 -> { target_ind, c };
	11 -> { target_ind, x };
	12 -> { target_ind, y };
	13 -> { target_ind, z };
	14 -> { target_ind, i };
	15 -> { target_ind, j };
	16 -> { target_next_ind, a };
	17 -> { target_next_ind, b };
	18 -> { target_next_ind, c };
	19 -> { target_next_ind, x };
	20 -> { target_next_ind, y };
	21 -> { target_next_ind, z };
	22 -> { target_next_ind, i };
	23 -> { target_next_ind, j };
	24 -> target_pop;
	25 -> target_peek;
	26 -> target_push;
	27 -> { target_reg, sp };
	28 -> { target_reg, pc };
	29 -> { target_reg, overflow };
	30 -> target_next_ind;
	31 -> target_next_lit;
	 _ -> {target_lit, Source band 31}
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
	27 -> { write_reg, sp };
	28 -> { write_reg, pc };
	29 -> { write_reg, o };
	30 -> write_next_ind;
	_ -> write_lit
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
cycle(Cpu, Ram, Cycles, [], CyclesLeft) ->
%    debug("Cpu = ~p~nRam = ~p~n", [Cpu, Ram]),

    Instruction = array:get(Cpu#cpu.pc, Ram),
    <<B:6, A:6, Opcode:4>> = <<Instruction:16>>,
    debug("~nOpcode = ~p A = ~p B = ~p~n", [Opcode, A, B]),

    case Cpu#cpu.skip of
	false -> Micro_ops = case Opcode of
				 0 -> decode_nonbasic_opcode(A, B);
				 1 -> [decode_target(A), decode_read(B), nop, decode_write(A)];
				 2 -> [decode_target(A), read_target, decode_read(B), nop, add, decode_write(A)];
				 3 -> [decode_target(A), read_target, decode_read(B), nop, sub, decode_write(A)];
				 4 -> [decode_target(A), read_target, decode_read(B), nop, mul, decode_write(A)];
				 5 -> [decode_target(A), read_target, decode_read(B), nop, nop, divide, decode_write(A)];
				 6 -> [decode_target(A), read_target, decode_read(B), nop, nop, mod, decode_write(A)];
				 7 -> [decode_target(A), read_target, decode_read(B), nop, shl, decode_write(A)];
				 8 -> [decode_target(A), read_target, decode_read(B), nop, shr, decode_write(A)];
				 9 -> [decode_target(A), read_target, decode_read(B), logical_and, decode_write(A)];
				 10 -> [decode_target(A), read_target, decode_read(B), logical_or, decode_write(A)];
				 11 -> [decode_target(A), read_target, decode_read(B), logical_xor, decode_write(A)];
				 12 -> [decode_target(A), read_target, decode_read(B), ife];
				 13 -> [decode_target(A), read_target, decode_read(B), ifn];
				 14 -> [decode_target(A), read_target, decode_read(B), ifg];
				 15 -> [decode_target(A), read_target, decode_read(B), ifb];
				 _ -> error
			     end,
		 
		 cycle(Cpu#cpu{pc = Cpu#cpu.pc + 1, target = none}, Ram, Cycles, Micro_ops, CyclesLeft);
	_ -> %% we want to skip the instruction
	    case Opcode of
		0 -> {Cpu#cpu{pc = Cpu#cpu.pc + 1 + calculate_pc_usage(B), skip = false}, Ram, Cycles + 1, []};
		_ -> {Cpu#cpu{pc = Cpu#cpu.pc + 1 + calculate_pc_usage(A) + calculate_pc_usage(B), skip = false}, Ram, Cycles + 1, []}
	    end
    end;		    
	 
%% We have micro operations so we need to perform them
cycle(Cpu, Ram, Cycles, [Micro_op|Micro_ops], CyclesLeft) ->
%    debug("~p ~p ~p ~p ~p~n", [Cpu, Ram, Cycles, Micro_op, Micro_ops]),
    debug("~p : ~p~n", [Cycles, Micro_op]),

    { NewCpu, NewRam, Cost } = case Micro_op of
				   %% internal operations

				   nop -> { Cpu, Ram, 1 };
				   
				   { lit, Value } -> { Cpu#cpu{w = lists:append([Value], Cpu#cpu.w)}, Ram, 0};

				   read_next_literal -> Literal = array:get(Cpu#cpu.pc, Ram),
							{ Cpu#cpu{ pc = Cpu#cpu.pc + 1, w = lists:append([Literal], Cpu#cpu.w)}, Ram, 1 };

				   { read_reg, Reg } -> { Cpu#cpu{w = lists:append([reg(Cpu, Reg)], Cpu#cpu.w)}, Ram, 0};

				   { write_reg, Reg } -> [Value|T] = Cpu#cpu.w,
							 Temp = reg(Cpu, Reg, Value),
							 { Temp#cpu{w = T}, Ram, 0 };

				   { target_reg, Reg } -> { Cpu#cpu{target = {register, Reg}}, Ram, 0 };

				   { target_ind, Reg } -> { Cpu#cpu{target = reg(Cpu, Reg)}, Ram, 0 };
				   
				   { target_lit, Value } -> { Cpu#cpu{target = { lit, Value} }, Ram, 0 };

				   target_next_ind -> Address = array:get(Cpu#cpu.pc, Ram),
						      { Cpu#cpu{ pc = Cpu#cpu.pc + 1, target = Address}, Ram, 1 };

				   read_target -> Value = case Cpu#cpu.target of
							      { register, Reg } -> reg(Cpu, Reg);
							      { lit, Lit } -> Lit;
							      peek -> array:get(Cpu#cpu.sp, Ram);
							      _ -> array:get(Cpu#cpu.target, Ram)
							  end,
						  { Cpu#cpu{ w = lists:append([Value], Cpu#cpu.w)}, Ram, 0 };
				   
				   { write_ind, Reg } -> Address = reg(Cpu, Reg),
							 [Value|T] = Cpu#cpu.w,
							 debug("Writing ~p to ~p via reg ~p~n", [Value, Address, Reg]),
							 { Cpu#cpu{ w = T }, array:set(Address, Value, Ram), 1};

				   write_next_ind -> Address = Cpu#cpu.target,
						     [Value|T] = Cpu#cpu.w,
						     debug("Writing ~p to ~p~n", [Value, Address]),
						     { Cpu#cpu{ w = T }, array:set(Address, Value, Ram), 0 };
				   
				   write_lit -> [Value|T] = Cpu#cpu.w,
						{ Cpu#cpu{ w = T }, Ram, 0 };

				   read_next_ind -> Address = array:get(Cpu#cpu.pc, Ram),
						    Value = array:get(Address, Ram),
						    debug("Read ~p from ~p~n", [Value, Address]),  
						    { Cpu#cpu{ pc = Cpu#cpu.pc + 1, w = lists:append([Value], Cpu#cpu.w) }, Ram, 1 };
				   
				   %% stack operations
				   target_push -> { Cpu#cpu{ target = push }, Ram, 0 };
				   target_peek -> { Cpu#cpu{ target = peek }, Ram, 0 };

				   write_push -> [Value|T] = Cpu#cpu.w,
						 NewSP = (Cpu#cpu.sp - 1) band 16#ffff,
						 { Cpu#cpu{ sp = NewSP, w = T}, array:set(NewSP, Value, Ram), 0};

				   read_pop -> Value = array:get(Cpu#cpu.sp, Ram),
					       NewSP = (Cpu#cpu.sp) band 16#ffff,
					       { Cpu#cpu{ sp = NewSP, w = lists:append([Value], Cpu#cpu.w)}, Ram, 0};

				   %% aritmetic operations
				   add -> [A, B] = Cpu#cpu.w,
					  Sum = A + B,
					  Overflow = (Sum band 16#10000) bsr 16,
					  { Cpu#cpu{ w = [Sum band 16#ffff], overflow = Overflow  }, Ram, 1 } ;

				   sub -> [A, B] = Cpu#cpu.w,
					  Sub = B - A,
					  debug("~p - ~p = ~p~n", [A, B, Sub]),
					  Overflow = (Sub band 16#ffff0000) bsr 16,
					  { Cpu#cpu{ w = [Sub band 16#ffff], overflow = Overflow  }, Ram, 1 };
				   
				   %% test operations
				   ifn -> [A, B] = Cpu#cpu.w,
					  { Cpu#cpu{ w = [], skip = A =:= B }, Ram, 1 };

				   ife -> [A, B] = Cpu#cpu.w,
					  { Cpu#cpu{ w = [], skip = not(A =:= B) }, Ram, 1 };

				   %% extended operations
				   jsr -> NewSP = (Cpu#cpu.sp - 1) band 16#ffff,
					  [Address|T] = Cpu#cpu.w,
					  { Cpu#cpu{ pc = Address, sp = NewSP, w = T}, array:set(NewSP, Cpu#cpu.pc, Ram), 1}
			       end,

    debug("~p~n", [NewCpu]),

    case length(Micro_ops) of
	0 -> {NewCpu, NewRam, Cycles + Cost, Micro_ops};
	_ ->
	    [Next_Op|_] = Micro_ops,
	    NextCost = micro_op_cost(Next_Op),

	    if 
		NextCost =:= 0 -> cycle(NewCpu, NewRam, Cycles + Cost, Micro_ops, CyclesLeft - Cost);
		CyclesLeft - Cost > 0 -> cycle(NewCpu, NewRam, Cycles + Cost, Micro_ops, CyclesLeft - Cost);
		true -> {NewCpu, NewRam, Cycles + Cost, Micro_ops}
	    end
    end.

micro_op_cost(Operation) ->
%    debug("Operation = ~p~n", [Operation]),
    case Operation of
	{ lit, _ } -> 0;
	{ read_reg, _ } -> 0;
	{ write_reg, _ } -> 0;
	{ target_reg, _ } -> 0;
	{ target_ind, _ } -> 0;
	{ target_lit, _ } -> 0;
	target_push -> 0;
	target_peek -> 0;
	write_push -> 0;
	write_lit -> 0;
	read_pop -> 0;
	read_target -> 0;
	write_target -> 0;
	write_next_ind -> 0;
	_ -> 1	       
    end.

cycle(State) ->
%    debug("State = ~p~n", [State]),

    { Cpu, Ram, Cycles, Operations } = State,
    cycle(Cpu, Ram, Cycles, Operations, 1).

cycle(State, Count) ->
%    debug("~B ~p~n", [Count, A]),
    debug("---~B---~n", [Count]),
    if
	Count > 0 -> cycle(cycle(State), Count - 1);
	true -> State
    end.
			   
print(State) ->
    {Cpu, _, Cycles, _} = State,

    debug("~B ~p~n", [Cycles, Cpu]).
