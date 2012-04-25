%%%-------------------------------------------------------------------
%%% @author  <Daniel Parnell>
%%% @copyright (C) 2012, 
%%% @doc
%%%     DCPU16 core
%%% @end
%%% Created : 8 Apr 2012 by  <Daniel Parnell>
%%%-------------------------------------------------------------------

-module(dcpu16_core).

-export([init/0, ram/2, ram/3, get_reg/2, set_reg/3, cycle/1, cycle/2, print/1]).

%% everything about the CPU is in the cpu record
-record(cpu, { a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 0, overflow = 0, skip = false, w = [], target = none, pointer = none }).

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
	skip -> Cpu#cpu.skip;
	target -> Cpu#cpu.target;
	pointer -> Cpu#cpu.pointer
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
	skip -> Cpu#cpu{skip = Value};
	target -> Cpu#cpu{target = Value};
	pointer -> Cpu#cpu{pointer = Value}		      
    end.


%% Helper functions for instruction decoding
decode_read_next_ind(Reg) ->
    [ 
      nop, 
      { read_ind, pc },
      { inc, pc },
      { add, Reg },
      { write_reg, pointer },
      { read_reg_ind, pointer }
    ].

reg(2#000)->a;
reg(2#001)->b;
reg(2#010)->c;
reg(2#011)->x;
reg(2#100)->y;
reg(2#101)->z;
reg(2#110)->i;
reg(2#111)->j.

%% produce the micro operations for a read into the working stack
decode_read(N) when N=<2#00111 -> { read_reg, reg(N) };
decode_read(N) when N=<2#01111 -> { read_ind, reg(N bxor 2#01000) };
decode_read(N) when N=<2#10111 -> decode_read_next_ind(reg(N bxor 2#10000));
decode_read(2#011000) -> % POP  == [sp++]
    [ 
      {read_reg, sp},
      {write_reg, pointer},
      {read_ind, sp},
      {inc, sp}
    ]; 
decode_read(2#011001) -> % PEEK == [sp]
    [ 
      {read_reg, sp},
      {write_reg, pointer},
      {read_ind, sp}
    ]; 
decode_read(2#011010) ->  % PUSH == [--sp]
    [ 
      {dec, sp},
      {read_reg, sp},
      {write_reg, pointer},
      {read_ind, sp} 
    ];
decode_read(2#011011) -> { read_reg, sp };
decode_read(2#011100) -> { read_reg, pc };
decode_read(2#011101) -> { read_reg, overflow };
decode_read(2#011110) -> % [next word]
    [ 
      nop,
      {read_ind, pc},
      {inc, pc},
      {write_reg, pointer},
      {read_ind, pointer}
    ]; 
decode_read(2#011111) -> % next word (literal)
    [ 
      nop,
      {read_reg, pc},
      {inc, pc},
      {write_reg, pointer},
      {read_ind, pointer} 
    ]; 
decode_read(N) -> {lit, N bxor 2#100000}.

%% produce the micro operations for a write from the working stack

decode_write(N) when N=<2#00111 -> { write_reg, reg(N) };
decode_write(N) when N=<2#01111 -> { write_ind, reg(N bxor 2#01000) };
decode_write(N) when N=<2#10111 -> { write_ind, target};
decode_write(2#011000) -> { write_ind, target }; % POP  == [sp++]
decode_write(2#011001) -> { write_ind, target }; % PEEK == [sp]
decode_write(2#011010) -> { write_ind, target }; % PUSH == [--sp]
decode_write(2#011011) -> { write_reg, sp };
decode_write(2#011100) -> { write_reg, pc };
decode_write(2#011101) -> { write_reg, overflow };
decode_write(2#011110) -> { write_ind, target }; % [next word]
decode_write(2#011111) -> { write_ind, target }; % next word (literal)
decode_write(_) -> drop.

%% decode a non-basic opcode
decode_nonbasic_opcode(Opcode, A) ->
    case Opcode of
	1 -> [decode_read(A), nop, jsr];
	_ -> [reserved]
    end.

opcode(Op, B) ->
    case Op of
	0 -> case B of
		 1 -> "JSR";
		 _ -> "RESERVED"
	     end;
	1 -> "SET";
	2 -> "ADD";
	3 -> "SUB";
	4 -> "MUL";
	5 -> "DIV";
	6 -> "MOD";
	7 -> "SHL";
	8 -> "SHR";
	9 -> "AND";
	10 -> "BOR";
	11 -> "XOR";
	12 -> "IFE";
	13 -> "IFN";
	14 -> "IFG";
	15 -> "IFB"
    end.

decode_instruction(2#0000, A, B) -> decode_nonbasic_opcode(A, B);
decode_instruction(2#0001, A, B) -> [decode_read(A), drop, set_target, decode_read(B), nop, decode_write(A)];
decode_instruction(2#0010, A, B) -> [decode_read(A), set_target, decode_read(B), nop, add, decode_write(A)];
decode_instruction(2#0011, A, B) -> [decode_read(A), set_target, decode_read(B), nop, sub, decode_write(A)];
decode_instruction(2#0100, A, B) -> [decode_read(A), set_target, decode_read(B), nop, mul, decode_write(A)];
decode_instruction(2#0101, A, B) -> [decode_read(A), set_target, decode_read(B), nop, nop, divide, decode_write(A)];
decode_instruction(2#0110, A, B) -> [decode_read(A), set_target, decode_read(B), nop, nop, mod, decode_write(A)];
decode_instruction(2#0111, A, B) -> [decode_read(A), set_target, decode_read(B), nop, shl, decode_write(A)];
decode_instruction(2#1000, A, B) -> [decode_read(A), set_target, decode_read(B), nop, shr, decode_write(A)];
decode_instruction(2#1001, A, B) -> [decode_read(A), set_target, decode_read(B), logical_and, decode_write(A)];
decode_instruction(2#1010, A, B) -> [decode_read(A), set_target, decode_read(B), logical_or, decode_write(A)];
decode_instruction(2#1011, A, B) -> [decode_read(A), set_target, decode_read(B), logical_xor, decode_write(A)];
decode_instruction(2#1100, A, B) -> [decode_read(A), set_target, decode_read(B), nop, ife];
decode_instruction(2#1101, A, B) -> [decode_read(A), set_target, decode_read(B), nop, ifn];
decode_instruction(2#1110, A, B) -> [decode_read(A), set_target, decode_read(B), nop, ifg];
decode_instruction(2#1111, A, B) -> [decode_read(A), set_target, decode_read(B), nop, ifb].

operand_size(Source) ->
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

instruction_length(0, _, B) -> 1 + operand_size(B);
instruction_length(_, A, B) -> 1 + operand_size(A) + operand_size(B).

%% When there are no micro operations to perform we need to fetch a new instruction and decode it into micro-operations
cycle(Cpu, Ram, Cycles, [], CyclesLeft) ->
%    debug("Cpu = ~p~nRam = ~p~n", [Cpu, Ram]),

    Instruction = array:get(Cpu#cpu.pc, Ram),
    <<B:6, A:6, Opcode:4>> = <<Instruction:16>>,
    debug("~nOpcode = ~p A = ~p B = ~p~n", [opcode(Opcode, B), A, B]),

    case Cpu#cpu.skip of
	false -> Micro_ops = decode_instruction(Opcode, A, B),
%		 debug("Micro ops: ~p~n", [Micro_ops]),
		 cycle(Cpu#cpu{pc = Cpu#cpu.pc + 1, target = none, pointer = none}, Ram, Cycles, lists:flatten(Micro_ops), CyclesLeft);
	_ -> {Cpu#cpu{pc = Cpu#cpu.pc + instruction_length(Opcode, A, B), skip = false}, Ram, Cycles + 1, []}
    end;		    


%% We have micro operations so we need to perform them
cycle(Cpu, Ram, Cycles, [Micro_op|Micro_ops], CyclesLeft) ->
%    debug("~p ~p ~p ~p ~p~n", [Cpu, Ram, Cycles, Micro_op, Micro_ops]),
    debug("~p : ~p~n", [Cycles, Micro_op]),

    { NewCpu, NewRam, Cost } = execute_micro_op(Micro_op, Cpu, Ram),
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

%% simple micro operations
execute_micro_op(nop, Cpu, Ram) ->  { Cpu, Ram, 1 };				   
execute_micro_op(free_nop, Cpu, Ram) -> { Cpu, Ram, 0 };
execute_micro_op(drop, Cpu, Ram) -> [_|T] = Cpu#cpu.w,
				    { Cpu#cpu{w = T}, Ram, 0};
execute_micro_op(dup, Cpu, Ram) -> [Value|_] = Cpu#cpu.w,
				   { Cpu#cpu{w = lists:append([Value], Cpu#cpu.w)}, Ram, 0 };

execute_micro_op(set_target, Cpu, Ram) -> Address = Cpu#cpu.pointer,
					  { Cpu#cpu{target = Address}, Ram, 0 };

execute_micro_op({ add, Reg }, Cpu, Ram) -> [V|T] = Cpu#cpu.w,
					    Result = (V + reg(Cpu, Reg)) band 16#ffff,
					    { Cpu#cpu{w = lists:append([Result], T)}, Ram, 0 };
				   
execute_micro_op({ lit, Value }, Cpu, Ram) -> { Cpu#cpu{w = lists:append([Value], Cpu#cpu.w)}, Ram, 0 };

execute_micro_op({ read_reg, Reg }, Cpu, Ram) -> { Cpu#cpu{w = lists:append([reg(Cpu, Reg)], Cpu#cpu.w)}, Ram, 0};

execute_micro_op({ write_reg, Reg }, Cpu, Ram) -> [Value|T] = Cpu#cpu.w,
						  Temp = reg(Cpu, Reg, Value),
						  { Temp#cpu{w = T}, Ram, 0 };

execute_micro_op({ read_ind, Reg }, Cpu, Ram) -> Address = reg(Cpu, Reg),
						 Value = array:get(Address, Ram),
						 { Cpu#cpu{ w = lists:append([Value], Cpu#cpu.w) }, Ram, 0 };

execute_micro_op({ write_ind, Reg }, Cpu, Ram) -> Address = reg(Cpu, Reg),
						  [Value|T] = Cpu#cpu.w,
						  { Cpu#cpu{ w = T }, array:set(Address, Value, Ram), 0};

execute_micro_op({ inc, Reg }, Cpu, Ram) -> Value = (reg(Cpu, Reg) + 1) band 16#ffff,
					    { reg(Cpu, Reg, Value), Ram, 0 };

execute_micro_op({ dec, Reg }, Cpu, Ram) -> Value = (reg(Cpu, Reg) - 1) band 16#ffff,
					    { reg(Cpu, Reg, Value), Ram, 0 };

%% aritmetic operations
execute_micro_op(add, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Sum = A + B,
				   Overflow = (Sum band 16#10000) bsr 16,
				   { Cpu#cpu{ w = [Sum band 16#ffff], overflow = Overflow  }, Ram, 1 } ;

execute_micro_op(sub, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Sub = A - B,
				   Overflow = (Sub band 16#ffff0000) bsr 16,
				   { Cpu#cpu{ w = [Sub band 16#ffff], overflow = Overflow  }, Ram, 1 };
				   
execute_micro_op(mul, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Mul = A * B,
				   Overflow = (Mul bsr 16) band 16#ffff,
				   { Cpu#cpu{ w = [Mul band 16#ffff], overflow = Overflow  }, Ram, 1 };

execute_micro_op(divide, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				      case B of
					  0 -> { Cpu#cpu{ w = [0], overflow = 0  }, Ram, 1 };
					  _ -> Div = A div B,
					       Overflow = ((A bsl 16) div B) band 16#ffff,
					       { Cpu#cpu{ w = [Div band 16#ffff], overflow = Overflow  }, Ram, 1 }
				      end;

execute_micro_op(mod, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   case B of
				       0 -> { Cpu#cpu{ w = [0] }, Ram, 1 };
				       _ -> Mod = A rem B,
						   { Cpu#cpu{ w = [Mod band 16#ffff] }, Ram, 1 }
				   end;

execute_micro_op(shl, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Value = A bsl B,
				   Overflow = (Value bsr 16) band 16#ffff,
				   { Cpu#cpu{ w = [Value band 16#ffff], overflow = Overflow  }, Ram, 1 };

execute_micro_op(shr, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Value = A bsr B,
				   Overflow = ((A bsl 16) bsr B) band 16#ffff,
				   { Cpu#cpu{ w = [Value band 16#ffff], overflow = Overflow  }, Ram, 1 };

execute_micro_op(logical_and, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
					   Value = A band B,
					   { Cpu#cpu{ w = [Value] }, Ram, 1 };

%% test operations
execute_micro_op(ifn, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = A =:= B }, Ram, 1 };

execute_micro_op(ife, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = not(A =:= B) }, Ram, 1 };

execute_micro_op(ifg, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = not(A > B) }, Ram, 1 };

execute_micro_op(ifb, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = (A band B) =:= 0 }, Ram, 1 };

%% extended operations
execute_micro_op(jsr, Cpu, Ram) -> NewSP = (Cpu#cpu.sp - 1) band 16#ffff,
				   [Address|T] = Cpu#cpu.w,
				   { Cpu#cpu{ pc = Address, sp = NewSP, w = T}, array:set(NewSP, Cpu#cpu.pc, Ram), 1}.

micro_op_cost(Operation) ->
%    debug("Operation = ~p~n", [Operation]),
    case Operation of
	free_nop -> 0;
	pop_w -> 0;
	dup -> 0;
	drop -> 0;
	set_target -> 0;
	{ lit, _ } -> 0;
	{ inc, _ } -> 0;
	{ dec, _ } -> 0;
	{ add, _ } -> 0;
	{ read_reg, _ } -> 0;
	{ write_reg, _ } -> 0;
	{ read_ind, _ } -> 0;
	{ write_ind, _ } -> 0;
	write_lit -> 0;
	write_next_ind -> 0;
	_ -> 1	       
    end.

cycle(State) ->
%    debug("State = ~p~n", [State]),

    { Cpu, Ram, Cycles, Operations } = State,
    cycle(Cpu, Ram, Cycles, Operations, 1).

cycle(State, Count) ->
%    debug("~B ~p~n", [Count, State]),
    debug("---~B---~n", [Count]),
    if
	Count > 0 -> cycle(cycle(State), Count - 1);
	true -> State
    end.
			   
print(State) ->
    {Cpu, _, Cycles, _} = State,

    debug("~B ~p~n", [Cycles, Cpu]).
