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


decode_read_next_ind(Reg) ->
    [ nop, 
      { read_ind, pc },
      { inc, pc },
      { add, Reg },
      { write_reg, pointer },
      { read_reg_ind, pointer }
    ].

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
 	16 -> decode_read_next_ind(a);
	17 -> decode_read_next_ind(b);
	18 -> decode_read_next_ind(c);
	19 -> decode_read_next_ind(x);
	20 -> decode_read_next_ind(y);
	21 -> decode_read_next_ind(z);
	22 -> decode_read_next_ind(i);
	23 -> decode_read_next_ind(j);
	24 -> [ { read_reg, sp }, { write_reg, pointer }, { read_ind, sp }, { inc, sp }];  %% POP  == [sp++]
	25 -> [ { read_reg, sp }, { write_reg, pointer }, { read_ind, sp }];               %% PEEK == [sp]
	26 -> [ { dec, sp }, { read_reg, sp }, { write_reg, pointer}, { read_ind, sp} ];   %% PUSH == [--sp]
	27 -> { read_reg, sp };
	28 -> { read_reg, pc };
	29 -> { read_reg, overflow };
	30 -> [ nop, { read_ind, pc }, { inc, pc }, { write_reg, pointer }, { read_ind, pointer } ] ; %% [next word]
	31 -> [ nop, { read_reg, pc }, { inc, pc }, { write_reg, pointer }, { read_ind, pointer } ] ; %% next word (literal)
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
	16 -> { write_ind, target };
	17 -> { write_ind, target };
	18 -> { write_ind, target };
	19 -> { write_ind, target };
	20 -> { write_ind, target };
	21 -> { write_ind, target };
	22 -> { write_ind, target };
	23 -> { write_int, target };
	24 -> { write_ind, target }; %% POP
	25 -> { write_ind, target }; %% PEEK
	26 -> { write_ind, target }; %% PUSH
	27 -> { write_reg, sp };
	28 -> { write_reg, pc };
	29 -> { write_reg, o };
	30 -> { write_ind, target };
	31 -> { write_ind, target };
	_ -> drop
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
	    
%% When there are no micro operations to perform we need to fetch a new instruction and decode it into micro-operations
cycle(Cpu, Ram, Cycles, [], CyclesLeft) ->
%    debug("Cpu = ~p~nRam = ~p~n", [Cpu, Ram]),

    Instruction = array:get(Cpu#cpu.pc, Ram),
    <<B:6, A:6, Opcode:4>> = <<Instruction:16>>,
    debug("~nOpcode = ~p A = ~p B = ~p~n", [opcode(Opcode, B), A, B]),

    case Cpu#cpu.skip of
	false -> Micro_ops = case Opcode of
				 0 -> decode_nonbasic_opcode(A, B);
				 1 -> [decode_read(A), drop, set_target, decode_read(B), nop, decode_write(A)];
				 2 -> [decode_read(A), set_target, decode_read(B), nop, add, decode_write(A)];
				 3 -> [decode_read(A), set_target, decode_read(B), nop, sub, decode_write(A)];
				 4 -> [decode_read(A), set_target, decode_read(B), nop, mul, decode_write(A)];
				 5 -> [decode_read(A), set_target, decode_read(B), nop, nop, divide, decode_write(A)];
				 6 -> [decode_read(A), set_target, decode_read(B), nop, nop, mod, decode_write(A)];
				 7 -> [decode_read(A), set_target, decode_read(B), nop, shl, decode_write(A)];
				 8 -> [decode_read(A), set_target, decode_read(B), nop, shr, decode_write(A)];
				 9 -> [decode_read(A), set_target, decode_read(B), logical_and, decode_write(A)];
				 10 -> [decode_read(A), set_target, decode_read(B), logical_or, decode_write(A)];
				 11 -> [decode_read(A), set_target, decode_read(B), logical_xor, decode_write(A)];
				 12 -> [decode_read(A), set_target, decode_read(B), nop, ife];
				 13 -> [decode_read(A), set_target, decode_read(B), nop, ifn];
				 14 -> [decode_read(A), set_target, decode_read(B), nop, ifg];
				 15 -> [decode_read(A), set_target, decode_read(B), nop, ifb];
				 _ -> error
			     end,

%		 debug("Micro ops: ~p~n", [Micro_ops]),
		 cycle(Cpu#cpu{pc = Cpu#cpu.pc + 1, target = none, pointer = none}, Ram, Cycles, lists:flatten(Micro_ops), CyclesLeft);
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
				   
				   free_nop -> { Cpu, Ram, 0 };

				   drop -> [_|T] = Cpu#cpu.w,
					   { Cpu#cpu{w = T}, Ram, 0};
				   
				   dup -> [Value|_] = Cpu#cpu.w,
					  { Cpu#cpu{w = lists:append([Value], Cpu#cpu.w)}, Ram, 0 };

				   set_target -> Address = Cpu#cpu.pointer,
						 { Cpu#cpu{target = Address}, Ram, 0 };

				   { add, Reg } -> [V|T] = Cpu#cpu.w,
						   Result = (V + reg(Cpu, Reg)) band 16#ffff,
						   { Cpu#cpu{w = lists:append([Result], T)}, Ram, 0 };
				   
				   { lit, Value } -> { Cpu#cpu{w = lists:append([Value], Cpu#cpu.w)}, Ram, 0 };

				   { read_reg, Reg } -> { Cpu#cpu{w = lists:append([reg(Cpu, Reg)], Cpu#cpu.w)}, Ram, 0};

				   { write_reg, Reg } -> [Value|T] = Cpu#cpu.w,
							 Temp = reg(Cpu, Reg, Value),
							 { Temp#cpu{w = T}, Ram, 0 };

				   { read_ind, Reg } -> Address = reg(Cpu, Reg),
							Value = array:get(Address, Ram),
							{ Cpu#cpu{ w = lists:append([Value], Cpu#cpu.w) }, Ram, 0 };

				   { write_ind, Reg } -> Address = reg(Cpu, Reg),
							 [Value|T] = Cpu#cpu.w,
							 { Cpu#cpu{ w = T }, array:set(Address, Value, Ram), 0};


				   { inc, Reg } -> Value = (reg(Cpu, Reg) + 1) band 16#ffff,
						   { reg(Cpu, Reg, Value), Ram, 0 };

				   { dec, Reg } -> Value = (reg(Cpu, Reg) - 1) band 16#ffff,
						   { reg(Cpu, Reg, Value), Ram, 0 };

				   %% aritmetic operations
				   add -> [B, A] = Cpu#cpu.w,
					  Sum = A + B,
					  Overflow = (Sum band 16#10000) bsr 16,
					  { Cpu#cpu{ w = [Sum band 16#ffff], overflow = Overflow  }, Ram, 1 } ;

				   sub -> [B, A] = Cpu#cpu.w,
					  Sub = A - B,
					  Overflow = (Sub band 16#ffff0000) bsr 16,
					  { Cpu#cpu{ w = [Sub band 16#ffff], overflow = Overflow  }, Ram, 1 };
				   
				   mul -> [B, A] = Cpu#cpu.w,
					  Mul = A * B,
					  Overflow = (Mul bsr 16) band 16#ffff,
					  { Cpu#cpu{ w = [Mul band 16#ffff], overflow = Overflow  }, Ram, 1 };

				   divide -> [B, A] = Cpu#cpu.w,
					     case B of
						 0 -> { Cpu#cpu{ w = [0], overflow = 0  }, Ram, 1 };
						 _ -> Div = A div B,
						      Overflow = ((A bsl 16) div B) band 16#ffff,
						      { Cpu#cpu{ w = [Div band 16#ffff], overflow = Overflow  }, Ram, 1 }
					     end;

				   mod -> [B, A] = Cpu#cpu.w,
					  case B of
					      0 -> { Cpu#cpu{ w = [0] }, Ram, 1 };
					      _ -> Mod = A rem B,
						   { Cpu#cpu{ w = [Mod band 16#ffff] }, Ram, 1 }
					  end;

				   %% test operations
				   ifn -> [B, A] = Cpu#cpu.w,
					  { Cpu#cpu{ w = [], skip = A =:= B }, Ram, 1 };

				   ife -> [B, A] = Cpu#cpu.w,
					  { Cpu#cpu{ w = [], skip = not(A =:= B) }, Ram, 1 };

				   ifg -> [B, A] = Cpu#cpu.w,
					  { Cpu#cpu{ w = [], skip = not(A > B) }, Ram, 1 };

				   ifb -> [B, A] = Cpu#cpu.w,
					  { Cpu#cpu{ w = [], skip = (A band B) =:= 0 }, Ram, 1 };

				   %% extended operations
				   jsr -> NewSP = (Cpu#cpu.sp - 1) band 16#ffff,
					  [Address|T] = Cpu#cpu.w,
					  { Cpu#cpu{ pc = Address, sp = NewSP, w = T}, array:set(NewSP, Cpu#cpu.pc, Ram), 1};

				   reserved -> reserved_dcpu_instruction
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
