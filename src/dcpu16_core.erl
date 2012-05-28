%%%-------------------------------------------------------------------
%%% @author  <Daniel Parnell>
%%% @copyright (C) 2012, 
%%% @doc
%%%     DCPU16 core implementing the DCPU 1.7 spec as defined here http://pastebin.com/raw.php?i=Q4JvQvnM
%%% @end
%%% Created : 8 Apr 2012 by  <Daniel Parnell>
%%%-------------------------------------------------------------------

-module(dcpu16_core).

-export([init/0, ram/2, ram/3, get_reg/2, set_reg/3, cycle/1, cycle/2, print/1, list_to_hex/1]).

%% everything about the CPU is in the cpu record
-record(cpu, { a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 0, ex = 0, ia = 0, skip = false, w = [], target = none, pointer = none }).

list_to_hex(List) ->
    io:format("~s", [[io_lib:format("~4.16.0B ",[X]) || X <- List ]]).


%% Set up a new DCPU-16 instance with everything we need
init() ->
    { #cpu{}, array:new(16#10000, {default, 0}), 0, [] }.

debug(Format, Values) ->
    io:fwrite(Format, Values).
%%   ok.

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
	ex -> Cpu#cpu.ex;
	ia -> Cpu#cpu.ia;
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
	ex -> Cpu#cpu{ex = Value};
	ia -> Cpu#cpu{ia = Value};
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
decode_read(N) when N=<16#07 -> { read_reg, reg(N) };
decode_read(N) when N=<16#0f -> { read_ind, reg(N bxor 2#01000) };
decode_read(N) when N=<16#17 -> decode_read_next_ind(reg(N bxor 2#10000));
decode_read(16#018) -> % POP  == [sp++]
    [ 
      {read_reg, sp},
      {write_reg, pointer},
      {read_ind, sp},
      {inc, sp}
    ]; 
decode_read(16#019) -> % PEEK == [sp]
    [ 
      {read_reg, sp},
      {write_reg, pointer},
      {read_ind, sp}
    ]; 
decode_read(16#1a) ->  % [sp + next] / PICK n
    decode_read_next_ind(sp);

decode_read(16#1b) -> { read_reg, sp };
decode_read(16#1c) -> { read_reg, pc };
decode_read(16#1d) -> { read_reg, ex };
decode_read(16#1e) -> % [next word]
    [ 
      nop,
      {read_ind, pc},
      {inc, pc},
      {write_reg, pointer},
      {read_ind, pointer}
    ]; 
decode_read(16#1f) -> % next word (literal)
    [ 
      nop,
      {read_reg, pc},
      {inc, pc},
      {write_reg, pointer},
      {read_ind, pointer} 
    ]; 
decode_read(N) -> {lit, (N bxor 2#100000)-1}.

%% produce the micro operations for a write from the working stack

decode_write(N) when N=<16#07 -> { write_reg, reg(N) };
decode_write(N) when N=<16#0f -> { write_ind, reg(N bxor 2#01000) };
decode_write(N) when N=<16#17 -> { write_ind, target};
decode_write(16#18) -> { write_ind, target }; % POP  == [sp++]
decode_write(16#19) -> { write_ind, target }; % PEEK == [sp]
decode_write(16#1a) -> { write_ind, target }; % PUSH == [--sp]
decode_write(16#1b) -> { write_reg, sp };
decode_write(16#1c) -> { write_reg, pc };
decode_write(16#1d) -> { write_reg, ex };
decode_write(16#1e) -> { write_ind, target }; % [next word]
decode_write(16#1f) -> { write_ind, target }; % next word (literal)
decode_write(_) -> drop.

%% decode a non-basic opcode
decode_nonbasic_opcode(1, A) -> [decode_read(A), nop, jsr];
decode_nonbasic_opcode(_, _) -> reserved.

opcode(Op, A) ->
    case Op of
	16#00 -> case A of
		     1 -> "JSR";
		     _ -> "RESERVED"
		 end;
	16#01 -> "SET";
	16#02 -> "ADD";
	16#03 -> "SUB";
	16#04 -> "MUL";
	16#05 -> "MLI";
	16#06 -> "DIV";
	16#07 -> "DVI";
	16#08 -> "MOD";
	16#09 -> "MDI";
	16#0a -> "AND";
	16#0b -> "BOR";
	16#0c -> "XOR";
	16#0d -> "SHR";
	16#0e -> "ASR";
	16#0f -> "SHL";
	16#10 -> "IFB";
	16#11 -> "IFC";
	16#12 -> "IFE";
	16#13 -> "IFN";
	16#14 -> "IFG";
	16#15 -> "IFA";
	16#16 -> "IFL";
	16#17 -> "IFU";
	_ -> Op
    end.

decode_instruction(16#00, A, B) -> decode_nonbasic_opcode(B, A);
decode_instruction(16#01, A, B) -> [decode_read(A), nop, decode_read(B), drop, set_target, decode_write(B)];
decode_instruction(16#02, A, B) -> [decode_read(A), decode_read(B), set_target, nop, add, decode_write(B)];
decode_instruction(16#03, A, B) -> [decode_read(A), decode_read(B), set_target, nop, sub, decode_write(B)];
decode_instruction(16#04, A, B) -> [decode_read(A), decode_read(B), set_target, nop, mul, decode_write(B)];
decode_instruction(16#05, A, B) -> [decode_read(A), decode_read(B), set_target, nop, mli, decode_write(B)];
decode_instruction(16#06, A, B) -> [decode_read(A), decode_read(B), set_target, nop, nop, divide, decode_write(B)];
decode_instruction(16#07, A, B) -> [decode_read(A), decode_read(B), set_target, nop, nop, dvi, decode_write(B)];
decode_instruction(16#08, A, B) -> [decode_read(A), decode_read(B), set_target, nop, nop, mod, decode_write(B)];
decode_instruction(16#09, A, B) -> [decode_read(A), decode_read(B), set_target, nop, nop, mdi, decode_write(B)];
decode_instruction(16#0a, A, B) -> [decode_read(A), decode_read(B), set_target, logical_and, decode_write(B)];
decode_instruction(16#0b, A, B) -> [decode_read(A), decode_read(B), set_target, logical_or, decode_write(B)];
decode_instruction(16#0c, A, B) -> [decode_read(A), decode_read(B), set_target, logical_xor, decode_write(B)];
decode_instruction(16#0d, A, B) -> [decode_read(A), decode_read(B), set_target, shr, decode_write(B)];
decode_instruction(16#0e, A, B) -> [decode_read(A), decode_read(B), set_target, asr, decode_write(B)];
decode_instruction(16#0f, A, B) -> [decode_read(A), decode_read(B), set_target, shl, decode_write(B)];
decode_instruction(16#10, A, B) -> [decode_read(A), decode_read(B), nop, ifb];
decode_instruction(16#11, A, B) -> [decode_read(A), decode_read(B), nop, ifc];
decode_instruction(16#12, A, B) -> [decode_read(A), decode_read(B), nop, ife];
decode_instruction(16#13, A, B) -> [decode_read(A), decode_read(B), nop, ifn];
decode_instruction(16#14, A, B) -> [decode_read(A), decode_read(B), nop, ifg];
decode_instruction(16#15, A, B) -> [decode_read(A), decode_read(B), nop, ifa];
decode_instruction(16#16, A, B) -> [decode_read(A), decode_read(B), nop, ifl];
decode_instruction(16#17, A, B) -> [decode_read(A), decode_read(B), nop, ifu].

operand_size(Source) ->
    case Source of
	16#10 -> 1;
	16#11 -> 1;
	16#12 -> 1;
	16#13 -> 1;
	16#14 -> 1;
	16#15 -> 1;
	16#16 -> 1;
	16#17 -> 1;
	16#1a -> 1;
	16#1e -> 1;
	16#1f -> 1;
  	 _ -> 0
    end.

instruction_length(0, _, B) -> 1 + operand_size(B);
instruction_length(_, A, B) -> 1 + operand_size(A) + operand_size(B).

%% When there are no micro operations to perform we need to fetch a new instruction and decode it into micro-operations
cycle(Cpu, Ram, Cycles, [], CyclesLeft) ->
    debug("Cpu = ~p~nRam = ~p~n", [Cpu, Ram]),

    Instruction = array:get(Cpu#cpu.pc, Ram),
    <<A:6, B:5, Opcode:5>> = <<Instruction:16>>,
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

signed(A) when A > 16#7fff -> -(A bxor 16#ffff) - 1;
signed(A) -> A.

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
				   Sum = B + A,
				   Overflow = (Sum band 16#10000) bsr 16,
				   { Cpu#cpu{ w = [Sum band 16#ffff], ex = Overflow  }, Ram, 1 } ;

execute_micro_op(sub, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Sub = B - A,
				   Overflow = (Sub band 16#ffff0000) bsr 16,
				   { Cpu#cpu{ w = [Sub band 16#ffff], ex = Overflow  }, Ram, 1 };
				   
execute_micro_op(mul, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Mul = B * A,
				   Overflow = (Mul bsr 16) band 16#ffff,
				   { Cpu#cpu{ w = [Mul band 16#ffff], ex = Overflow  }, Ram, 1 };

execute_micro_op(mli, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Mul = signed(B) * signed(A),
				   Overflow = (Mul bsr 16) band 16#ffff,
				   { Cpu#cpu{ w = [Mul band 16#ffff], ex = Overflow  }, Ram, 1 };

execute_micro_op(divide, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				      case A of
					  0 -> { Cpu#cpu{ w = [0], ex = 0  }, Ram, 1 };
					  _ -> Div = B div A,
					       Overflow = ((B bsl 16) div A) band 16#ffff,
					       { Cpu#cpu{ w = [Div band 16#ffff], ex = Overflow  }, Ram, 1 }
				      end;

execute_micro_op(dvi, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   case A of
				       0 -> { Cpu#cpu{ w = [0], ex = 0  }, Ram, 1 };
				       _ -> Div = signed(B) div signed(A),
					    Overflow = ((signed(B) bsl 16) div signed(A)) band 16#ffff,
					    { Cpu#cpu{ w = [Div band 16#ffff], ex = Overflow  }, Ram, 1 }
				   end;

execute_micro_op(mod, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   case A of
				       0 -> { Cpu#cpu{ w = [0] }, Ram, 1 };
				       _ -> Mod = B rem A,
						   { Cpu#cpu{ w = [Mod band 16#ffff] }, Ram, 1 }
				   end;

execute_micro_op(shl, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Value = B bsl A,
				   Overflow = (Value bsr 16) band 16#ffff,
				   { Cpu#cpu{ w = [Value band 16#ffff], ex = Overflow  }, Ram, 1 };

execute_micro_op(shr, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   Value = B bsr A,
				   Overflow = ((A bsl 16) bsr B) band 16#ffff,
				   { Cpu#cpu{ w = [Value band 16#ffff], ex = Overflow  }, Ram, 1 };

execute_micro_op(logical_and, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
					   Value = B band A,
					   { Cpu#cpu{ w = [Value] }, Ram, 1 };

execute_micro_op(logical_or, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
					  Value = B bor A,
					  { Cpu#cpu{ w = [Value] }, Ram, 1 };

execute_micro_op(logical_xor, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
					   Value = B bxor A,
					   { Cpu#cpu{ w = [Value] }, Ram, 1 };

%% test operations
execute_micro_op(ifn, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = B =:= A }, Ram, 1 };

execute_micro_op(ife, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = not(B =:= A) }, Ram, 1 };

execute_micro_op(ifg, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = not(B > A) }, Ram, 1 };

execute_micro_op(ifb, Cpu, Ram) -> [B, A] = Cpu#cpu.w,
				   { Cpu#cpu{ w = [], skip = (B band A) =:= 0 }, Ram, 1 };

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
