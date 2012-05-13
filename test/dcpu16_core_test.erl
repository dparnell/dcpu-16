-module(dcpu16_core_test).
-compile([export_all]).

-author("me@danielparnell.com").
-include_lib("eunit/include/eunit.hrl").

-import(test_helpers, [attempt/1]).

simple_set() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([ 
							     { set, a, 16#1234 }
							   ])
			      ),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 2),
    
    dcpu16_core:get_reg(ResultCPU, a).

simple_add() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 16#BE00 },
							    { set, b, 16#00EF },
							    { add, a, b }
							   ])
			      ),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 6),
    
    dcpu16_core:get_reg(ResultCPU, a).

simple_subtraction() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 16#BEEF },
							    { set, b, 16#FEED },
							    { sub, a, b }
							  ])
			      ),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 6),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, ex)
    }.

indirect_register_write() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 16#1234 },
							    { set, b, 16#8000 },
							    { set, [b], a }
							   ])
			      ),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 5),
    
    dcpu16_core:ram(ResultCPU, 16#8000).

complicated_subtraction() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 16#30 },
							    { set, [16#1000], 16#20 },
							    { sub, a, [16#1000] }
							   ])
			      ),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 8),
    
    dcpu16_core:get_reg(ResultCPU, a).

test_subroutines() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 0 },
							    { jsr, 5 },
							    { ifn, a, 1},
							    { sub, pc, 1 }, %% failed
							    { sub, pc, 1 }, %% success
							    % subroutine starts here
							    { set, a, 1 },
							    { set, pc, pop }
							   ])
			      ),
			       
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 10),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, pc)
    }.

test_stack_operations() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 9876 },
							    { set, b, 0 },
							    { set, push, a},
							    { ifn, peek, 9876 },
							    { sub, pc, 1 }, %% test failed
							    { set, b, pop },
							    { ifn, b, 9876 },
							    { sub, pc, 1 }, %% test failed
							    { set, a, peek }, %% save off the top of the stack
							    { set, peek, 1234 }, 
							    { ifn, 1234, pop },
							    { sub, pc, 1 }, %% test failed
							    { set, push, a },  %% restore the top of the stack
							    { sub, pc, 1 } %% test successful
							   ])
			      ),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 23),
    
    dcpu16_core:get_reg(ResultCPU, pc).

subtractions_and_overflow() ->    
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { sub, 1, 1 }, % should do nothing except set the O flag to 0
							    { ifn, ex, 0 },
							    { sub, pc, 1}, % test failed
							    { ife, [1], 0},
							    { sub, pc, 1}, % test failed
							    { sub, 1, 2 }, % should also do nothing but set the O flag to 0xffff
							    { ifn, ex, 16#ffff },
							    { sub, pc, 1 }, % test failed
							    { ife, [1], 16#ffff },
							    { sub, pc, 1 }, % test failed
							    { add, pc, 2 }, % branch over the next two instructions
							    { add, pc, 1 }, % branch over the next instruction
							    { sub, pc, 3 }, % jump back a little
							    { sub, pc, 1 }  % test successful
							   ])
			      ),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 26),

    dcpu16_core:get_reg(ResultCPU, pc).

compare_instructions() ->    
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 1 },
							    { ifn, a, 1 },
							    { sub, pc, 1 }, % test failed
							    { ife, a, 1 },
							    { add, pc, 1 }, % skip the next instruction
							    { sub, pc, 1 }, % test failed
							    { set, b, 1 },
							    { ifg, a, b },
							    { sub, pc, 1 }, % test failed
							    { set, a, 10 },
							    { ifg, a, b },
							    { add, pc, 1 }, % skip the next instruction
							    { sub, pc, 1 }, % test failed
							    { set, a, 1 },
							    { set, b, 2 },
							    { ifb, b, a },
							    { sub, pc, 1 }, % test failed
							    { set, b, 3 },
							    { ifb, a, b },
							    { add, pc, 1 }, % skip the next instruction
							    { sub, pc, 1 }, % test failed
							    { sub, pc, 1 }  % test successful :)
							   ])
			      ),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 27),

    dcpu16_core:get_reg(ResultCPU, pc).

simple_multiply() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, dcpu16_asm:assemble([
							    { set, a, 5 },
							    { mul, a, 6 },
							    { ifn, a, 30 },
							    { sub, pc, 1 }, % test failed
							    { set, a, 16#abcd },
							    { mul, a, 16#cccc }
							   ])
			      ),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 11),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, ex)
    }.

simple_divide() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 100
					16#0064,
					16#8005, %% DIV A, 0
					16#800d, %% IFN A, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#81dd, %% IFN O, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#7c01, %% SET A, 100
					16#0064,
					16#8805, %% DIV A, 2
					16#7c0d, %% IFN A, 50
					16#0032,
					16#85c3, %% SUB PC, 1 ; test failed
					16#81dd, %% IFN O, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#7c01, %% SET A, 0x99
					16#0099, 
					16#7c05, %% DIV A, 0x00cc
					16#00cc,
					16#800d, %% IFN A, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#7ddd, %% IFN O, 0xc000
					16#c000, 
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 38),
    
    dcpu16_core:get_reg(ResultCPU, pc).


simple_mod() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#a801, %% SET A, 10
					16#8011, %% SET B, 0
					16#0406, %% MOD A, B
					16#800d, %% IFN A, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#a801, %% SET A, 10
					16#9c06, %% MOD A, 7
					16#8c0d, %% IFN A, 3
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 17),
    
    dcpu16_core:get_reg(ResultCPU, pc).

simple_shl() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1234
					16#1234,
					16#8407, %% SHL A, 1
					16#7c0d, %% IFN A, 0x2468
					16#2468,
					16#85c3, %% SUB PC, 1 ; test failed
					16#c007, %% SHL A, 16
					16#800d, %% IFN A, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#7ddd, %% IFN O, 0x2468
					16#2468,
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 19),
    
    dcpu16_core:get_reg(ResultCPU, pc).


simple_shr() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1234
					16#1234,
					16#8408, %% SHR A, 1
					16#7c0d, %% IFN A, 0x091a
					16#091a,
					16#85c3, %% SUB PC, 1 ; test failed
					16#c008, %% SHR A, 16
					16#800d, %% IFN A, 0
					16#85c3, %% SUB PC, 1 ; test failed
					16#7ddd, %% IFN O, 0x091a
					16#091a,
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 19),
    
    dcpu16_core:get_reg(ResultCPU, pc).

simple_and() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1234
					16#1234, 
					16#7c09, %% AND A, 0x5432
					16#5432,
					16#7c0d, %% IFN A, 0x1030
					16#1030,
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 10),
    
    dcpu16_core:get_reg(ResultCPU, pc).


simple_bor() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1200
					16#1200,
					16#7c0a, %% BOR A, 0x0034
					16#0034,
					16#7c0d, %% IFN A, 0x1234
					16#1234,
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 10),
    
    dcpu16_core:get_reg(ResultCPU, pc).

simple_xor() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0xFEED
					16#feed,
					16#7c0b, %% XOR A, 0xBEEF
					16#beef,
					16#7c0d, %% IFN A, 0x4002
					16#4002,
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 10),
    
    dcpu16_core:get_reg(ResultCPU, pc).

basic_test_() ->    
    [
     ?_assertEqual(16#1234, attempt(fun() -> simple_set() end)),
     ?_assertEqual(16#beef, attempt(fun() -> simple_add() end)),
     ?_assertMatch({16#c002, 16#ffff}, attempt(fun() -> simple_subtraction() end)),
     ?_assertEqual(16#1234, attempt(fun() -> indirect_register_write() end)),
     ?_assertEqual(16#0010, attempt(fun() -> complicated_subtraction() end)),
     ?_assertMatch({16#0001, 16#0004}, attempt(fun() -> test_subroutines() end)),
     ?_assertEqual(16#0012, attempt(fun() -> test_stack_operations() end)),
     ?_assertEqual(16#0013, attempt(fun() -> subtractions_and_overflow() end)),
     ?_assertEqual(16#0015, attempt(fun() -> compare_instructions() end)),
     ?_assertMatch({16#435c, 16#8970}, attempt(fun() -> simple_multiply() end)),
     ?_assertEqual(16#0018, attempt(fun() -> simple_divide() end)),
     ?_assertEqual(16#0009, attempt(fun() -> simple_mod() end)),
     ?_assertEqual(16#000c, attempt(fun() -> simple_shl() end)),
     ?_assertEqual(16#000c, attempt(fun() -> simple_shr() end)),
     ?_assertEqual(16#0007, attempt(fun() -> simple_and() end)),
     ?_assertEqual(16#0007, attempt(fun() -> simple_bor() end)),
     ?_assertEqual(16#0007, attempt(fun() -> simple_xor() end))
    ].
