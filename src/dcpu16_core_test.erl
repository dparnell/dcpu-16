-module(dcpu16_core_test).
-compile([export_all]).

-author("me@danielparnell.com").
-include_lib("eunit/include/eunit.hrl").

simple_set() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1234
					16#1234
				       ]),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 2),
    
    dcpu16_core:get_reg(ResultCPU, a).

simple_add() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0xBE00
					16#be00,
					16#7c11, %% SET B, 0x00EF
					16#00ef,
					16#0402  %% ADD A, B
				       ]),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 6),
    
    dcpu16_core:get_reg(ResultCPU, a).

simple_subtraction() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0xBEEF
					16#beef, 
					16#7c11, %% SET B, 0xFEED
					16#feed,
					16#0403  %% SUB A, B
				       ]),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 6),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, overflow)
    }.

indirect_register_write() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1234
					16#1234,
					16#7c11, %% SET B, 0x8000
					16#8000,
					16#0091  %% SET [B], A
				       ]),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 5),
    
    dcpu16_core:ram(ResultCPU, 16#8000).

complicated_subtraction() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x30
					16#0030, 
					16#7de1, %% SET [0x1000], 0x20
					16#1000,
					16#0020,
					16#7803, %% SUB A, [0x1000]
					16#1000
				       ]),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 8),
    
    dcpu16_core:get_reg(ResultCPU, a).

test_subroutines() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#8001, %% SET A, 0
					16#7c10, %% JSR FOO
					16#0006,
					16#840d, %% IFN A, 1
					16#85c3, %% SUB PC, 1 ; failed 
					16#85c3, %% SUB PC, 1 ; success
					         %% :foo
					16#8401, %% SET A, 1
					16#61c1  %% SET PC, POP
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 11),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, pc)
    }.

test_stack_operations() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01,  %% SET A, 9876
					16#2694,
					16#8011,  %% SET B, 0
					16#01a1,  %% SET PUSH, A
					16#7d9d,  %% IFN PEEK, 9876
					16#2694,
					16#85c3,  %% SUB PC, 1 ; test failed
					16#6011,  %% SET B, POP
					16#7c1d,  %% IFN B, 9876
					16#2694,  
					16#85c3,  %% SUB PC, 1 ; test failed

					16#7da1,  %% SET PUSH, 55
					16#0037,
					16#7da1,  %% SET PUSH, 90 
					16#005a, 
					16#6182,  %% ADD POP, POP
					16#6a11,  %% SET 1, PUSH
					16#6a11,  %% SET 1, PUSH
					16#6001,  %% SET A, POP
					16#6011,  %% SET B, POP
					16#7c0d,  %% IFN A, 145
					16#0091,  
					16#85c3,  %% SUB PC, 1 ; test failed
					16#7c1d,  %% IFN B, 55
					16#0037, 
					16#85c3,  %% SUB PC, 1 ; test failed

					16#6401,  %% SET A, PEEK    ; save off the top of the stack
					16#7d91,  %% SET PEEK, 1234 ; set the top ofthe stack
					16#04d2, 
					16#7d8d,  %% IFN POP, 1234
					16#04d2, 
					16#85c3,  %% SUB PC, 1 ; test failed
					16#01a1,  %% SET PUSH, A    ; restore the top of thestack

					16#85c3   %% SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 41),
    
    dcpu16_core:get_reg(ResultCPU, pc).

subtractions_and_overflow() ->    
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#8613,  %% SUB 0x01, 0x01  ; should do nothing except set the O flag to 0
					16#81dd,  %% IFN O, 0
					16#85c3,  %% SUB PC, 1 ; test failed
					16#81ec,  %% IFE [0x01], 0
					16#0001,  
					16#85c3,  %% SUB PC, 1 ; test failed
					16#8a13,  %% SUB 0x01, 0x02  ; should also do nothing but set the O flag to 0xffff
					16#7ddd,  %% IFN O, 0xffff
					16#ffff, 
					16#85c3,  %% SUB PC, 1 ; test failed
					16#7dec,  %% IFE [0x01], 0xffff
					16#0001, 
					16#ffff, 
					16#85c3,  %% SUB PC, 1 ; test failed
					16#7dc1,  %% SET PC, test_sub2       ; jump over the next instruction
					16#0012,
					16#7dc1,  %% SET PC, test_sub3       ; jump to the return instruction
					16#0013,
					16#8dc3,  %% :test_sub2 SUB PC, 3               ; branch back to the jump to the return
					16#85c3   %% :test_sub3 SUB PC, 1 ; test passed
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 26),

    dcpu16_core:get_reg(ResultCPU, pc).

compare_instructions() ->    
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#8401, %% SET A, 1
					16#840d, %% IFN A, 1
					16#85c3, %% SUB PC, 1 ; test failed
					16#840c, %% IFE A, 1
					16#85c2, %% ADD PC, 1 ; skip the next instruction
					16#85c3, %% SUB PC, 1 ; test failed
					16#8411, %% SET B, 1
					16#040e, %% IFG A, B
					16#85c3, %% SUB PC, 1 ; test failed
					16#a801, %% SET A, 10
					16#040e, %% IFG A, B
					16#85c2, %% ADD PC, 1 ; skip the next instruction
					16#85c3, %% SUB PC, 1 ; test failed
					16#8401, %% SET A, 1
					16#8811, %% SET B, 2
					16#040f, %% IFB B, A
					16#85c3, %% SUB PC, 1 ; test failed
					16#8c11, %% SET B, 3
					16#040f, %% IFB A, B
					16#85c2, %% ADD PC, 1 ; skip the next instruction
					16#85c3, %% SUB PC, 1 ; test failed
					16#85c3  %% SUB PC, 1 ; test successful :)
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 27),

    dcpu16_core:get_reg(ResultCPU, pc).

simple_multiply() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#9401, %% SET A, 5
					16#9804, %% MUL A, 6
					16#f80d, %% IFN A, 30
					16#85c3, %% SUB PC, 1 ; failed
					16#7c01, %% SET A, 0xABCD
					16#abcd,
					16#7c04, %% MUL A, 0xCCCC
					16#cccc
				       ]),

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 11),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, overflow)
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

attempt(F) ->
    try
	F()
    catch
	Type:X ->
	    io:format("~p~n", [{Type, X, erlang:get_stacktrace()}]),
	    ?assert(unhandled_exception)
    end.

basic_test_() ->    
    [
     ?_assertEqual(16#1234, attempt(fun() -> simple_set() end)),
     ?_assertEqual(16#beef, attempt(fun() -> simple_add() end)),
     ?_assertMatch({16#c002, 16#ffff}, attempt(fun() -> simple_subtraction() end)),
     ?_assertEqual(16#1234, attempt(fun() -> indirect_register_write() end)),
     ?_assertEqual(16#0010, attempt(fun() -> complicated_subtraction() end)),
     ?_assertMatch({16#0001, 16#0005}, attempt(fun() -> test_subroutines() end)),
     ?_assertEqual(16#0021, attempt(fun() -> test_stack_operations() end)),
     ?_assertEqual(16#0013, attempt(fun() -> subtractions_and_overflow() end)),
     ?_assertEqual(16#0015, attempt(fun() -> compare_instructions() end)),
     ?_assertMatch({16#435c, 16#8970}, attempt(fun() -> simple_multiply() end)),
     ?_assertEqual(16#0018, attempt(fun() -> simple_divide() end)),
     ?_assertEqual(16#0009, attempt(fun() -> simple_mod() end)),
     ?_assertEqual(16#000c, attempt(fun() -> simple_shl() end)),
     ?_assertEqual(16#000c, attempt(fun() -> simple_shr() end)),
     ?_assertEqual(16#0007, attempt(fun() -> simple_and() end)),
     ?_assertEqual(16#0007, attempt(fun() -> simple_bor() end)),
     ?_assertEqual(16#0008, attempt(fun() -> simple_xor() end))
    ].
