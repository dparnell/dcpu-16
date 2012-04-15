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
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 6),
    
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

    ResultCPU = dcpu16_core:cycle(ReadyCPU, 100),
    
    {
      dcpu16_core:get_reg(ResultCPU, a),
      dcpu16_core:get_reg(ResultCPU, pc)
    }.
    

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
     ?_assertMatch({16#0001, 16#0005}, attempt(fun() -> test_subroutines() end))
    ].
