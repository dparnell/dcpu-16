-module(dcpu16_core_test).
-compile([export_all]).

-author("me@danielparnell.com").
-include_lib("eunit/include/eunit.hrl").

simple_code() ->
    CPU = dcpu16_core:init(),
    
    ReadyCPU = dcpu16_core:ram(CPU, 0, [
					16#7c01, %% SET A, 0x1234
					16#1234
				       ]),
    
    ResultCPU = dcpu16_core:cycle(ReadyCPU, 2),
    
    dcpu16_core:get_reg(ResultCPU, a).

simple_code2() ->
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
     ?_assertEqual(16#1234, attempt(fun() -> simple_code() end)),
     ?_assertEqual(16#beef, attempt(fun() -> simple_code2() end))
    ].
