-module(dcpu16_core_test).
-compile([export_all]).

-author("me@danielparnell.com").
-include_lib("eunit/include/eunit.hrl").

simple_code() ->
    try
	CPU = dcpu16_core:init(),

	CPU2 = dcpu16_core:ram(CPU,  0, 16#7c01),  %% SET A, 0x1234
	CPU3 = dcpu16_core:ram(CPU2, 1, 16#1234),
	
	ResultCPU = dcpu16_core:cycle(CPU3, 2),
	
	dcpu16_core:get_reg(ResultCPU, a)

    catch
	Type:X ->
	    io:format("~p~n", [{Type, X, erlang:get_stacktrace()}]),
	    ?assert(unhandled_exception)
    end.

basic_test_() ->
    [
     ?_assertEqual(16#1234, simple_code())
    ].
