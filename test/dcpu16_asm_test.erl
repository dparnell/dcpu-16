-module(dcpu16_asm_test).
-compile([export_all]).

-author("me@danielparnell.com").
-include_lib("eunit/include/eunit.hrl").

-import(test_helpers, [attempt/1]).

assemble_ifn() ->
    Code = dcpu16_asm:assemble([
				{ set, a, 9876 },
				{ set, b, 0 },
				{ set, push, a},
				{ ifn, peek, 9876 }
			       ]),
    io:fwrite("~p~n", [dcpu16_core:list_to_hex(Code)]),

    Code.

basic_test_() ->   
    [
     ?_assertMatch([
		    16#7c01,  %% SET A, 9876
		    16#2694, 
		    16#8421,  %% SET B, 0
		    16#0301,  %% SET PUSH, A
		    16#7f33,  %% IFN PEEK, 9876
		    16#2694
		   ], attempt(fun() -> assemble_ifn() end))
     
    ].
