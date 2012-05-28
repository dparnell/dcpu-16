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

%%    io:fwrite("~p~n", [dcpu16_core:list_to_hex(Code)]),

    Code.

assemble_sub() ->
    Code = dcpu16_asm:assemble([
				{ sub, 1, 1 },
				{ ifn, ex, 0 },
				{ sub, pc, 1}
			       ]),

%%    io:fwrite("~p~n", [dcpu16_core:list_to_hex(Code)]),

    Code.

assemble_negative_numbers() ->
    Code = dcpu16_asm:assemble([
				{ set, a, -1 },
				{ mli, a, -2 }
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
		   ], attempt(fun() -> assemble_ifn() end)),
     ?_assertMatch([
		    16#8be3, %% SUB 1, 1
		    16#0001,
		    16#87b3, %% IFN EX, 0
		    16#8b83  %% SUB PC, 1
		   ], attempt(fun() -> assemble_sub() end)),
    ?_assertMatch([
		    16#8001,  %% SET A, -1
		    16#7c05,  %% MLI A, -2
		    16#fffe
		   ], attempt(fun() -> assemble_negative_numbers() end))
     
    ].
