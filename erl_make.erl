-module(erl_make).

-export([make/1]).

make(Mode) ->
    case make:all([{d, Mode}]) of
    	error ->
    		error;
    	_ ->
    		test_suite:test()
    end.
