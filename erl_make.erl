-module(erl_make).

-export([make/1]).

post_process(test) ->
    case make:files(filelib:wildcard("test/*.erl"), [debug_info, {outdir, "ebin"}, {i, "include"}]) of
	error -> error;
	_ -> test_suite:test()
    end;

post_process(_) -> ok.
    
make(Mode) ->
    case make:all([{d, Mode}]) of
    	error -> error;
    	_ -> post_process(Mode)
    end.
