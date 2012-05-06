-module(test_helpers).
-compile([export_all]).

-author("me@danielparnell.com").
-include_lib("eunit/include/eunit.hrl").


attempt(F) ->
    try
	F()
    catch
	Type:X ->
	    io:format("~p~n", [{Type, X, erlang:get_stacktrace()}]),
	    ?assert(unhandled_exception)
    end.
