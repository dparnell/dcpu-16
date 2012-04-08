%%%-------------------------------------------------------------------
%%% @author  <Daniel Parnell>
%%% @copyright (C) 2012, 
%%% @doc
%%%     DCPU16 core
%%% @end
%%% Created : 8 Apr 2012 by  <Daniel Parnell>
%%%-------------------------------------------------------------------

-module(dcpu16_core).

-import(array, [new/1, new/2]).

-export([init/1, cycle/1]).

-record(cpu, {a = 0, b = 0, c = 0, x = 0, y = 0, z = 0, i = 0, j = 0, pc = 0, sp = 16#ffff, overflow = 0 }).


init([]) ->
    { #cpu{}, array:new(16#10000, {default,0}), 0, [] }.

cycle({cpu, ram, cycles, micro_ops}) ->
    ok.
