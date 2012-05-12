%%%-------------------------------------------------------------------
%%% @author  <Daniel Parnell>
%%% @copyright (C) 2012, 
%%% @doc
%%%     DCPU16 assembler for DCPU16 1.7
%%% @end
%%% Created : 5 May 2012 by  <Daniel Parnell>
%%%-------------------------------------------------------------------

-module(dcpu16_asm).

-export([assemble/1]).

assemble(Code) ->
    { Instructions, Symbols } = lists:mapfoldl(fun process_instruction/2, dict:new(), Code),
    lists:filter(fun(X) -> X /= nothing end, lists:flatten(Instructions)).

encode_read(a) -> 0;
encode_read(b) -> 1;
encode_read(c) -> 2;
encode_read(x) -> 3;
encode_read(y) -> 4;
encode_read(z) -> 5;
encode_read(i) -> 6;
encode_read(j) -> 7;
encode_read([a]) -> 8;
encode_read([b]) -> 9;
encode_read([c]) -> 10;
encode_read([x]) -> 11;
encode_read([y]) -> 12;
encode_read([z]) -> 13;
encode_read([i]) -> 14;
encode_read([j]) -> 15;
encode_read([a, _]) -> 16;
encode_read([b, _]) -> 17;
encode_read([c, _]) -> 18;
encode_read([x, _]) -> 19;
encode_read([y, _]) -> 20;
encode_read([z, _]) -> 21;
encode_read([i, _]) -> 22;
encode_read([j, _]) -> 23;
encode_read(pop) -> 24;
encode_read([sp]) -> 25;
encode_read(peek) -> 25;
encode_read([sp, _]) -> 26;
encode_read(sp) -> 27;
encode_read(pc) -> 28;
encode_read(ex) -> 29;
encode_read([A]) when is_integer(A) -> 30;
encode_read(A) when A < -1 -> 31;
encode_read(A) when A > 30 -> 31;
encode_read(A) -> 33 + A. %% -1 to 30, this works because 33 + -1 =:= 32

encode_write(a) -> 16#00;
encode_write(b) -> 16#01;
encode_write(c) -> 16#02;
encode_write(x) -> 16#03;
encode_write(y) -> 16#04;
encode_write(z) -> 16#05;
encode_write(i) -> 16#06;
encode_write(j) -> 16#07;
encode_write([a]) -> 16#08;
encode_write([b]) -> 16#09;
encode_write([c]) -> 16#0a;
encode_write([x]) -> 16#0b;
encode_write([y]) -> 16#0c;
encode_write([z]) -> 16#0d;
encode_write([i]) -> 16#0e;
encode_write([j]) -> 16#0f;
encode_write([a, _]) -> 16#10;
encode_write([b, _]) -> 16#11;
encode_write([c, _]) -> 16#12;
encode_write([x, _]) -> 16#13;
encode_write([y, _]) -> 16#14;
encode_write([z, _]) -> 16#15;
encode_write([i, _]) -> 16#16;
encode_write([j, _]) -> 16#17;
encode_write(pop) -> error;
encode_write(push) -> 16#18;
encode_write([sp]) -> 16#19;
encode_write(peek) -> 16#19;
encode_write([sp, _]) -> 19#1a;
encode_write(sp) -> 16#1b;
encode_write(pc) -> 16#1c;
encode_write(ex) -> 16#1d;
encode_write([A]) when is_integer(A) -> 16#1e;
encode_write(_) -> 16#1f.

encode_instruction(Opcode, A, B) ->
    Read = encode_read(A),
    Write = encode_write(B),

    <<Instruction:16>> = << Read:6, Write:5, Opcode:5>>,
    Instruction.

encode_instruction(Opcode, A) ->
    Read = encode_read(A),

    <<Instruction:16>> = << Read:6, Opcode:5, 0:5>>,
    Instruction.

encode_next([A]) when is_integer(A) ->
    A;
encode_next(A) when is_integer(A) ->
    if A < -1 -> A;
       A > 30 -> A; 
       true -> nothing
    end;
encode_next([_, A]) when is_integer(A) ->
    A;
encode_next(_) ->
    nothing.

process_instruction({ jsr, A }, Symbols) ->
    {[encode_instruction(16#01, A), encode_next(A)], Symbols};
    
process_instruction({ set, B, A }, Symbols) ->
    {[encode_instruction(16#01, A, B), encode_next(A), encode_next(B)], Symbols};

process_instruction({ add, B, A }, Symbols) ->
    {[encode_instruction(16#02, A, B), encode_next(A), encode_next(B)], Symbols};

process_instruction({ sub, B, A }, Symbols) ->
    {[encode_instruction(16#03, A, B), encode_next(A), encode_next(B)], Symbols};

process_instruction({ ifn, B, A }, Symbols) ->
    {[encode_instruction(16#13, A, B), encode_next(A), encode_next(B)], Symbols};

process_instruction(_, _) -> error.


