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

encode_write(a) -> 0;
encode_write(b) -> 1;
encode_write(c) -> 2;
encode_write(x) -> 3;
encode_write(y) -> 4;
encode_write(z) -> 5;
encode_write(i) -> 6;
encode_write(j) -> 7;
encode_write([a]) -> 8;
encode_write([b]) -> 9;
encode_write([c]) -> 10;
encode_write([x]) -> 11;
encode_write([y]) -> 12;
encode_write([z]) -> 13;
encode_write([i]) -> 14;
encode_write([j]) -> 15;
encode_write([a, _]) -> 16;
encode_write([b, _]) -> 17;
encode_write([c, _]) -> 18;
encode_write([x, _]) -> 19;
encode_write([y, _]) -> 20;
encode_write([z, _]) -> 21;
encode_write([i, _]) -> 22;
encode_write([j, _]) -> 23;
encode_write(pop) -> 24;
encode_write([sp]) -> 25;
encode_write(peek) -> 25;
encode_write([sp, _]) -> 26;
encode_write(sp) -> 27;
encode_write(pc) -> 28;
encode_write(ex) -> 29;
encode_write([A]) when is_integer(A) -> 30;
encode_write(A) when A < -1 -> 31;
encode_write(A) when A > 30 -> 31;
encode_write(_) -> error.

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


