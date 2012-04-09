What is this?
=============

This is a go at implementing the DCPU-16 processor in the upcoming game [0x10<sup>c</sup>](http://0x10c.com/).
Everyone seems to be doing one, so I thought I'd have a go, and just to make it more interesting I decided to try it in a language I'm not particularly familiar with Erlang.
Perhaps when I've finished this one I might try implementing it in lisp ;)

Compiling
---------

    make

Running
-------

    erl -pa ebin
    application:start(dcpu16).
    dcpu16_server:test_call("Call").
    dcpu16_server:test_cast("Cast").
