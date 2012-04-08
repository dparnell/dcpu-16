What is this?
=============

This is a go at implementing the DCPU-16 processor in the upcoming game [0x10<sup>c</sup>](http://0x10c.com/).

Compiling
---------

    make

Running
-------

    erl -pa ebin
    application:start(dcpu16).
    dcpu16_server:test_call("Call").
    dcpu16_server:test_cast("Cast").


