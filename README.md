What is this?
=============

This is a go at implementing the DCPU-16 processor in the upcoming game [0x10<sup>c</sup>](http://0x10c.com/).
Everyone seems to be doing one, so I thought I'd have a go, and just to make it more interesting I decided to try it in a language I'm not particularly familiar with Erlang.
Perhaps when I've finished this one I might try implementing it in lisp ;)

The main focus of this implementation is correctness.  I am attempting to make the CPU core execute instructions with the correct timings as defined in the DCPU16 specification in the docs folder.
The way I am hoping to achieve this is by breaking the individual instructions down into micro-operations.  A micro-operation takes either 0 or 1 machine cycles.
Any one call the cycle function will execute at most a single micro-operation with a cost of 1 and as many as are available with cost of 0.  
Thus to achieve the correct instruction timings all I need to so is break the DCPU16 instructions down into the correct micro-operations in the correct order.
This also has the advantage that it is easier to test as all I need to do is make sure I exercise each of the top level instructions (ADD, SUB, MUL and so on) and each of the micro-operations.

Compiling
---------

    make

Running
-------

    erl -pa ebin
    application:start(dcpu16).
    dcpu16_server:test_call("Call").
    dcpu16_server:test_cast("Cast").
