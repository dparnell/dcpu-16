What is this?
=============

This is a simple template for an OTP application.  It is based on one found [here](https://github.com/zpeters/ErlangOtpTemplate) and some information on [StackOverflow](http://stackoverflow.com/questions/1582818/what-tool-do-you-use-to-build-an-erlang-program).

Compiling
---------

    make

Running
-------

    erl -pa ebin
    application:start(myapp).
    myapp_server:test_call("Call").
    myapp_server:test_cast("Cast").


