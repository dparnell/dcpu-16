all: test

compile:
	erlc -o ebin +debug_info erl_make.erl

dist: compile
	erl -pa ./ebin -eval "case erl_make:make(development) of ok -> halt(0); _ -> halt(1) end." -s init stop -noshell

test: compile
	erl -pa ./ebin -eval "case erl_make:make(test) of ok -> halt(0); _ -> halt(1) end." -s init stop -noshell

clean:
	rm ebin/*.beam

