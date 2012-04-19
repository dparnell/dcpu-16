all: test

compile:
	erlc -o ebin +debug_info erl_make.erl

test: compile
	erl -pa ./ebin -eval "erl_make:make(development)" -s init stop -noshell

clean:
	rm ebin/*.beam

