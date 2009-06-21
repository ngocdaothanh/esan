compile:
	erl -make all

test: compile
	erl -pa ebin -noshell -s esan_tests test -s init stop

clean:
	rm -rf ebin/*.beam
