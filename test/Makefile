SOURCES=$(wildcard *.erl)
OBJECTS=$(SOURCES:%.erl=../ebin/%.beam)

all: ../ebin $(OBJECTS) test

../ebin:
	mkdir ../ebin

../ebin/%.beam: %.erl
	erlc -o ../ebin $<

test:
	erl +K true -noshell -pa ../ebin -eval 'eunit:test("../ebin")' -s init stop

test-verbose:
	erl +K true -noshell -pa ../ebin -eval 'eunit:test("../ebin", [verbose])' -s init stop
