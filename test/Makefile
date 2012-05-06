SOURCES=$(wildcard *.erl)
OBJECTS=$(SOURCES:%.erl=../ebin/%.beam)

all: $(OBJECTS) test

../ebin/%.beam: %.erl
	erlc -o ../ebin $<

test:
	erl -noshell -pa ../ebin -eval 'eunit:test("../ebin", [verbose])' -s init stop
