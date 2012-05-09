SOURCES=$(wildcard *.erl)
OBJECTS=$(SOURCES:%.erl=../ebin/%.beam)

all: ../ebin $(OBJECTS)

../ebin:
	mkdir ../ebin

../ebin/%.beam: %.erl
	erlc -o ../ebin $<
