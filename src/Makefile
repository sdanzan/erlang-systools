SOURCES=$(wildcard *.erl)
OBJECTS=$(SOURCES:%.erl=../ebin/%.beam)

all: $(OBJECTS)

../ebin/%.beam: %.erl
	erlc -o ../ebin $<
