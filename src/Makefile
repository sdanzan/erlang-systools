all: $(notdir $(patsubst %.erl, %.beam, $(wildcard *.erl)))
	if [ ! -d ../ebin ]; then mkdir ../ebin; fi
	mv *.beam ../ebin

%.beam: %.erl
	erlc $<
