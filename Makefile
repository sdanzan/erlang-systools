all: main test

main:
	@$(MAKE) --directory=src

.PHONY: test
test:
	@$(MAKE) --directory=test

clean:
	rm -f ebin/*.beam
