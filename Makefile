all:
	@$(MAKE) --directory=src
	@$(MAKE) --directory=test

clean:
	rm -f ebin/*.beam
