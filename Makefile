.PHONY:compile test clean

REBAR = ./rebar

all:compile test

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit
