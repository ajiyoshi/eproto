
ERLC_FLAGS= -I include
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard include/*.hrl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)

all:$(OBJECTS) test

ebin:
	mkdir ebin

ebin/%.beam : src/%.erl $(HEADERS) Makefile ebin
	erlc $(ERLC_FLAGS) -o ebin/ $<

clean:
	-rm $(OBJECTS) addressbook.bin address.erl
	-rm -r ebin

test:
	erl -noshell -pa ebin \
		-eval 'eunit:test("ebin", [verbose])' \
		-s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"
