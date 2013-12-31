REBAR = `which rebar`

all: deps compile

deps:
	@($(REBAR) get-deps)

compile: clean
	@($(REBAR) compile)

clean:
	@($(REBAR) clean)

test:
	@($(REBAR) ct skip_deps=true)

run:
	@(erl -pa ebin deps/*/ebin -s tri_main start)

dev_run:
	@(erl -pa ebin deps/*/ebin -s tri_main dev_start)

.PHONY: all deps compile clean run dev_run test
