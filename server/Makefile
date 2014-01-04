REBAR = `which rebar`

all: compile

deps:
	@($(REBAR) get-deps)

compile_all: clean_all
	@($(REBAR) compile)

clean_all:
	@($(REBAR) clean)

compile: clean
	@($(REBAR) compile skip_deps=true)

clean:
	@($(REBAR) clean skip_deps=true)

test: compile
	@($(REBAR) ct skip_deps=true)

run:
	@(erl -pa ebin deps/*/ebin -s tri_main start)

dev_run:
	@(erl -pa ebin deps/*/ebin -s tri_main dev_start)

.PHONY: all deps compile_all clean_all compile clean run dev_run test
