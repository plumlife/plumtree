.PHONY: deps compile rel

DIALYZER_APPS = kernel stdlib erts sasl eunit syntax_tools compiler crypto
REBAR="rebar3"
DEP_DIR="_build/default/lib"

all: compile

include tools.mk

test: common_test

common_test:
	rebar3 ct

compile: deps
	./build.sh

rel:
	rebar3 release

stage:
	rebar3 release -d

