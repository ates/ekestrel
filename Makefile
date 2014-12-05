PROJECT = ekestrel

REBAR = $(shell which rebar 2>/dev/null || echo $(PWD)/rebar)
LIBS = ERL_LIBS=deps

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

run:
	@$(REBAR) compile skip_deps=true
	@$(LIBS) erl -pa ebin -config $(PROJECT) -s $(PROJECT)

clean:
	@$(REBAR) clean

test:
	@$(REBAR) xref ct skip_deps=true

.PHONY: deps test
