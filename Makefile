ERL ?= erl
APP := constrictor

.PHONY: deps

REBAR ?= rebar

all: deps 
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps
	@rm $(DEPS_PLT)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

app:
	@$(REBAR) compile skip_deps=true

test:
	@$(REBAR) skip_deps=true eunit

#################
# Analysis

DIALYZER_OPTS = -Wrace_conditions -Werror_handling -Wunderspecs
DEPS_PLT=$(PWD)/.appendb_deps_plt
ERLANG_DIALYZER_APPS = asn1 \
                       compiler \
                       crypto \
                       edoc \
                       edoc \
                       erts \
                       eunit \
                       eunit \
                       gs \
                       hipe \
                       inets \
                       kernel \
                       mnesia \
                       mnesia \
                       observer \
                       public_key \
                       runtime_tools \
                       runtime_tools \
                       ssl \
                       stdlib \
                       syntax_tools \
                       syntax_tools \
                       tools \
                       webtool \
                       xmerl

~/.dialyzer_plt:
	@echo "ERROR: Missing ~/.dialyzer_plt. Please wait while a new PLT is compiled"
	@dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS)
	@echo "now try your build again"

$(DEPS_PLT):
	@dialyzer --output_plt $(DEPS_PLT) --build_plt -r deps 

dialyzer: $(DEPS_PLT) ~/.dialyzer_plt
	@dialyzer $(DIALYZER_OPTS) $(DIALYZER_OPTS) --plts ~/.dialyzer_plt $(DEPS_PLT) -I deps --src src

typer: $(DEPS_PLT) ~/.dialyzer_plt
	@typer --plt $(DEPS_PLT) --plt ~/.dialyzer_plt --annotate -I deps/ -r src/
