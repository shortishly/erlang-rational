REBAR = rebar
DIALYZER = dialyzer
RM = rm

.PHONY: all clean deps compile test ct build-plt dialyze update

all:	clean compile test ct

clean:
	@$(REBAR) skip_deps=true clean

squeaky: clean
	@$(REBAR) delete-deps

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

test:
	@$(REBAR) skip_deps=true eunit

ct:
	@$(REBAR) skip_deps=true ct

update:
	@$(REBAR) update-deps


build-plt:
	@$(DIALYZER) --build_plt --output_plt .dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@$(DIALYZER) `find ebin -name "*.beam"|grep -v _test` --plt .dialyzer.plt -Werror_handling \
		-Wrace_conditions -Wunmatched_returns -Wunderspecs -Wno_behaviours
