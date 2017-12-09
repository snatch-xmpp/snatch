all: compile

doc:
	./rebar3 as doc edown

clean-devel: clean
	-rm -rf _build

clean:
	./rebar3 clean

compile:
	./rebar3 compile

test:
	-epmd -daemon
	./rebar3 do xref, eunit, cover
	./covertool \
		-cover _build/test/cover/eunit.coverdata \
		-appname snatch \
		-output cobertura.xml > /dev/null

shell:
	./rebar3 shell

.PHONY: doc test compile all shell
