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
	./rebar3 do xref, eunit, cover, covertool generate
	mv _build/test/covertool/snatch.covertool.xml cobertura.xml

shell:
	./rebar3 shell

.PHONY: doc test compile all shell
