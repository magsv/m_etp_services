ERL ?= erl
DEPS_DIR:=/media/magnus/hdd_1/projects/erlang/deps

.PHONY: deps

all: deps
	@./rebar compile

eunit:
	@./rebar skip_deps=true eunit

test: eunit ct

app:
	@./rebar compile skip_deps=true

deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

erlconnect:
	@erl -sname remotetest -remsh 'magnus@magnus-desktop'




m_etp_server: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(DEPS_DIR)/*/ebin -boot start_sasl  -s crypto -s lager  -s m_etp_server -s m_etp_store -s m_etp_avro -sname m_etp 





	
	
 
	
