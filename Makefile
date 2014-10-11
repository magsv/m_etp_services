ERL ?= erl
DEPS_DIR:=/media/magnus/hdd_1/projects/erlang/deps
MNESIA_DIR:=/media/magnus/hdd_1/projects/erlang/energistics/m_etp_services/mnesia_dir
APP_CNFG:=/media/magnus/hdd_1/projects/erlang/energistics/m_etp_services/apps/m_etp_store/priv/app.config
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
	@erl -sname remotetest -remsh 'm_etp@magnus-desktop'




m_etp_server: app
	exec erl -pa $(PWD)/apps/*/ebin -pa $(DEPS_DIR)/*/ebin -boot start_sasl  -mnesia dir '"$(MNESIA_DIR)"' -config $(APP_CNFG) -s crypto -s lager  -s m_etp_server -s m_etp_store -s m_etp_avro -sname m_etp 





	
	
 
	
