ERL ?= erl
DEPS_DIR:=$(PWD)/deps
MNESIA_DIR:=$(PWD)/mnesia_dir
APP_CNFG:=/$(PWD)/apps/m_etp_store/priv/app.config
OPTIMIZED_SCHEMAS_DIR:=$(PWD)/etp_schemas/m_etp_schemas_optimized
SERVER_NAME:=http://localhost:8080
AVRO_WILD_CARD:=$(wildcard $(OPTIMIZED_SCHEMAS_DIR:%=%/*.avsc))

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

load_protocols: 
	for file in $(AVRO_WILD_CARD); \
	do \
		curl -X POST -d @$$file -H "Content-type:application/json" $(SERVER_NAME)/m_etp_protocol_service;\
	done

docker_build_image:
	sudo docker build -t m_etp_server ./docker/.

docker_list_images:
	sudo docker images 

docker_run_bash:
	sudo docker run -t -i m_etp_server /bin/bash







	
	
 
	
