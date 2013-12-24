#!/usr/bin/env sh
APP_NAME =push_service 
WD=`pwd | sed 's/\//\\\\\//g'`

help:
	@printf "\
	 REBAR: \n\
	dep		- install dependencies \n\
	clean		- clean compiled project and dependencies \n\
	compile		- compile project and dependencies \n\
	generate	- generate reltool release \n\
	\n UTILITY \n\
	config		- edit front.config setting php path from current directory\n\
	rebuild		- clean and compile project\n\
	regenerate	- rebuild and generate releae directory\n\
	reset		- regenerate and restart applicaion (uses softstop)\n\
	\n APPLICATION \n\
	start		- start an applicaion\n\
	stop		- stop an applicaion\n\
	restart		- restart an applicaion\n\
	softstop	- stop applicaion without error if not running\n\
	console		- start applicaion in console mode\n\
	attach		- attach to running applicaionnode\n\
	ping		- ping working applicaion\n\
	status		- show workers status\n\
	\n DIALYZER \n\
	build_plt	- create plt file \n\
	analyze		- analyse project with generated plt file\n\
	"


status:
	escript apps/front/src/show_status.escript

ulimit:
	sh -c 'ulimit -Sd 200000'

redep:
	rm -rf deps
	./rebar get-deps

dep:
	./rebar get-deps

reset: rebuild 

hard_reset: reset restart

rebuild: clean compile

clean:
	./rebar clean
compile:
	./rebar compile

console: ulimit
	./rel/$(APP_NAME)/bin/$(APP_NAME) console
start: ulimit
	./rel/$(APP_NAME)/bin/$(APP_NAME) start
restart: softstop start
softstop:
	make stop ; true
stop:
	./rel/$(APP_NAME)/bin/$(APP_NAME) stop
ping:
	./rel/$(APP_NAME)/bin/$(APP_NAME) ping
foreground: ulimit
	./rel/$(APP_NAME)/bin/$(APP_NAME) foreground
attach:
	./rel/$(APP_NAME)/bin/$(APP_NAME) attach


build_plt:
	dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc -r deps 	
analyze: compile
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r apps/$(APP_NAME)/ --src -I deps
