#!/usr/bin/env sh
APP_NAME=push_service
WD=`pwd | sed 's/\//\\\\\//g'`

	 
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

rebuild: clean compile

clean:
	./rebar clean
compile:
	./rebar compile

console:
	erl -pa ebin -pa deps/*/ebin

build_plt:
	dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc -r deps 	
analyze: compile
	ERL_LIBS=$(PWD)/deps dialyzer --plt $(APP_NAME).plt -r src --src -I deps -I include
