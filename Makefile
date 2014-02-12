SHELL						:= /bin/bash
REPORTER				= spec
MOCHA_TIMEOUT		= 5000
PRISM_HOME			= $(shell PWD)
SERVER_IP				:= $(shell /sbin/ifconfig | grep "inet " | grep -v 127.0.0.1 | cut -d " " -f2)
NODE_ENV				:= $(shell if [ "$(SERVER_IP)" = "192.168.0.108" ]; then echo "development"; else echo "local"; fi)

test:
	@PRISM_HOME=$(PWD)/ \
		NODE_ENV=test \
			./node_modules/.bin/mocha \
			--reporter $(REPORTER) \
			--timeout $(MOCHA_TIMEOUT) \
			--colors \
			--recursive

start:
	@PRISM_HOME=$(PRISM_HOME)/ NODE_ENV=$(NODE_ENV) node server.js

start-local:
	@PRISM_HOME=$(PRISM_HOME)/ NODE_ENV=local node server.js

.PHONY: test start start-local
