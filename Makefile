SHELL := /bin/bash
REPORTER = spec
MOCHA_TIMEOUT = 5000
SERVER_IP := $(shell /sbin/ifconfig | grep "inet " | grep -v 127.0.0.1 | cut -d " " -f2)
NODE_ENV  := $(shell if [ "$(SERVER_IP)" = "192.168.0.108" ]; then echo "development"; else echo "local"; fi)

mount-fixtures:
	@mongoimport -d prism_test -c users --file ./test/fixtures/users.json --drop --jsonArray
	@mongoimport -d prism_test -c posts --file ./test/fixtures/posts.json --drop --jsonArray

test: mount-fixtures
	@NODE_ENV=test node_modules/.bin/mocha --reporter $(REPORTER) --timeout 50000 --colors --recursive

test-debug:
	@NODE_ENV=test node_modules/.bin/mocha --reporter $(REPORTER) --timeout 50000 --colors --recursive debug

start:
	@NODE_ENV=$(NODE_ENV) node server.js

start-local:
	@NODE_ENV=local node server.js

.PHONY: test start test-debug mount-fixtures

