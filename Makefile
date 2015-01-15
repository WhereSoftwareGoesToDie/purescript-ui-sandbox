SHELL := /bin/bash --init-file ./make-scripts/makefile-functions.sh

sources    = $(wildcard src/*.purs)
objects    = $(patsubst src/%.purs, build/scripts/%.js, $(sources))
components = $(shell find bower_components -type f \( -name '*.purs' -and \! -path '*example*' \))
react      = $(shell find bower_components -type f \( -name 'react.min.js' \))

.PHONY: all clean build build/scripts run bower_components

.SUFFIXES: .js .purs

all: clean $(objects) build/lib/react.js

clean:
	@rm -rf build

bower_components:
	@bower install

build:
	cp -r static build

build/scripts: build bower_components
	@mkdir -p build/scripts

build/lib: build bower_components
	@mkdir -p build/lib

build/lib/react.js: build/lib
	@cp bower_components/react/react.min.js build/lib/react.js

build/scripts/%.js: module = $(notdir $(basename $<))
build/scripts/%.js: src/%.purs build/scripts
	@psc --module=$(module) --main=$(module) --output=$@ $(components) $<

run: all
	@cd build && python -m SimpleHTTPServer
