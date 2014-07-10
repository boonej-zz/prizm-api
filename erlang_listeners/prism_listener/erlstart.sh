#!/bin/sh
erl -pa deps/*/ebin apps/*/ebin -boot start_sasl
