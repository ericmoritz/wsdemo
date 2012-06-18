#!/usr/bin/env bash
erl -noshell -pa ../ebin ../deps/*/ebin +K true -s wsdemo
