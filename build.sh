#!/bin/bash

rebar3 compile

cd _build/default/lib/lets && make
