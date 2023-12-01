#!/usr/bin/env bash

find . -name '*.hs' -exec hindent {} \;

