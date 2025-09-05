#! /bin/sh

find src -type f | \
    xargs -n 1 doctest
