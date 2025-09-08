#! /bin/sh

has_doctest() {
    egrep -q '^( *|-- )>>> ' "$1" > /dev/null
}

vecho=:
if [ x"$1" = x-v -o x"$1" = x--verbose ]; then
    vecho=echo
fi

find src -type f -name '*.hs' | while read path ; do
    if has_doctest "$path" ; then
        $vecho "* run doctest for $path"
        doctest -isrc $path
    else
        $vecho "* doctest not found for $path"
    fi
done
