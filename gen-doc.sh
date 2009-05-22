#!/bin/sh

scm/docgen.scm doc/toplevel
for mod in \
    cmdopts collections console extref gcollect \
    inet process regex sxml threads xml; do
    scm/docgen.scm --default-all --module $mod doc/$mod
done
doxygen Doxyfile

