#!/bin/sh

# stack run

# stack build
hlint src/Main.hs
stack install
# stack run

exit 0
