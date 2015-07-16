# Installation Caveats

Ensure that:

* The homebrew `nlopt` package _is_not_ installed. It screws up `nloptr` installation.
* There are no `~/.R/Makevars` defined.

Then packrat should take care of installing necessary R packages.