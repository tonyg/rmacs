# rmacs

An EMACS written in Racket. Runs in ANSI-compatible terminals.

# Instructions

rmacs is still very beta.

## Installing using the Racket package system

Run

    raco pkg install rmacs

After that, `rmacs` should be on your path.

## Installing from git

After cloning the repo, install the
[racket-ansi](https://github.com/tonyg/racket-ansi) package:

    raco pkg install ansi

Then, run `make link`, which will install the package from the local
git working tree.

After `make link` has successfully run once, you don't need to do it
again; running plain `make` or `raco setup rmacs` will recompile the
rmacs package in-place.

## License

Copyright (C) 2011, 2013, 2014, 2015 Tony Garnock-Jones <mailto:tonyg@leastfixedpoint.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program (see the files "lgpl.txt" and
"gpl.txt"). If not, see <http://www.gnu.org/licenses/>.
