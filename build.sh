#!/bin/bash

gio trash files/test1.pdf
gio trash files/test2.pdf

cabal run pdf-converter -- files/test1.md files/test2.md