#!/bin/bash

gio trash files/test1.pdf
gio trash files/test2.pdf
gio trash files/test3.pdf
gio trash files/test_chroma.pdf

cabal run pdf-converter -- files/test1.md files/test2.md files/test3.md files/test_chroma.md
