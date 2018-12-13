#!/bin/zsh
./photocop lena.ppm lenaVertical.ppm -fv
./photocop lena.ppm lenaHorizontalVertical.ppm -fv -fh
./photocop lena.ppm lenaHorizontal.ppm -fh
./photocop lena.ppm lenaCinza.ppm -gs
./photocop lena.ppm lenaVermelho.ppm -rc
./photocop lena.ppm lenaAzul.ppm -bc
./photocop lena.ppm lenaVerde.ppm -gc
./photocop lena.ppm lenaPDF.ppm -fh -fv -hw -hh -gs
