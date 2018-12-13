#!/bin/zsh
./photoshop lena.ppm lenaVertical.ppm -fv
./photoshop lena.ppm lenaHorizontalVertical.ppm -fv -fh
./photoshop lena.ppm lenaHorizontal.ppm -fh
./photoshop lena.ppm lenaCinza.ppm -gs
./photoshop lena.ppm lenaVermelho.ppm -rc
./photoshop lena.ppm lenaAzul.ppm -bc
./photoshop lena.ppm lenaVerde.ppm -gc
./photoshop lena.ppm lenaPDF.ppm -fh -fv -hw -hh -gs
