#! /bin/sh


# SU must be installed with the XDR flag set
inputdata=3stomp_test.segd
outfile=temp.su

segdread tape=$inputdata verbose=1 ptmax=2 buff=0 aux=1 use_stdio=1 |
suoldtonew > $outfile


surange < $outfile

suxwigb < $outfile title="3 Stomp test (segd)" &

supswigb < $outfile title="3 Stomp test (segd)" > 3stomp_test.eps



# Sercel test
# note, there appear to be NANs on the output, but the result of
# filtering through "sunan" seems to be acceptable. I do not know
# if the nans are in the data.
inputdata=sercel.segd
outfile=sercel_temp.su

segdread tape=$inputdata verbose=1 ptmax=2 buff=0 aux=1 use_stdio=1 |
suoldtonew | sunan verbose=0 >  $outfile

surange < $outfile

suxwigb < $outfile title="Sercel Test" perc=95  &

supswigb < $outfile title="Sercel Test" perc=95 > sercel_test.eps

exit 0
