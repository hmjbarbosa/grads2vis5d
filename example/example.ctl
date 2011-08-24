dset ^example.grib
index ^example.grib.idx
undef 9.999E+20
title example.grib
*  produced by grib2ctl v0.9.12.5p45
dtype grib 255
options yrev
ydef 75 linear -70.500000 1.5
xdef 81 linear 240.000000 1.500000
tdef 4 linear 00Z01jan2011 6hr
*  z has 13 levels, for prs
zdef 13 levels
1000 950 900 850 800 750 700 600 500 400 300 200 100
vars 6
CC 13 248,100,0 ** (profile) Cloud cover [(0 - 1)]
Q 13 133,100,0 ** (profile) Specific humidity [kg kg**-1]
T 13 130,100,0 ** (profile) Temperature [K]
U 13 131,100,0 ** (profile) U velocity [m s**-1]
V 13 132,100,0 ** (profile) V velocity [m s**-1]
W 13 135,100,0 ** (profile) Vertical velocity [Pa s**-1]
ENDVARS
