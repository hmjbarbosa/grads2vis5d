#
# To convert Era Interin grib to binary it is NOT enough to use 
#
#   wgrib -d all -o [output file] -bin -nh [input grib]
#
# because then the order inside the binary file will not be what grads
# expects. The grib is organized as (X) (Y) (V) (Z) (T), while grads
# hopes for (X) (Y) (Z) (V) (T). In version 1.9 or EARLIER, there was
# an option -1,10,1 one could use in the variable definition to
# reverse (Z,V), but that is not available anymore! WTF?!?
#
# The way out is to reorganize the decoding of the grib file in the
# exact order that we nedd. We can do this usint 'sort' command and a
# few tricks.
#
# == TRICKS ==
#
# write the inventory list to stdout
#
#    wgrib example.grib |\ 
#
# sort using coluns 3,4 and 7
# for col7, start at pos=7 and use reverse numerical order
#
#    sort -t : -k 3,4 -k 7.7,7nr |\ 
#
# cat the SORTED list through stdin to the decoder
#
#    wgrib -i -o example.bin -bin -nh example.grib

wgrib example.grib |\
sort -t : -k 3,4 -k 7.7,7nr|\
wgrib -i -o example.bin -bin -nh example.grib

# process the ctl

cat example.ctl |\
grep -v index | grep -v dtype | sed s/.grib/.bin/ > example.bin.ctl

# end