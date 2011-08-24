#
# To convert Era Interin grib to binary it is NOT enough to use 
#
#   wgrib -d all -o [output file] -bin -nh [input grib]
#
# because then the order inside the binary file will not be what
# grads expects. The grib is organized as X Y V Z T, while grads
# hopes for X Y Z V T. In version 1.9 or EARLIER, there was an
# option -1,10,1 one could use in the variable definition to 
# reverse Z,V, but that is not available anymore! WTF?!?
#
# The way out is to reorganize the decode the grib file in the order
# that we want. We can do this usint 'sort' command and a few
# tricks.
#
# Inside grib, the important coluns are:
#
#  3= time
#  4= variable name
#  7= level 
#
# Hence a command like
#
#  sort -t : -k 3,4 -k 7,7
#
# should do the trick, except that in the 7th column we have 
# levels that are not zero padded. Therefore, sort result as:
#
#  level 100, 1000, 125, 150, ..., 975
#  
# this can be circumvented by

# write the inventory list
#    wgrib interim.grib |\ 
# sort using coluns 3,4 and 7
# for col7, start at pos=7 to the end of line
# and use reverse numerical order
#    sort -t : -k 3,4 -k 7.7,7nr |\ 
# finally pass the SORTED inventory list to the decoder
#    wgrib -i -o interim.bin -bin -nh interim.grib

wgrib interim.grib |\
sort -t : -k 3,4 -k 7.7,7nr|\
wgrib -i -o interim.bin -bin -nh interim.grib

# end