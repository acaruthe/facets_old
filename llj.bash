#!/bin/bash 

######
#
## This script will loop thru every month (A M J J A) and calculate llj things!
#
##############################

# first we need to link the month to the current directory from /mnt/nrel/acaruthe/regcm/ and name it infile.nc for ATM and sfcfile.nc for srf

   month=04

 for year in {1989..2010}; do 

ln -sf /mnt/nrel/acaruthe/regcm/lake_001_skt_ATM.${year}${month}0100.nc infile.nc

ln -sf /mnt/nrel/acaruthe/regcm/lake_001_skt_SRF.${year}${month}0100.nc sfcinfile.nc

ncl year=${year} lljcat.ncl

#ncl year=${year} figs.ncl 

#ncl year=${year} msp.ncl

done
