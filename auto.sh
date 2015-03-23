#!/bin/bash

### Create arrays of the input parameters ###
ix=()
ax=()
ay=()
bx=()
by=()
np1=()
np2=()
mu1=()
mu2=()

n=3

for i in $(seq 2 $n)
do  
  ix[$i]=10
  ax[$i]=60
  ay[$i]=2
  bx[$i]=2
  by[$i]=$i
done

#echo ${ix[@]}
#echo ${#ax[@]}
#echo ${by[@]}


### Write to the runhydro.h file ###
infile=runhydro.h

for i in $(seq 2 $n)
do

  sed -i -e '15d' $infile
  sed -i '' "15i\        real, parameter :: ix=${ix[$i]}" $infile

#  sed -i -e '16d' $infile
#  sed -i "16i\        real, parameter :: ax=${ax[$i]}" $infile

#  sed -i -e '17d' $infile
#  sed -i "17i\        real, parameter :: ay=${ay[$i]}" $infile

#  sed -i -e '19d' $infile
#  sed -i "19i\        real, parameter :: bx=${bx[$i]}" $infile

#  sed -i -e '20d' $infile
#  sed -i "20i\        real, parameter :: by=${by[$i]}" $infile

  cp "$infile" "$infile.$i"
  echo "File $infile.$i created" 
#  make cl
#  make
#  ./scf
 
done


