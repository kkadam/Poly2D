#!/bin/bash
### Set important parameters ###
run=s1
readme="Fixed normalization of envelope?, similar to m26"
resolution=HR  
#Options for resolution are LR, HR, SHR
work_dir=/work/kkadam/lonely_runs


### Make the binary ###
make cl
make

if [ $? -eq 0 ]
then  
   echo "========================================================"
   echo " Binary file "scf" successfully compiled!"
   echo "========================================================"
else
   echo "========================================================"
   echo "ERROR: COMPILATION FAILED"
   echo "========================================================"
   exit
fi


### Make run_dir and copy files ###
if [ -d $run ]; then
   rm -r $run
fi

mkdir $run
cp scf $run
cp runhydro.h $run
echo $readme > $run/readme


### Edit batch file ###
sed -i -e '8d' bs
sed -i "8i\#PBS -N $run" bs
sed -i -e '11d' bs
sed -i "11i\\$work_dir\/$run\/scf" bs
cp bs $run


### Move run dir, or not ###
if [ -d $work_dir\/$run ]; then
   echo "========================================================"
   echo "The run \"$run\" is ready in current working directory,"
   echo "because a directory with the same name is present in "
   echo "$work_dir "
   echo "========================================================"
else
   mv $run $work_dir
   echo "============================================================"
   echo "The run \"$run\" is ready in $work_dir!"
   echo "============================================================"
fi

