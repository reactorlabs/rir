#! /bin/bash

echo
echo Running before_install-linux.sh...
echo

# fix for strange automake error on travis
cd external/libjit
for i in jitdynamic/Makefile.am jitplus/Makefile.am jit/Makefile.am; do
  grep pkgconfigdir $i
  if [[ $? == 1 ]]; then
    echo 'pkgconfigdir = .' >> "$i"
  fi
done
cd ../..

sudo apt-get update
sudo apt-get install -y gfortran texi2html
