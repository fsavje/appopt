Rscript genalldata.R

./runwlimit.sh aoD2AG.R td-10M-5.RData > result/aoD2AG-10M-5
./runwlimit.sh aoD2AG.R td-10M-10.RData > result/aoD2AG-10M-10
./runwlimit.sh aoD2AG.R td-100M-5.RData > result/aoD2AG-100M-5
./runwlimit.sh aoD2AG.R td-100M-10.RData > result/aoD2AG-100M-10

./runwlimit.sh aoD2SG.R td-10M-5.RData > result/aoD2SG-10M-5
./runwlimit.sh aoD2SG.R td-10M-10.RData > result/aoD2SG-10M-10
./runwlimit.sh aoD2SG.R td-100M-5.RData > result/aoD2SG-100M-5
./runwlimit.sh aoD2SG.R td-100M-10.RData > result/aoD2SG-100M-10

./runwlimit.sh aoDLAV.R td-10M-5.RData > result/aoDLAV-10M-5
./runwlimit.sh aoDLAV.R td-10M-10.RData > result/aoDLAV-10M-10
./runwlimit.sh aoDLAV.R td-100M-5.RData > result/aoDLAV-100M-5
./runwlimit.sh aoDLAV.R td-100M-10.RData > result/aoDLAV-100M-10

./runwlimit.sh aoULAV.R td-10M-5.RData > result/aoULAV-10M-5
./runwlimit.sh aoULAV.R td-10M-10.RData > result/aoULAV-10M-10
./runwlimit.sh aoULAV.R td-100M-5.RData > result/aoULAV-100M-5
./runwlimit.sh aoULAV.R td-100M-10.RData > result/aoULAV-100M-10

./runwlimit.sh greedy.R td-50k-2.RData > result/greedy-50k-2
./runwlimit.sh greedy.R td-50k-5.RData > result/greedy-50k-5
./runwlimit.sh greedy.R td-50k-10.RData > result/greedy-50k-10
./runwlimit.sh greedy.R td-100k-2.RData > result/greedy-100k-2
./runwlimit.sh greedy.R td-100k-5.RData > result/greedy-100k-5
./runwlimit.sh greedy.R td-100k-10.RData > result/greedy-100k-10

./runwlimit.sh moore.R td-50k-2.RData > result/moore-50k-2
./runwlimit.sh moore.R td-50k-5.RData > result/moore-50k-5
./runwlimit.sh moore.R td-50k-10.RData > result/moore-50k-10
./runwlimit.sh moore.R td-100k-2.RData > result/moore-100k-2
./runwlimit.sh moore.R td-100k-5.RData > result/moore-100k-5
./runwlimit.sh moore.R td-100k-10.RData > result/moore-100k-10

./runwlimit.sh nbpm.R td-50k-2.RData > result/nbpm-50k-2
./runwlimit.sh nbpm.R td-50k-5.RData > result/nbpm-50k-5
./runwlimit.sh nbpm.R td-50k-10.RData > result/nbpm-50k-10
./runwlimit.sh nbpm.R td-100k-2.RData > result/nbpm-100k-2
./runwlimit.sh nbpm.R td-100k-5.RData > result/nbpm-100k-5
./runwlimit.sh nbpm.R td-100k-10.RData > result/nbpm-100k-10

echo "All done."
