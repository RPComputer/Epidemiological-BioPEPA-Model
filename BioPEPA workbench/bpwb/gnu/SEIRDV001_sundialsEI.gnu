# Gnuplot script generated by the BioPEPA Workbench

set terminal png
set output "SEIRDV001_sundialsEI.png"
set xlabel "Time "
set ylabel "Number"
set key bmargin left horizontal box

plot     "../dat/SEIRDV001_sundials_results_0.dat" using 1:3   title "E",     "../dat/SEIRDV001_sundials_results_0.dat" using 1:4   title "I"