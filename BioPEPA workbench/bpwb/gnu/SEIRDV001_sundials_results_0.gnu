# Gnuplot script generated by the BioPEPA Workbench

set terminal png
set output "SEIRDV001_sundials_results_0.png"
set xlabel "Time "
set ylabel "Number"
set key bmargin left horizontal box

plot     "SEIRDV001_sundials_results_0.dat" using 1:2   title "S",     "SEIRDV001_sundials_results_0.dat" using 1:3   title "E",     "SEIRDV001_sundials_results_0.dat" using 1:4   title "I",     "SEIRDV001_sundials_results_0.dat" using 1:5   title "R",     "SEIRDV001_sundials_results_0.dat" using 1:6   title "D",     "SEIRDV001_sundials_results_0.dat" using 1:7   title "V"
