# Gnuplot script generated by the BioPEPA Workbench

set terminal png
set output "SIRDV001_stochkit_results_0.png"
set xlabel "Time "
set ylabel "Number"
set key bmargin left horizontal box

plot     "SIRDV001_stochkit_results_0.dat" using 1:2   title "S",     "SIRDV001_stochkit_results_0.dat" using 1:3   title "I",     "SIRDV001_stochkit_results_0.dat" using 1:4   title "R",     "SIRDV001_stochkit_results_0.dat" using 1:5   title "D",     "SIRDV001_stochkit_results_0.dat" using 1:6   title "V",     "SIRDV001_stochkit_results_0.dat" using 1:7   title "CUMI"
