# Gnuplot script generated by the BioPEPA Workbench

set terminal png
set output "provaSundials001_stochkit_results_0.png"
set xlabel "Time "
set ylabel "Number"
set key bmargin left horizontal box

plot     "provaSundials001_stochkit_results_0.dat" using 1:2   title "S",     "provaSundials001_stochkit_results_0.dat" using 1:3   title "I",     "provaSundials001_stochkit_results_0.dat" using 1:4   title "R",     "provaSundials001_stochkit_results_0.dat" using 1:5   title "D"
