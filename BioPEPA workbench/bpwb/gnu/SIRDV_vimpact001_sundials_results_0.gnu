# Gnuplot script generated by the BioPEPA Workbench

set terminal png
set output "SIRDV_vimpact001_sundials_results_0.png"
set xlabel "Time "
set ylabel "Number"
set key bmargin left horizontal box

plot     "SIRDV_vimpact001_sundials_results_0.dat" using 1:2   title "S",     "SIRDV_vimpact001_sundials_results_0.dat" using 1:3   title "I",     "SIRDV_vimpact001_sundials_results_0.dat" using 1:4   title "R",     "SIRDV_vimpact001_sundials_results_0.dat" using 1:5   title "D",     "SIRDV_vimpact001_sundials_results_0.dat" using 1:6   title "V",     "SIRDV_vimpact001_sundials_results_0.dat" using 1:7   title "CUMI"