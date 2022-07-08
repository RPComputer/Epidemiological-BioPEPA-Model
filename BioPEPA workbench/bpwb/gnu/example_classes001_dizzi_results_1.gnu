# Gnuplot script generated by the BioPEPA Workbench

set terminal png
set output "example_classes001_dizzi_results_1.png"
set xlabel "Time "
set ylabel "Number"
set key bmargin left horizontal box

plot     "example_classes001_dizzi_results_1.dat" using 1:8  with linespoints title "Ry",     "example_classes001_dizzi_results_1.dat" using 1:9  with linespoints title "Rm",     "example_classes001_dizzi_results_1.dat" using 1:10  with linespoints title "Ro",     "example_classes001_dizzi_results_1.dat" using 1:2  with linespoints title "Iy",     "example_classes001_dizzi_results_1.dat" using 1:3  with linespoints title "Im",     "example_classes001_dizzi_results_1.dat" using 1:4  with linespoints title "Io",     "example_classes001_dizzi_results_1.dat" using 1:5  with linespoints title "Sy",     "example_classes001_dizzi_results_1.dat" using 1:6  with linespoints title "Sm",     "example_classes001_dizzi_results_1.dat" using 1:7  with linespoints title "So",     "example_classes001_dizzi_results_1.dat" using 1:11  with linespoints title "D"
