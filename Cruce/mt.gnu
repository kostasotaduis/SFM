set terminal pngcairo
set output "mt.png"

set xlabel "{/Symbol r}_{0} (pers.·m^{-2})"
set ylabel "t (s)"
set key left

plot [0:1][4:25] "mtd.dat" u 1:2 w linespoints pt 7 lc "#ff6961" ti "Normal", \
"mtd.dat" u 1:4 w linespoints pt 7 lc "#84b6f4" ti "Rotonda"
