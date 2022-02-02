set terminal pngcairo
set output "fvv2.png"
set xrange[3:5.5]
set xlabel "v_{0} (m/s)"
set ylabel "<F> (N)"
set key left
plot "mp.dat" w linespoints pt 16 lc "#ff6961" ti "normal", \
"mpcirc.dat" w linespoints pt 9 lc "#84b6f4" ti "embudo", \
"mpcol.dat" w linespoints pt 5 lc "#77dd77" ti "columnas"
