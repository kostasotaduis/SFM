set terminal pngcairo
set output
set xrange [0:7]
set yrange [0:7]
set xlabel "{/Symbol F}_{izq} (pers.·m^{-1}·s^{-1})"
set xlabel "{/Symbol F}_{der} (pers.·m^{-1}·s^{-1})"
plot "1p14m.dat" u 5:6
