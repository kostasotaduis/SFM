set terminal pngcairo
set output "fund5m10dcol.png"
set xrange [0:3]
set yrange [0:3]
set xlabel "{/Symbol r} (pers·m^{-2})"
set ylabel "{/Symbol F} (pers·m^{-1}s^{-1})"
plot "fund5m10dcol.dat" u 1:3 pt 7 lc "#ff6961" ti "{/Symbol F}_{izq}",\
"fund5m10dcol.dat" u 2:4 pt 7 lc "#77dd77" ti "{/Symbol F}_{der}", \
1.34*x dt 3 lc "black" ti "v_{0}·{/Symbol r}"
