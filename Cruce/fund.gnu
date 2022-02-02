set terminal pngcairo
set output "fundcruce075.png"

set xlabel "{/Symbol r} (pers.·m^{-2})"
set ylabel "{/Symbol F} (pers.·m^{-1}s^{-1})"


set key left

#plot "cint02d.dat" u 2:3 w p pt 7 lc "#ff6961" ti "Normal", \
#"crot02d.dat" u 2:3 w p pt 7 lc "#84b6f4" ti "Rotonda", \
#1.34 * x w l dt 3 lc "black" ti "{/Symbol r}·v_{0}"



plot [0:2][0:1.5] "cint075d.dat" u 2:3 w p pt 7 lc "#ff6961" ti "Normal", \
"crot075d.dat" u 2:3 w p pt 7 lc "#84b6f4" ti "Rotonda", \
1.34 * x w l dt 3 lc "black" ti "{/Symbol r}·v_{0}"
