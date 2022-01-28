set terminal pngcairo
set output "evtt.png"
set xrange[5:30]
set xlabel "t (s)"
set ylabel "<E_{T}> - <E_{k}> (J)"
plot "medu.dat" u 2:($3-$4) index 0 w l lc "#ff6961" ti "{/Symbol r}_{0} = 0.3" ,\
"medu.dat" u 2:($3-$4) index 3 w l lc "#84b6f4" ti "{/Symbol r}_{0} = 0.6" ,\
"medu.dat" u 2:($3-$4) index 5 w l lc "#77dd77" ti "{/Symbol r}_{0} = 0.8"
