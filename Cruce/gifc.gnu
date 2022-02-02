set terminal gif animate delay 5 size 720, 720
set output "cruce.gif"
stats "ocruce.dat" nooutput
blocks = STATS_blocks
stats "cruce2.dat" nooutput
set size ratio 1
unset xtics
unset ytics
set xrange [-25:25]
set yrange [-25:25]
do for [i=0:int(STATS_blocks)-2] {
plot "cruce2.dat" using 1:($3 == 1 ? $2 : 1/0) index i with points pt 7 ps 2 lc "#ff6961" noti, \
"cruce2.dat" using 1:($3 == 2 ? $2 : 1/0) index i with points pt 7 ps 2 lc "#84b6f4" noti, \
"cruce2.dat" using 1:($3 == 3 ? $2 : 1/0) index i with points pt 7 ps 2 lc "#77dd77" noti, \
"cruce2.dat" using 1:($3 == 4 ? $2 : 1/0) index i with points pt 7 ps 2 lc "#ffda9e" noti, \
for [j=0:blocks-1] "ocruce.dat" w l lc "black" noti
}
