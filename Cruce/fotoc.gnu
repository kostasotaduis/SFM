set terminal pngcairo size 720, 720
set output "cint.png"
stats "ocruce.dat" nooutput
blocks = STATS_blocks
stats "cint2.dat" nooutput
set size ratio 1
unset xtics
unset ytics
set xrange [-25:25]
set yrange [-25:25]
plot "cint2.dat" using 1:($3 == 1 ? $2 : 1/0) index 100 with points pt 7 ps 2 lc "#ff6961" noti, \
"cint2.dat" using 1:($3 == 2 ? $2 : 1/0) index 100 with points pt 7 ps 2 lc "#84b6f4" noti, \
"cint2.dat" using 1:($3 == 3 ? $2 : 1/0) index 100 with points pt 7 ps 2 lc "#77dd77" noti, \
"cint2.dat" using 1:($3 == 4 ? $2 : 1/0) index 100 with points pt 7 ps 2 lc "#ffda9e" noti, \
for [j=0:blocks-1] "ocruce.dat" w l lc "black" noti
