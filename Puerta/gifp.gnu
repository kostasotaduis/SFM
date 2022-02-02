set terminal gif animate delay 10 size 1080, 720
set output "puerta.gif"
stats "opuerta.dat" nooutput
blocks = STATS_blocks
stats "puerta2.dat" nooutput
set xrange [-10:10]
set yrange [-10:10]
do for [i=0:int(STATS_blocks)-2] {
plot "puerta2.dat" using 1:2 index i with points pt 7 ps 2 lc rgbcolor "#ff6961", \
"puerta2.dat" using 1:($3 > 0 ? $2 : 1/0) index i with points pt 7 ps 2 lc rgbcolor "#84b6f4", \
for [j=0:blocks-1] "opuerta.dat" w l lc "black" noti
}
