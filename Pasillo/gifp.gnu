set terminal gif animate delay 10 size 1080, 720
set output "pasillo.gif"
stats "opasillo.dat" nooutput
blocks = STATS_blocks
stats "pasillo2.dat" nooutput
set size ratio 1
set xrange [-10:10]
set yrange [-10:10]
unset xtics
unset ytics
do for [i=0:int(STATS_blocks)-2] {
plot "pasillo2.dat" using 1:($3 < 0 ? $2 : 1/0):(0.5+abs($3)) index i with points pt 7 ps variable lc "#ff6961" noti, \
"pasillo2.dat" using 1:($3 > 0 ? $2 : 1/0):(0.5+abs($3)) index i with points pt 7 ps variable lc "#84b6f4" noti,\
for [j=0:blocks-1] "opasillo.dat" w l lc "black" noti
}
