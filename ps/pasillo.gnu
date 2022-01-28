set terminal wxt
stats "opasillo.dat" nooutput
blocks = STATS_blocks
set size ratio -1
unset key
plot [-25:25][-10:10] "pasillo.dat" using 1:($3<0?$2:1/0):(abs($3)) index 0 with points pt 7 ps variable lc "#ff6961", \
"pasillo.dat" using 1:($3>0?$2:1/0):(abs($3)) index 0 with points pt 7 ps variable lc "#5086c1", \
for [i=0:blocks-1] "opasillo.dat" w l lc "black" noti
#set output "h1000.png"
#plot [-25:25][-10:10] "pasillo.dat" using 1:($3<0?$2:1/0):(abs($3)) index 1 with points pt 7 ps variable lc "#ff6961", \
#"pasillo.dat" using 1:($3>0?$2:1/0):(abs($3)) index 1 with points pt 7 ps variable  lc "#5086c1"
pause mouse key
