set terminal gif animate delay 5 size 1080, 720
set output "puerta.gif"
stats "opuerta.dat" nooutput
blocks = STATS_blocks
stats "puertap2.dat" nooutput
blocks2 = STATS_blocks
stats "puertap2.dat" u 6 nooutput
maxc = STATS_max
rgb(r,g,b) = 65536*int(r) + 256*int(g) + int(b)
set size ratio 0.75
set xrange [-15:11]
set yrange [-10:10]
set arrow from -15, 7.5 to -15, -7.5 nohead
unset xtics
unset ytics
set style fill solid noborder
do for [i=0:blocks2-2] {
plot "puertap2.dat" using 1:2:5:(rgb((255-132)*($6/maxc)+132,(105-182)* \
($6/maxc)+182,(97-244)*($6/maxc)+244)) \
index i with circles lc rgb variable ti sprintf("%i", i), \
for [j=0:blocks-1] "opuerta.dat" w l lc "black" lw 2 noti
}
#do for [i=blocks2-200:blocks2-2] {
#do for [i=0:200] {
