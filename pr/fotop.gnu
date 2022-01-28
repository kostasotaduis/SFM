set terminal png 
set output "puerta.png"
stats "puerta.dat" nooutput
set xrange [-10:10]
set yrange [-10:10]
set arrow from -25, 8 to 25, 8 nohead
set arrow from -25, -8 to 25, -8 nohead
set arrow from 0,8 to 0,1.5 nohead
#set arrow from 0,0.5 to 0,-0.5 nohead
set arrow from 0,-1.5 to 0,-8 nohead
plot "puerta.dat" using 1:($3 < 0 ? $2 : 1/0):(abs($3)+0.5) notitle with points pt 7 ps variable lc rgbcolor "#ff6961", \
"puerta.dat" using 1:($3 > 0 ? $2 : 1/0):(abs($3)+0.5) notitle with points pt 7 ps variable lc rgbcolor "#84b6f4"

