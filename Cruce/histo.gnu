set terminal pngcairo
set output "h01.png"


binwidth = 0.2
bin(x, width) = width*floor(x/width)
set boxwidth binwidth
set style fill solid
set xrange [0:30]
set xzeroaxis
set xlabel "t (s)"
set ylabel "p"
stats "tint01d.dat" nooutput
n1 = STATS_records
m1 = STATS_max
stats "trot01d.dat" nooutput
n2 = STATS_records
m2 = STATS_max


plot [0:20][-0.1:0.1] "tint01d.dat" using (bin($1, binwidth)):(1.0/n1) smooth freq with boxes lc "#ff6961" noti, \
"trot01d.dat" using (bin($1, binwidth)):(-1.0/n2) smooth freq with boxes lc "#84b6f4" noti
