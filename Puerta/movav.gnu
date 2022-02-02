
# average over N values
N = 10
array Avg[N]
array X[N]

MovAvg(col) = (Avg[(t-1)%N+1]=column(col), n = t<N ? t : N, t=t+1, (sum [i=1:n] Avg[i])/n)
MovAvgCenterX(col) = (X[(t-1)%N+1]=column(col), n = t<N ? t%2 ? NaN : (t+1)/2 : ((t+1)-N/2)%N+1, n==n ? X[n] : NaN)   # be aware: gnuplot does integer division here

set datafile missing NaN

set xlabel "t (s)"
set ylabel "F (Personas/s)"
set xzeroaxis
stats "puerta.dat" nooutput
plot "puerta.dat" u 1:2 w l noti lc "#ffe4e1", \
 "puerta.dat" u 1:3 w l noti lc "#b2e2f2", \
 t=1 '' u (MovAvgCenterX(1)):(MovAvg(2)) w l lw 2 lc "#c63637" ti "Puerta A", \
 t=1 '' u (MovAvgCenterX(1)):(MovAvg(3)) w l lw 2 lc "#5086c1" ti "Puerta B"

pause mouse key