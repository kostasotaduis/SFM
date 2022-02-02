program prueba

real :: t, t0

open(unit=0, file="tiempo.dat", status="replace", action="write")

call cpu_time(t0)



do
	call cpu_time(t)
	if ((t - t0) > 5.0) then
		write(unit=0, fmt="(a)") "pasaron 5 segs"
		exit
	end if
end do
do
	call cpu_time(t)
	if ((t - t0) > 100.0) then
		write(unit=0, fmt="(a)") "pasaron 100 min"
		exit
	end if
end do
close(unit=0)






end program prueba
