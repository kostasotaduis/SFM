program opasillo

real :: xmin, xmax, ymin, ymax, x, y
integer :: i, n

open (unit=0, file="obspasillo.dat", status="replace", action="write")

xmin = -25
xmax = 25
ymin = 5
ymax = 5

n = 60

do i = 0, n
	x = i * (xmax - xmin)/n + xmin
	y = i * (ymax - ymin)/n + ymin
	write(unit=0, fmt=*) x, y
end do

ymin = -5
ymax = -5

do i = 0, n
	x = i * (xmax - xmin)/n + xmin
	y = i * (ymax - ymin)/n + ymin
	write(unit=0, fmt=*) x, y
end do

end program opasillo