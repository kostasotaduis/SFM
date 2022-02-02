program opasillo

real :: xmin, xmax, ymin, ymax, x, y
integer :: i, n, j
real, parameter :: pi = 4.0 * atan(1.0)
real :: rcol, theta, sepcol, x0, y0
integer :: ncol, ntheta

open (unit=0, file="opasillo.dat", status="replace", action="write")

xmin = -25
xmax = 25
ymin = 4.0
ymax = 4.0

n = 200

do i = 0, n
	x = i * (xmax - xmin)/n + xmin
	y = i * (ymax - ymin)/n + ymin
	write(unit=0, fmt=*) x, y
end do

write(unit=0, fmt=*)
write(unit=0, fmt=*)

ymin = -4.0
ymax = -4.0

do i = 0, n
	x = i * (xmax - xmin)/n + xmin
	y = i * (ymax - ymin)/n + ymin
	write(unit=0, fmt=*) x, y
end do

write(unit=0, fmt=*)
write(unit=0, fmt=*)



rcol = 0.15
ntheta = 8
ncol = 25
sepcol = (xmax - xmin)/ncol

y0 = 0.0
if (.false.) then
do i = 1, ncol
	x0 = xmin - (sepcol/2.0) + i * sepcol
	theta = 0.0
	do j = 0, ntheta
		theta = ((2 * pi) / ntheta) * j 
		x = x0 + rcol * cos(theta)
		y = y0 + rcol * sin(theta)
		write(unit=0, fmt=*) x, y
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
end do
end if


close (unit=0)

end program opasillo
