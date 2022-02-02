program opasillo

real :: x1, x2, y1, y2, x, y, sep, rrt, theta, xx, yy
integer :: i, n, nrt
real, parameter :: pi = 4.0*atan(1.0)

open (unit=0, file="opuerta.dat", status="replace", action="write")

sep = 0.1

x1 = -15
x2 = 0

n = (x2 - x1) / sep
do i = 0, n
	x = i * sep + x1
	y = 7.5
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

n = (x2 - x1) / sep

do i = 0, n
	x = i * sep + x1
	y = -7.5
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

y1 = 0.5
y2 = 7.5
n = (y2 - y1)/sep

do i = 0, n
	x = 0.0
	y = i * sep + y1
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)
!si se quiere 1 puerta quitar

if (.false.) then
	y1 = -1.0
	y2 = 1.0
	n = (y2 - y1)/sep
	
	do i = 0, n
		x = 0.0
		y = i * sep + y1
		write(unit=0, fmt=*) x, y
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
end if

y1 = -7.5
y2 = -0.5
n = (y2 - y1)/sep
sep = -0.1
do i = 0, n
	x = 0.0
	y = i * sep + y2
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

sep = 0.1
rrt = 0.3
nrt = nint(2.0 * pi * rrt / sep)

x1 = -1.5
y1 = 0.0
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -1.5
y1 = 1.5
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -1.5
y1 = -1.5
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -3.0
y1 = 0.0
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -3.0
y1 = 1.5
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -3.0
y1 = -1.5
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -4.5
y1 = 0.0
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -1.5
y1 = 3.0
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -1.5
y1 = -3.0
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = x1 + rrt * cos(theta)
	yy = y1 + rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)


end program opasillo
