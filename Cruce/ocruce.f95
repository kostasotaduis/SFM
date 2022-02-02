program opasillo

real :: x1, x2, y1, y2, x, y, sep, x1n, y1n, x2n, y2n
integer :: i, n, nrt, j, sx, sy
real, parameter :: pi = 4.0 * atan(1.0)
real :: rrt, theta
real, dimension(3) :: xx, yy, xxn, yyn

open (unit=0, file="ocruce.dat", status="replace", action="write")

sep = 0.2

x1 = 4.0
x2 = 75

n = (x2 - x1) / sep
do i = 0, n
	x = i * sep + x1
	y = 4.0
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

n = (x2 - x1) / sep

do i = 0, n
	x = i * sep + x1
	y = -4.0
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -75
x2 = -4.0

n = (x2 - x1) / sep
do i = 0, n
	x = i * sep + x1
	y = 4.0
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

n = (x2 - x1) / sep

do i = 0, n
	x = i * sep + x1
	y = -4.0
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)


y1 = -75
y2 = -4.0
n = (y2 - y1)/sep

do i = 0, n
	x = -4.0
	y = i * sep + y1
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

n = (y2 - y1)/sep

do i = 0, n
	x = 4.0
	y = i * sep + y1
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)


y1 = 4.0
y2 = 75
n = (y2 - y1)/sep
sep = 0.2
do i = 0, n
	x = -4.0
	y = i * sep + y1
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

n = (y2 - y1)/sep
sep = 0.2
do i = 0, n
	x = 4.0
	y = i * sep + y1
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

!rotonda

rrt = 0.5
nrt = nint(2.0 * pi * rrt / sep)
if (.false.) then
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	x = rrt * cos(theta)
	y = rrt * sin(theta)	
	write(unit=0, fmt=*) x, y
end do

write(unit=0, fmt=*)
write(unit=0, fmt=*)
end if

!rotondificadores
if (.false.) then
x1 = 4.0
x2 = 20.0
y1 = 1.5
y2 = -1.5
n = ((x2 - x1)**2.0 + (y2 - y1)**2.0)**0.5 / sep
theta = pi / 2.0
do i = 1, 4
	sx = (x2 - x1)/abs(x2 - x1)
	sy = (y2 - y1)/abs(y2 - y1)
	do j = 0, n
		x = j * (x2 - x1)/n + x1
		y = j * (y2 - y1)/n + y1
		write(unit=0, fmt=*) x, y
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
	x1n = -y1
	y1n = x1
	x2n = -y2
	y2n = x2
	x1 = x1n
	y1 = y1n
	x2 = x2n
	y2 = y2n
end do
end if
	
!rotondificadores 2

xx = (/4.0, 4.0, 6.0/)


end program opasillo

