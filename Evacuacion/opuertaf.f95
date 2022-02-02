program opuerta

real :: x, y, x1, x2, y1, y2, xx, yy
real :: sep, theta, rrt
integer :: n, nrt, i
real, parameter :: pi = 4.0*atan(1.0)

open (unit=0, file="opuerta.dat", status="replace", action="write")

sep = 0.1

x1 = -15
x2 = 0.0
n = (x2 - x1)/sep

do i = 0, n
	x = i * (x2 - x1)/n + x1
	y = 7.5
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = -15
x2 = 0.0
n = (x2 - x1)/sep

do i = 0, n
	x = i * (x2 - x1)/n + x1
	y = -7.5
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

rrt = 7.0
nrt = 0.5 * pi * rrt / sep

x1 = 7
y1 = -7.5

do i = 0, nrt
	theta = i * 0.5 * pi / nrt
	x = x1 - rrt*cos(theta)
	y = y1 + rrt*sin(theta)
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

x1 = 7
y1 = 7.5

do i = 0, nrt
	theta = i * 0.5 * pi / nrt
	x = x1 - rrt*cos(theta)
	y = y1 - rrt*sin(theta)
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)


end program opuerta
