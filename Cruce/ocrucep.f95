program opasillo

real :: x1, x2, y1, y2, x, y, sep, x1n, y1n, x2n, y2n
integer :: i, n, nrt, j, sx, sy
real, parameter :: pi = 4.0 * atan(1.0)
real :: rrt, theta
real, dimension(3) :: xx, yy, xxn, yyn

open (unit=0, file="ocruce.dat", status="replace", action="write")

sep = 0.2

x1 = 0.0
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
x2 = -0.0

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

end program opasillo

