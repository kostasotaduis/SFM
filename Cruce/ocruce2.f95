program ocruce

use util

real :: sep, alpha, theta
integer :: i, j, k, n, nrt
real, dimension (:), allocatable :: x, y, xn, yn
real, dimension(2,2) :: mr
real :: xx, yy, rrt


open (unit=0, file="ocruce2.dat", status="replace", action="write")

sep = 0.2
alpha = pi/2.0
mr = reshape((/0.0, -1.0, 1.0, 0.0/), shape=(/2,2/))
!allocate(x(4), y(4), xn(4), yn(4))
allocate(x(3), y(3), xn(3), yn(3))
!x = (/4.0, 4.0, 6.0, 25.0/)
!y = (/25.0, 6.0, 4.0, 4.0/)
x = (/4.0, 4.0, 75.0/)
y = (/75.0, 4.0, 4.0/)
if (.true.) then
do k = 1, 4
	n = nint((norm((/x(2)-x(1),y(2)-y(1)/)))/sep)
	do i = 0, n
		xx = i * (x(2) - x(1)) / n + x(1)
		yy = i * (y(2) - y(1)) / n + y(1)
		write(unit=0, fmt=*) xx, yy
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
	n = nint((norm((/(x(3)-x(2)),(y(3)-y(2))/)))/sep)
	do i = 0, n
		xx = i * (x(3) - x(2)) / n + x(2)
		yy = i * (y(3) - y(2)) / n + y(2)
		write(unit=0, fmt=*) xx, yy
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
	!n = nint((norm((/(x(4)-x(3)),(y(4)-y(3))/)))/sep)
	!do i = 0, n
	!	xx = i * (x(4) - x(3)) / n + x(3)
	!	yy = i * (y(4) - y(3)) / n + y(3)
	!	write(unit=0, fmt=*) xx, yy
	!end do
	!write(unit=0, fmt=*)
	!write(unit=0, fmt=*)
	do i = 1, size(x)
		xn(i) = -y(i)
		yn(i) = x(i)
	end do
	x = xn
	y = yn
end do
end if
deallocate(x, y, xn, yn)
allocate(x(3), y(3), xn(3), yn(3))
x = (/4.0, 7.0, 14.0/)
y = (/1.0, -0.5, -1.0/)
if (.true.) then
do k = 1, 4
	!n = nint((norm((/x(2)-x(1),y(2)-y(1)/)))/sep)
	!do i = 0, n
	!	xx = i * (x(2) - x(1)) / n + x(1)
	!	yy = i * (y(2) - y(1)) / n + y(1)
	!	write(unit=0, fmt=*) xx, yy
	!end do
	!write(unit=0, fmt=*)
	!write(unit=0, fmt=*)
	!n = nint((norm((/(x(3)-x(2)),(y(3)-y(2))/)))/sep)
	!do i = 0, n
	!	xx = i * (x(3) - x(2)) / n + x(2)
	!	yy = i * (y(3) - y(2)) / n + y(2)
	!	write(unit=0, fmt=*) xx, yy
	!end do
	!write(unit=0, fmt=*)
	!write(unit=0, fmt=*)
	n = nint((norm((/(x(1)-x(3)),(y(1)-y(3))/)))/sep)
	do i = 0, n
		xx = i * (x(1) - x(3)) / n + x(3)
		yy = i * (y(1) - y(3)) / n + y(3)
		write(unit=0, fmt=*) xx, yy
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
	do i = 1, 3
		xn(i) = -y(i)
		yn(i) = x(i)
	end do
	x = xn
	y = yn
end do
end if


x = (/14.0, 7.0, 24.0/)
y = (/0.5, -0.5, -0.5/)
if (.true.) then
do k = 1, 4
	!n = nint((norm((/x(2)-x(1),y(2)-y(1)/)))/sep)
	!do i = 0, n
	!	xx = i * (x(2) - x(1)) / n + x(1)
	!	yy = i * (y(2) - y(1)) / n + y(1)
	!	write(unit=0, fmt=*) xx, yy
	!end do
	!write(unit=0, fmt=*)
	!write(unit=0, fmt=*)
	!n = nint((norm((/(x(3)-x(2)),(y(3)-y(2))/)))/sep)
	!do i = 0, n
	!	xx = i * (x(3) - x(2)) / n + x(2)
	!	yy = i * (y(3) - y(2)) / n + y(2)
	!	write(unit=0, fmt=*) xx, yy
	!end do
	!write(unit=0, fmt=*)
	!write(unit=0, fmt=*)
	n = nint((norm((/(x(1)-x(3)),(y(1)-y(3))/)))/sep)
	do i = 0, n
		xx = i * (x(1) - x(3)) / n + x(3)
		yy = i * (y(1) - y(3)) / n + y(3)
		write(unit=0, fmt=*) xx, yy
	end do
	write(unit=0, fmt=*)
	write(unit=0, fmt=*)
	do i = 1, 3
		xn(i) = -y(i)
		yn(i) = x(i)
	end do
	x = xn
	y = yn
end do
end if


rrt = 0.3
nrt = nint(2.0 * pi * rrt / sep)

if (.false.) then
do i = 0, nrt - 1
	theta = ((2.0 * pi) / nrt) * i
	xx = rrt * cos(theta)
	yy = rrt * sin(theta)	
	write(unit=0, fmt=*) xx, yy
end do
end if
!write(unit=0, fmt=*) 0.0, 0.0
write(unit=0, fmt=*)
write(unit=0, fmt=*)





end program ocruce
