program opasillo

real :: x1, x2, y1, y2, x, y, sep
integer :: i, n

open (unit=0, file="opuerta.dat", status="replace", action="write")

sep = 0.2

x1 = -25
x2 = 25

n = (x2 - x1) / sep
do i = 0, n
	x = i * sep + x1
	y = 8.0
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

n = (x2 - x1) / sep

do i = 0, n
	x = i * sep + x1
	y = -8.0
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

y1 = 0.7
y2 = 8
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

y1 = -8
y2 = -0.7
n = (y2 - y1)/sep
sep = -0.2
do i = 0, n
	x = 0.0
	y = i * sep + y2
	write(unit=0, fmt=*) x, y
end do
write(unit=0, fmt=*)
write(unit=0, fmt=*)

end program opasillo
