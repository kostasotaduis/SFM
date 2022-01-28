program colorize

integer :: i, is
real :: x, y, r, g, b, vx, vxmax


open (unit=0, file="carril1.dat", status="old", action="read")
open (unit=1, file="carril1col.dat", status="replace", action="write")

vxmax = 0
do
	read (unit=0, iostat=is, fmt=*) x, vx, y
	if (is < 0) then
		exit
	end if
	if (abs(vx) > vxmax) then
		vxmax = vx
	end if
end do
rewind(unit=0)
do
	read (unit=0, iostat=is, fmt=*) x, vx, y
	if (is < 0) then
		exit
	end if
	r = 125
	g = ((vx+vxmax)/(2*vxmax)) * (255.0)
	b = 255.0 - ((vx+vxmax)/(2*vxmax))*(255.0)
	write(unit=1, fmt=*) x, vx, r, g, b
end do

close(unit=0)
close(unit=1)

end program colorize
