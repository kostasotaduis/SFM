module fobs

use util
use cv

public :: fo, ocercano
real, public :: ao, oR

contains

function fo(x, y, x0, y0) result(fobj)

real, intent(in) :: x, y, x0, y0
real, dimension(2) :: fobj
real :: h
h = 0.1

fobj(1) = -ao * (e**(-(d(x+h,y,x0,y0)/(oR))) - &
 e**(-(d(x-h,y,x0,y0)/(oR)))) / (2*h)
fobj(2) = -ao * (e**(-(d(x,y+h,x0,y0)/(oR))) - &
 e**(-(d(x,y-h,x0,y0)/(oR)))) / (2*h)

end function fo

subroutine ocercano(x0, y0, v0x, v0y, lx, ly, otrovato, ic)

real, intent(in) :: x0, y0, v0x, v0y
real, dimension(:), intent(in) :: lx, ly
integer, intent(out) :: ic
integer :: i
real :: x, y, dmin, dd
logical, intent(out) :: otrovato

if ((size(lx)) /= (size(ly))) then
	print*, "error"
end if

x = huge(x)
y = huge(y)
dmin = huge(dmin)
otrovato = .false.
do i = 1, size(lx)
	if (dentrocv(lx(i), ly(i), x0, y0, v0x, v0y)) then
		dd = d(lx(i), ly(i), x0, y0)
		if (dd < dmin) then
			otrovato = .true.
			dmin = dd
			ic = i
		end if
	end if
end do

end subroutine ocercano

end module fobs
