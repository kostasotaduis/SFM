module cv

use util

public :: dentrocv
real, public :: rcv, thetacv, v000

contains


function dentrocv(x, y, x0, y0, v0x, v0y) result(dentro)

real, intent(in) :: x, y, x0, y0, v0x, v0y
logical :: dentro
real :: cphi, cthetacv
dentro = .false.


if (d(x, y, x0, y0) <= rcv) then
	cphi = ((x - x0) * v0x + (y - y0) * v0y)/ &
	 (d(x, y, x0, y0) * (v0x**2.0 + v0y**2.0))
	cthetacv = cos(thetacv/2.0)
	if (cphi >= cthetacv) then
		dentro = .true.
	end if
end if

end function dentrocv

end module cv
