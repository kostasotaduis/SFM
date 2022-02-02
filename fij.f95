module fij

use util
use cv

public :: b, fev
real, public :: deltapaso, sigma, fev0

contains

function b(rij, vj) result(bb)

real, dimension(:), intent(in) :: rij, vj
real :: bb

bb = ((norm(rij) + norm(rij - vj * deltapaso))**2.0 - &
 (norm(vj) * deltapaso)**2.0)**0.5 / 2.0

end function b

function fev(x, y, x0, y0, v0x, v0y) result(fevv)

real, intent(in) :: x, y, y0, x0, v0x, v0y
real, dimension(2) :: fevv, v00
real :: h
h = 0.1

v00 = (/v0x, v0y/) 

fevv(1) = -fev0 * (e**(-b((/x0-(x+h),y0-y/), v00) / sigma) - &
 e**(-b((/x0-(x-h),y0-y/), v00) / sigma)) / (2 * h)
fevv(2) = -fev0 * (e**(-b((/x0-x,y0-(y+h)/), v00) / sigma) - & 
 e**(-b((/x0-x,y0-(y-h)/), v00) / sigma)) / (2 * h)

end function fev

end module fij