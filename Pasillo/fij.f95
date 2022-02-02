module fij

use util
use cv

public :: b, fev, uev
real, public :: deltapaso, sigma, fev0

contains

function b(rij, vj) result(bb)

real, dimension(:), intent(in) :: rij, vj
real :: bb

bb = (abs((norm(rij) + norm(rij - vj * deltapaso))**2.0 - &
 (norm(vj) * deltapaso)**2.0))**0.5 / 2.0

end function b

function uev(x, y, x0, y0, v0x, v0y) result(uevv)

real, intent(in) :: x, y, x0, y0, v0x, v0y
real :: uevv

uevv = fev0 * e**(-((b((/x-x0,y-y0/),(/v0x, v0y/)))/(sigma)))


end function uev


subroutine fev(x, y, x0, y0, vx, vy, v0x, v0y, fevv)

real, intent(in) :: x, y, x0, y0, vx, vy, v0x, v0y
real, dimension(2) :: v00, vv
real, dimension(:), intent(out) :: fevv
real :: h
real :: bx1, bx2, by1, by2
h = 0.1

fevv = (/0.0, 0.0/)

v00 = (/v0x, v0y/) 
vv = (/vx, vy/)
bx1 = b((/((x+h)-x0),(y-y0)/), v00)
bx2 = b((/((x-h)-x0),(y-y0)/), v00)
by1 = b((/(x-x0),((y+h)-y0)/), v00)
by2 = b((/(x-x0),((y-h)-y0)/), v00)

fevv(1) = -fev0 * (e**((-bx1) / (sigma * (0.5 + 0.5))) - &
 e**((-bx2) / (sigma * (0.5 + 0.5)))) / (2.0 * h)
fevv(2) = -fev0 * (e**((-by1) / (sigma * (0.5 + 0.5))) - & 
 e**((-by2) / (sigma * (0.5 + 0.5)))) / (2.0 * h)

end subroutine fev

end module fij
