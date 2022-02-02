module fij

use util
use cv

public :: b, fev
real, public :: deltapaso, sigma, fev0, fevp0
private :: pev
contains

function b(rij, vj) result(bb)

real, dimension(:), intent(in) :: rij, vj
real :: bb

bb = (abs((norm(rij) + norm(rij - vj * deltapaso))**2.0 - &
 (norm(vj) * deltapaso)**2.0))**0.5 / 2.0

end function b

subroutine fev(x, y, x0, y0, vx, vy, vx0, vy0, fevv)

real, intent(in) :: x, y, x0, y0, vx, vy, vx0, vy0
real, dimension(:), intent(out) :: fevv
real :: h
h = 0.1

fevv = (/0.0, 0.0/)

fevv(1) = - (pev(x+h, y, x0, y0, vx, vy, vx0, vy0) - &
 pev(x-h, y, x0, y0, vx, vy, vx0, vy0)) / (2.0*h)
fevv(2) =  - (pev(x, y+h, x0, y0, vx, vy, vx0, vy0) - &
 pev(x, y-h, x0, y0, vx, vy, vx0, vy0)) / (2.0*h)

end subroutine fev

function pev(xi, yi, xj, yj, vxi, vyi, vxj, vyj) result(pevv)

real, intent(in) :: xi, yi, xj, yj, vxi, vyi, vxj, vyj
real :: pevv, dij, d2, dijp
real, dimension(2) :: vi, vj

vi = (/vxi, vyi/)
vj = (/vxj, vyj/)
dij = d(xi, yi, xj, yj)
dijp = d(xi, yi, xj+0.5*vj(1), yj+0.5*vj(2))
if (norm(vi) == 0.0) then
	d2 = d(xi, yi, (xj + vj(1)), (yj + vj(2)))
else if (norm(vj) == 0.0) then
	d2 = d(xi, yi, &
	 (xj + vj(1) - vi(1)), &
 	 (yj + vj(2) - vi(2)))
else
	d2 = d(xi, yi, (xj + vj(1) - vi(1)), (yj + vj(2) - vi(2)))
end if

pevv = fev0 * (e**(-dij/sigma) + &
 (norm(vi-vj)) / 1.34 * ( &
 abs(dot_product(vi, vj)/(norm(vj)*norm(vi)+0.001)) * e**(-(dijp**2.0)/(1.5*sigma)) + &
 fevp0 * (1 - abs(dot_product(vi, vj)/(norm(vj)*norm(vi)+0.001))) * e ** (-(d2**2.0)/(0.7*sigma))))

end function pev

end module fij
