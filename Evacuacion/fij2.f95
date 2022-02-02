module fij

use util
use cv

public :: b, fev, fric, body
real, public :: deltapaso, sigma, fev0, kappab, kappaf


contains

function b(rij, vj) result(bb)

real, dimension(:), intent(in) :: rij, vj
real :: bb

bb = (abs((norm(rij) + norm(rij - vj * deltapaso))**2.0 - &
 (norm(vj) * deltapaso)**2.0))**0.5 / 2.0

end function b

subroutine fev(x, y, x0, y0, v0x, v0y, fevv)

real, intent(in) :: x, y, x0, y0, v0x, v0y
real, dimension(2) :: v00
real, dimension(:), intent(out) :: fevv
real :: h
real :: bx1, bx2, by1, by2
h = 0.1

fevv = (/0.0, 0.0/)

v00 = (/v0x, v0y/) 

bx1 = b((/((x+h)-x0),(y-y0)/), v00)
bx2 = b((/((x-h)-x0),(y-y0)/), v00)
by1 = b((/(x-x0),((y+h)-y0)/), v00)
by2 = b((/(x-x0),((y-h)-y0)/), v00)

fevv(1) = -fev0 * (e**((-bx1) / sigma) - &
 e**((-bx2) / sigma)) / (2.0 * h)
fevv(2) = -fev0 * (e**((-by1) / sigma) - & 
 e**((-by2) / sigma)) / (2.0 * h)

end subroutine fev

subroutine fric(x1, y1, x2, y2, v1x, v1y, v2x, v2y, r1, r2, fricc)

real, intent(in) :: x1, x2, y1, y2, v1x, v2x, v1y, v2y, r1, r2
real, dimension(2) :: n21, v11, v22, t21
real, dimension(:), intent(out) :: fricc
real :: d12, v12

n21 = (/x1 - x2, y1 - y2/)
d12 = norm(n21)
n21 = n21 / d12
t21 = (/-n21(2), n21(1)/)
v11 = (/v1x, v1y/)
v22 = (/v2x, v2y/)
v12 = dot_product((v22-v11), t21)

fricc = kappaf * ((r1 + r2) - d12) * v12 * t21

end subroutine fric

subroutine body(xi, yi, xj, yj, ri, rj, bodyy)

real, intent(in) :: xi, yi, xj, yj, ri, rj
real, dimension(:), intent(out) :: bodyy
real, dimension(2) :: nji
real :: dij
nji = (/xi - xj, yi - yj/)
dij = norm(nji)
nji = nji / dij

bodyy = kappab * ((ri + rj) - dij) * nji

end subroutine body

end module fij
