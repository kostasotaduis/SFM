module fluc

use util

real, public :: sigmafluc, ftheta0
public :: ffluc, gaus

contains

subroutine ffluc(vx, vy, ff)

real, intent(in) :: vx, vy
real, dimension(:), intent(out) :: ff
real :: ftheta

call gaus(sigmafluc, ftheta)
ftheta = ftheta * ftheta0
if ((abs(vx) + abs(vy)) > 0.0) then
ff(2) = ftheta * (vx)/(abs(vx)) * (abs(vx))**(1.5) &
 * (vx**2.0 + vy**2.0 + ftheta0/4.0)**(-0.5)
ff(1) = ftheta * (vy)/(abs(vy)) * (abs(vy))**(1.5) &
 * (vx**2.0 + vy**2.0 + ftheta0/4.0)**(-0.5)
end if

end subroutine ffluc

subroutine gaus(sigma, g)

real, intent(in) :: sigma
real, intent(out) :: g
real :: r, k

k = pi**2.0 / (6.0 * sigma)
call random_number(r)
r = r * 0.8 + 0.1
g = -(1.0/k) * (log((1-r)/r))

end subroutine gaus

end module fluc