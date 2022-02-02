module fluc

use util

real, public :: sigmafluc, ftheta0
public :: ffluc, gaus

contains

subroutine ffluc(ff)

real, dimension(:), intent(out) :: ff
real :: ftheta, r

call random_number(r)
ftheta = r * 2 * pi

ff(1) = ftheta0 * cos(ftheta)
ff(2) = ftheta0 * cos(ftheta)

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
