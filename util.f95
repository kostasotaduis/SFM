module util

real, public, parameter :: pi = 4.0 * atan(1.0), e = 10.0 ** (1.0/log(10.0))
public :: norm, d

contains

function norm(v) result(nr)

real, dimension(:), intent(in) :: v
real :: nr
integer :: i

nr = 0.0
do i = 1, size(v)
	nr = nr + v(i) ** 2.0
end do
nr = nr ** 0.5

end function norm

function d(x, y, x0, y0) result(dd)

real, intent(in) :: x, y, x0, y0
real :: dd

dd = ((x - x0)**2.0 + (y - y0)**2.0)**(0.5)

end function d

end module util