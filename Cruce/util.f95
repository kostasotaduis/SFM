module util

real, public, parameter :: pi = 4.0 * atan(1.0), e = 10.0 ** (1.0/log(10.0))
public :: norm, d, cercano

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

subroutine cercano(x, y, lx, ly, nc)

real, intent(in) :: x, y
real, dimension(:), intent(in) :: lx, ly
integer, intent(out) :: nc
integer :: i
real :: dc, dd

dc = huge(dc)
do i = 1, size(lx)
	dd = d(x, y, lx(i), ly(i))
	if (dd < dc) then
		dc = dd
		nc = i
	end if
end do

end subroutine cercano

end module util
