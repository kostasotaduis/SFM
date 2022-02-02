module cv

use util

public :: dentrocv, dz
real, public :: rcv, thetacv

contains


function dentrocv(x, y, x0, y0, v0x, v0y) result(dentro)

real, intent(in) :: x, y, x0, y0, v0x, v0y
integer :: dentro
real :: cphi, cthetacv
dentro = 0


if (d(x, y, x0, y0) <= rcv) then
	dentro = 1
	cphi = ((x - x0) * v0x + (y - y0) * v0y)/ &
	 (d(x, y, x0, y0) * (v0x**2.0 + v0y**2.0))
	cthetacv = cos(thetacv/2.0)
	if (cphi >= cthetacv) then
		dentro = 2
	end if
end if

end function dentrocv

subroutine dz(x, y, xzmin, xzmax, yzmin, yzmax, dest, iz)

real, intent(in) :: x, y
real, dimension(:), intent(in) :: xzmin, xzmax, yzmin, yzmax
integer, intent(in) :: dest
integer, intent(out) :: iz
integer :: i

iz = 0
do i = 1, size(xzmax)
	if ((x >= xzmin(i)) .and. (x <= xzmax(i)) .and. &
	 (y >= yzmin(i)) .and. (y <= yzmax(i))) then
		iz = i
		if (i == dest) then
			exit
		end if
	end if
end do

end subroutine dz

end module cv
