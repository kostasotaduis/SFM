module voronoi

use util

public :: dvor

contains 

subroutine dvor(x, y, xp, yp, vxp, vyp, rc, bi, ro, vv)


real, dimension(:), intent(in) :: xp, yp, vxp, vyp
real, intent(in) :: x, y, rc
integer, intent(in) :: bi
logical, dimension(:,:), allocatable :: vor
integer :: i, j, i0, k, m, n, nrc
real :: xv, yv, dd, dmin, a
real, intent(out) :: ro
real, dimension(:), intent(out) :: vv
logical :: trovato

nrc = 10
allocate(vor(-nrc:nrc,-nrc:nrc))

dmin = rc
trovato = .false.
do i = 1, size(xp)
	dd = d(xp(i), yp(i), x, y)
	if (dd < dmin) then
		if (bi < 0) then
			if (vxp(i) > 0.0) then
				cycle
			end if
		else if (bi > 0) then
			if (vxp(i) < 0.0) then
				cycle
			end if
		end if
		i0 = i
		dmin = dd
		trovato = .true.
	end if
end do

if (trovato) then
	vv(1) = vxp(i0)
	vv(2) = vyp(i0)
	do m = -nrc, nrc
		do n = -nrc, nrc
			vor(m,n) = .true.
		end do
	end do
	do j = -nrc, nrc
		do k = -nrc, nrc
			xv = xp(i0) + j * (rc / nrc)
			yv = yp(i0) + k * (rc / nrc)
			if (d(xv, yv, xp(i0), yp(i0)) > rc) then
				vor(j,k) = .false.
			else
				do i = 1, size(xp)
					if (i == i0) then
						cycle
					else if (d(xp(i0), yp(i0), xp(i), yp(i)) > (2 * rc)) then
						cycle
					else
						if (bi < 0) then
							if (vxp(i) > 0.0) then
								cycle
							end if
						else if (bi > 0) then
							if (vxp(i) < 0.0) then
								cycle
							end if
						end if						
						if (d(xv, yv, xp(i), yp(i)) < d(xv, yv, xp(i0), yp(i0))) then
							vor(j,k) = .false.
						end if
					end if
				end do
			end if
		end do
	end do

	a = 0.0
	do j = -nrc, nrc
		do k = -nrc, nrc
			if (vor(j,k)) then
				a = a + (rc**2.0 / nrc**2.0)
			end if
		end do
	end do
	ro = 1.0 / a
else if (.not. trovato) then
	ro = 0.0
	vv = (/0.0, 0.0/)
end if

end subroutine dvor

end module voronoi
