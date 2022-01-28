program carriles

integer :: i, is, nlineas, nbins, j, nbin
real :: bsr
real, dimension(:), allocatable :: y, vx, mvx
character(len=3) :: cont
real :: ymin, ymax, deltay, ybin1, ybin2


open (unit=0, file="pasillo.dat", status="old", action="read")
open (unit=1, file="carril1.dat", status="replace", action="write")

nlineas = 0
do
	read (unit=0, iostat=is, fmt="(a)") cont
	nlineas = nlineas + 1
	if (is < 0 .or. (cont == "")) then
	nlineas = nlineas - 1
		exit
	end if
end do
rewind(unit=0)
allocate(y(nlineas), vx(nlineas))
do i = 1, nlineas
	read (unit=0, iostat=is, fmt=*)
end do
do i = 1, nlineas
	read (unit=0, iostat=is, fmt=*) bsr, y(i), vx(i), bsr
end do

ymin = -5
ymax = 5


nbins = 20
allocate(mvx(nbins))
deltay = (ymax - ymin) / nbins


do i = 1, nbins
	nbin = 0
	mvx(i) = 0.0
	ybin1 = (i-1)*deltay + ymin
	ybin2 = i*deltay + ymin
	do j = 1, size(y)
		if ((y(j) >= ybin1) .and. (y(j) < ybin2)) then
			nbin = nbin + 1
			mvx(i) = mvx(i) + vx(j)
		end if
	end do
	mvx(i) = mvx(i) / nbin
	write(unit=1, fmt=*) (i - 0.5) * deltay + ymin, mvx(i), nbin
end do
	


close(unit=0)
close(unit=1)

end program carriles