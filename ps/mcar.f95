program mcar

real :: ancho, ymin, ymax, bsr, xmin, xmax, x, y
real :: dns, v0, mc, ftheta0
real, dimension(:), allocatable :: cr
integer, dimension(:), allocatable :: np
integer :: i, nl, dir, nc, j, nt, n
integer :: is
ancho = 10
dns = 0.3
v0 = 1.34
nt = 20
xmin = -25
xmax = 25
n = 60
open(unit=1, file="mcardns.dat", status="replace", action="write")
!para hacerlo con ancho ponerlo aquí(cambiar final del do tb)
!do
	if (.false.) then
		ymin = -(ancho/2.0)
		ymax = ancho/2.0
		open (unit=0, file="obspasillo.dat", status="replace", action="write")
		do i = 0, n
			x = i * (xmax - xmin)/n + xmin
			y = ymax
			write(unit=0, fmt=*) x, y
		end do
		do i = 0, n
			x = i * (xmax - xmin)/n + xmin
			y = ymin
			write(unit=0, fmt=*) x, y
		end do
		close(unit=0)
	end if
	
!para hacerlo con densidad o fluc poner este
	if (.true.) then
		ymin = -(ancho/2.0)
		ymax = ancho/2.0
		open (unit=0, file="obspasillo.dat", status="replace", action="write")
		do i = 0, n
			x = i * (xmax - xmin)/n + xmin
			y = ymax
			write(unit=0, fmt=*) x, y
		end do
		do i = 0, n
			x = i * (xmax - xmin)/n + xmin
			y = ymin
			write(unit=0, fmt=*) x, y
		end do
		close(unit=0)
	end if

!dns = 0.01
ftheta0 = 0.0
do	
	open (unit=0, file="pdata.dat", status="replace", action="write")
	write(unit=0, fmt=*) ymin+0.1, ymax-0.1, dns, ftheta0
	close(unit=0)
	mc = 0.0
	do j = 1, nt
		call system("./pasillo")
		call system("./carriles")
		open (unit=0, file="carril1.dat", status="old", action="read")
		rewind(unit=0)
		nl = 0
		do
			read (unit=0, iostat=is, fmt=*)
			if (is < 0) then
				exit
			end if
			nl = nl + 1
		end do
		rewind(unit=0)
		allocate(cr(nl), np(nl))
		do i = 1, nl
			read(unit=0, fmt=*) bsr, cr(i), np(i)
		end do
		close(unit=0)
		dir = 0
		nc = 0
		do i = 1, nl
			if (np(i) > 5) then
				if (abs(cr(i)) > 0.7 * v0) then
					if ((dir * cr(i) < 0.0) .or. (dir == 0)) then
						nc = nc + 1
						dir = nint(cr(i)/abs(cr(i)))
					end if
				end if
			end if
		end do
		if (nc == 0) then
		end if
		mc = mc + nc
		deallocate(cr, np)
	end do
	mc = mc / nt
	write (unit=1, fmt=*) ftheta0, mc	
	!ancho = ancho + 0.2
	!if (ancho > 20.0) then
	!	exit
	!end if
	!if (dns < 0.195) then
	!	dns = dns + 0.01
	!else if (dns > 0.195) then
	!	dns = dns + 0.04
	!end if
	!if (dns > 0.5) then
	!	exit
	!end if
	ftheta0 = ftheta0 + 0.1
	if (ftheta0 > 4.0) then
		exit
	end if

end do

close(unit=1)

end program mcar
