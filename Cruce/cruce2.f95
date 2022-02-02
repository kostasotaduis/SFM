program i1

use util
use voronoi
use cv
use fobs
use fij
use fluc

real, dimension(:), allocatable :: x, y, vx, vy
real, dimension(:), allocatable :: xd, yd, edx, edy, edxp, edyp, fdx, fdy, v0, v0max
real, dimension(:), allocatable :: dedxp, dedyp
real, dimension(:), allocatable :: xzmax, xzmin, yzmax, yzmin, zedx, zedy, pdx, pdy
real, dimension(:), allocatable :: thzmin, thzmax
real, dimension(:), allocatable :: xz0max, xz0min, yz0max, yz0min
real, dimension(2) :: ud
integer, dimension(:), allocatable :: tp, dest
integer, dimension(:,:), allocatable :: mdest
integer :: iz
real :: tau, sigma0, deltav0, taup, taupp
real :: tpaso
integer :: h, topeh
real, dimension(:), allocatable :: ox, oy, fov
real, dimension(:), allocatable :: fox, foy, frotx, froty
integer :: nlineas, status, ic
logical :: otrovato
real, dimension(2) :: fevv
real, dimension(:), allocatable :: fevx, fevy
integer :: N, i, j
real, dimension(2) :: fflu
real, dimension(:), allocatable :: fflux, ffluy
  real :: xmax, xmin, ymax, ymin, xr, dns
!per voronoi
real :: yvmin, yvmax, xvmax, xvmin, xv, yv, rho, rhot
real :: fljout, rot, flj, thetarot, frot0
integer :: npasov, iv
real, dimension(2) :: vc, rhva, rhvb, rhvc, rhvd, vn
character(len=3) :: cont
logical, dimension(:), allocatable :: entrado




!parametros simulacion
tpaso = 0.2
call random_seed()

!pasillo
xmin = -25.0
xmax = 25.0
ymin = -25.0
ymax = 25.0


!zonas
allocate(xzmin(5), xzmax(5), yzmin(5), yzmax(5))
allocate(xz0min(4), xz0max(4), yz0min(4), yz0max(4))
allocate(thzmin(4), thzmax(4))
allocate(zedx(4), zedy(4), pdx(4), pdy(4))
xzmin = (/4.0, -4.0, -75.0, -4.0, -4.0/)
xzmax = (/75.0, 4.0, -4.0, 4.0, 4.0/)
yzmin = (/-4.0, 4.0, -4.0, -75.0, -4.0/)
yzmax = (/4.0, 75.0, 4.0, -4.0, 4.0/)
xz0min = (/25.0, -4.0, -75.0, -4.0/)
xz0max = (/75.0, 4.0, -25.0, 4.0/)
yz0min = (/-4.0, 25.0, -4.0, -75.0/)
yz0max = (/4.0, 75.0, 4.0, -25.0/)
thzmin = pi * (/-0.75, -0.75, -0.25, 0.25/)
thzmax = pi * (/0.75, -0.25, 0.25, 0.75/)
zedx = (/1.0, 0.0, -1.0, 0.0/)
zedy = (/0.0, 1.0, 0.0, -1.0/)
pdx = (/4.0, 0.0, -4.0, 0.0/)
pdy = (/0.0, 4.0, 0.0, -4.0/)
!matriz direciones
allocate(mdest(-1:1,4))
mdest(-1,:) = (/4, 1, 2, 3/)
mdest(0,:) = (/3, 4, 1, 2/)
mdest(1,:) = (/2, 3, 4, 1/)
!



!numero de individuos
dns = 0.8
N = nint(dns * ((xmax - xmin) * (ymax - ymin) - 4 * 21**2.0))
!N = 1
allocate(x(N), y(N), vx(N), vy(N))
allocate(edx(N), edy(N), edxp(N), edyp(N), fdx(N), fdy(N), dedxp(N), dedyp(N))
allocate(yd(N), xd(N), v0(N), v0max(N))
allocate(fevx(N), fevy(N))
allocate(fflux(N), ffluy(N))
allocate(fox(N), foy(N), frotx(N), froty(N))
allocate(tp(N), dest(N), entrado(N))

!parametros individuo
sigma0 = 0.26
do i = 1, N
	call gaus(sigma0, deltav0)
	v0(i) = 1.34 + deltav0
	v0max(i) = 1.3 * v0(i)
end do
rcv = 3.0
thetacv = 200 * pi / 180.0

!fluctuaciones
sigmafluc = 0.7
ftheta0 = 0.1

!parametros objetivo(dESTINO)
tau = 0.5
taup = 1.5
taupp = 0.3

!parametros obstaculos
ao = 10.0
oR = 0.20
frot0 = 0.0
allocate(fov(2))
open(unit=0, file="ocruce.dat", status="old", action="read")
nlineas = 0
do
	read (unit = 0, fmt="(a)", iostat=status) cont
	if ((status < 0)) then
		exit
	else if (cont == "") then
		cycle
	end if
	nlineas = nlineas + 1
end do
allocate(ox(nlineas), oy(nlineas))
rewind(unit=0)
do i = 1, nlineas
	read (unit=0, fmt=*, iostat=status) ox(i), oy(i)
	if (status < 0) then
		exit
	end if
end do
close (unit=0)

!parametros interaccion entre individuos
deltapaso = 2.0
sigma = 0.3
fev0 = 2.1
fevp0 = 0.3

!parametros voronoi
yvmin = -1.4
yvmax = 1.4
npasov = 5

!condiciones iniciales
do i = 1, N
	call random_number(xr)
	xr = xr * 4 + 0.5
	iz = nint(xr)
	do
		call random_number(xr)
		x(i) = xr * (xz0max(iz) - xz0min(iz)) + xz0min(iz) + 0.2
		call random_number(xr)
		y(i) = xr * (yz0max(iz) - yz0min(iz)) + yz0min(iz) + 0.2
		if (d(x(i), y(i), 0.0, 0.0) > 1.5) then
			exit
		end if
	end do
	vx(i) = 0.0
	vy(i) = 0.0
	entrado(i) = .false.
	edxp(i) = 0.0
	edyp(i) = 0.0
	call random_number(xr)
	xr = xr * 3.0 - 1.5
	tp(i) = nint(xr)
	if (iz == 5) then
		call random_number(xr)
		xr = xr * 4.0 + 0.5
		dest(i) = nint(xr)
	else
		dest(i) = mdest(tp(i), iz)
	end if
end do


!abrir archivo para escribir datos
open (unit=1, file="cruce1.dat", status="replace", action="write")
open (unit=2, file="cruce2.dat", status="replace", action="write")
open (unit=3, file="cruce3.dat", status="replace", action="write")

!escribir primer paso (poner false si no se quiere

if (.false.) then
	do i = 1, N
		write(unit=2, fmt=*) x(i), y(i), dest(i), vx(i), vy(i)
	end do
	write(unit=2, fmt=*)
	write(unit=2, fmt=*)
end if
!iteracion
topeh = 300
do h = 1, topeh
	!sacar las fuerzas de todos
	do i = 1, N
		!velocidad deseada
		!ver donde estan
		ud = 0.0 * ud
		call dz(x(i), y(i), xzmin, xzmax, yzmin, yzmax, dest(i), iz)
		if ((iz == dest(i))) then
			edx(i) = zedx(dest(i)) + taupp * vx(i)/v0(i)
			edy(i) = zedy(dest(i)) + taupp * vy(i)/v0(i)
		else if (iz == 5) then
			if (d(x(i), y(i), 0.0, 0.0) <= 3.0) then
			thetarot = atan2(y(i), x(i))
			if (thetarot >= thzmin(dest(i)) .and. thetarot <= thzmax(dest(i))) then
				edx(i) = -sin(thetarot) + taupp * zedx(dest(i)) + taupp * vx(i)/v0(i)
				edy(i) = cos(thetarot) + taupp * zedx(dest(i)) + taupp * vy(i)/v0(i)
			else if ((abs(thetarot) >= abs(thzmax(dest(i)))) .and. &
			 (abs(thetarot) >= abs(thzmin(dest(i))))) then
				edx(i) = -sin(thetarot) + taupp * vx(i)/v0(i)
				edy(i) = cos(thetarot) + taupp * vy(i)/v0(i)
			else
				edx(i) = zedx(dest(i)) + taupp * vx(i)/v0(i)
				edy(i) = zedy(dest(i)) + taupp * vy(i)/v0(i)
			end if
			else
				edx(i) = zedx(dest(i)) + taupp * vx(i)/v0(i)
				edy(i) = zedy(dest(i)) + taupp * vy(i)/v0(i)
			end if

				
		else if (iz /= 0) then
			edx(i) = -zedx(iz) + taupp * vx(i)/v0(i)
			edy(i) = -zedy(iz) + taupp * vy(i)/v0(i)
		end if
		ud = (/edx(i), edy(i)/)
		edx(i) = edx(i)/norm(ud)
		edy(i) = edy(i)/norm(ud)
		dedxp(i) = (1.0 / taup) * (edx(i) - edxp(i))
		dedyp(i) = (1.0 / taup) * (edy(i) - edyp(i))
		edxp(i) = edxp(i) + dedxp(i) * tpaso
		edyp(i) = edyp(i) + dedyp(i) * tpaso
		ud = (/edxp(i), edyp(i)/)
		edxp(i) = edxp(i) / norm(ud)
		edyp(i) = edyp(i) / norm(ud)
		fdx(i) = (1.0 / tau) * (v0(i) * edxp(i) - vx(i))
		fdy(i) = (1.0 / tau) * (v0(i) * edyp(i) - vy(i))
		!obstaculos
		call ocercano(x(i), y(i), vx(i), vy(i), ox, oy, otrovato, ic)
		if (otrovato) then
			fov = fo(x(i), y(i), ox(ic), oy(ic))
			fox(i) = fov(1)
			foy(i) = fov(2)
		else
			fox(i) = 0.0
			foy(i) = 0.0
		end if
		!interaccion con otros individuos
		fevx(i) = 0.0
		fevy(i) = 0.0
		do j = 1, N
			if (j == i) then
				cycle
			end if
			if (dentrocv(x(j), y(j), x(i), y(i), vx(i), vy(i)) == 1) then
				call fev(x(i), y(i), x(j), y(j), vx(i), vy(i), vx(j), vy(j), fevv)
				fevx(i) = fevx(i) + 0.5 * fevv(1)
				fevy(i) = fevy(i) + 0.5 * fevv(2)
			else if (dentrocv(x(j), y(j), x(i), y(i), vx(i), vy(i)) == 2) then
				call fev(x(i), y(i), x(j), y(j), vx(i), vy(i), vx(j), vy(j), fevv)
				fevx(i) = fevx(i) + fevv(1)
				fevy(i) = fevy(i) + fevv(2)
			end if
		end do
		!la puuuuttttaa rotonda
		if (d(x(i), y(i), 0.0, 0.0) < 3.0) then
			thetarot = atan2(y(i), x(i))
			frotx(i) = frot0 * (v0(i) * (-sin(thetarot)) - vx(i))
			froty(i) = frot0 * (v0(i) * cos(thetarot) - vy(i))
		else 
			frotx(i) = 0.0
			froty(i) = 0.0
		end if
		!fluctuaciones
		call ffluc(vx(i), vy(i), fflu)
		fflux(i) = fflu(1)
		ffluy(i) = fflu(2)
	end do
	!actualizar velocidades
	vx = vx + (fdx + fevx + fflux + fox + frotx) * tpaso
	vy = vy + (fdy + fevy + ffluy + foy + froty) * tpaso
	do i = 1, N
		if ((norm((/vx(i), vy(i)/))) > v0max(i)) then
			vx(i) = vx(i) * (v0max(i))/(norm((/vx(i),vy(i)/)))
			vy(i) = vy(i) * (v0max(i))/(norm((/vx(i),vy(i)/))) 
		end if
	end do
	x = x + vx * tpaso
	y = y + vy * tpaso
	do i = 1, N
	if (entrado(i)) then
		if (x(i) >= xmax) then
			x(i) = x(i) - xmax + xmin
			dest(i) = mdest(tp(i),3)
		else if (x(i) <= xmin) then
			x(i) = x(i) - xmin + xmax
			dest(i) = mdest(tp(i),1)
		else if (y(i) >= ymax) then
			y(i) = y(i) - ymax + ymin
			dest(i) = mdest(tp(i),4)
		else if (y(i) <= ymin) then
			y(i) = y(i) + ymax - ymin
			dest(i) = mdest(tp(i),2)
		end if
	else if (.not. entrado(i)) then
		if ((x(i) > xmin) .and. (x(i) < xmax) .and. (y(i) > ymin) .and. (y(i) < ymax)) then
			entrado(i) = .true.
		end if
	end if
	end do
	!calculo flujos
	if ((.false.) .and. (h > (topeh-300))) then
		flj = 0.0
		rhot = 0.0
		vn = (/1.0, 0.0/)
		do iv = 0, npasov
			xv = 5.0
			yv = 8.0/npasov * iv - 4.0
			call dvor(xv, yv, x, y, vx, vy, 2.0, vn, 1, rho, vc)
			rhot = rhot + rho / (npasov)
			flj = flj + ((rho * dot_product(vc, vn))/(npasov))
		end do
		!vn = (/0.0, 1.0/)
		!do iv = 0, npasov
		!	yv = 6.0
!			xv = 8.0/npasov * iv - 4.0
!			call dvor(xv, yv, x, y, vx, vy, 2.0, vn, 1, rho, vc)
!			rhot = rhot + rho / (npasov * 4)
!			flj = flj + ((rho * dot_product(vc, vn))/(npasov * 4))
!		end do		
!		vn = (/-1.0, 0.0/)
!		do iv = 0, npasov
!			xv = -6.0
!			yv = 8.0/npasov * iv - 4.0
!			call dvor(xv, yv, x, y, vx, vy, 2.0, vn, 1, rho, vc)
!			rhot = rhot + rho / (npasov * 4)
!			flj = flj + ((rho * dot_product(vc, vn))/(npasov * 4))
!		end do
!		vn = (/0.0, -1.0/)
!		do iv = 0, npasov
!			yv = -6.0
!			xv = 8.0/npasov * iv - 4.0
!			call dvor(xv, yv, x, y, vx, vy, 2.0, vn, 1, rho, vc)
!			rhot = rhot + rho / (npasov * 4)
!			flj = flj + ((rho * dot_product(vc, vn))/(npasov * 4))
!		end do
	end if
	!calculo del rotacional con voronoi
	if ((.false.) .and. (h/500*500 == h)) then
		xvmax = 4.0
		xvmin = -4.0
		yvmin = -4.0
		yvmax = 4.0
		rhva = 0.0 * rhva
		rhvb = 0.0 * rhvb
		rhvc = 0.0 * rhvc
		rhvd = 0.0 * rhvd
		do iv = 0, npasov
			xv = 4.0/npasov * iv
			yv = 1.0
!			call dvor(xv, yv, x, y, vx, vy, 2.0, 0, rho, vc)
			rhva = rhva + rho * vc / npasov
		end do
		!do iv = 0, npasov
		!	xv = 4.0/npasov * iv + xvmin
		!	yv = 0.0
!		!	call dvor(xv, yv, x, y, vx, vy, 2.0, 0, rho, vc)
		!	rhvc = rhvc + rho * vc / npasov
		!end do
		!do iv = 0, npasov
		!	xv = 0.0
		!	yv = 4.0/npasov * iv 
!		!	call dvor(xv, yv, x, y, vx, vy, 2.0, 0, rho, vc)
		!	rhvb = rhvb + rho * vc / npasov
		!end do
		!do iv = 0, npasov
		!	xv = 0.0
		!	yv = 4.0/npasov * iv + yvmin
!		!	call dvor(xv, yv, x, y, vx, vy, 2.0, 0, rho, vc)
		!	rhvd = rhvd + rho * vc / npasov
		!end do
		rot = (rhva(2) - rhvc(2))/4.0 - (rhvb(1) - rhvd(1))/4.0
	end if
	!escribir datos
	!posicion + velocidad
	if (.true. .and. (h > (topeh-200)) .and. (h == h)) then
		do i = 1, N
			write(unit=2, fmt=*) x(i), y(i), dest(i), vx(i), vy(i), edxp(i), edyp(i), edx(i), edy(i)
		end do
		write(unit=2, fmt=*)
		write(unit=2, fmt=*)
	end if
	if (.false. .and. (h >= 0) .and. (h/500*500 == h)) then
		do i = 1, N
			write(unit=3, fmt=*) x(i), y(i), dest(i), vx(i), vy(i)
		end do
		write(unit=3, fmt=*)
		write(unit=3, fmt=*)
	end if
	!fundamental diagrama
	if ((h > (topeh - 300)) .and. (.false.)) then
		write(unit=1, fmt=*) h, rhot, flj
	end if
end do

!escribir ultimo paso
if (.false.) then
	do i = 1, N
		write(unit=1, fmt=*) x(i), y(i), edx(i), vy(i)
	end do
	write(unit=1, fmt=*)
	write(unit=1, fmt=*)
end if

close(unit=1)
close(unit=2)
end program i1
