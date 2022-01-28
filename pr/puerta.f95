program i1

use util
use voronoi
use cv
use fobs
use fij
use fluc

real, dimension(:), allocatable :: x, y, vx, vy
real, dimension(:), allocatable :: xd, yd, edx, edy, fdx, fdy, v0, v0max
real, dimension(:), allocatable :: px, py
real :: rp
integer :: nc
real :: tau, sigma0, deltav0
real :: tpaso
integer :: h
real, dimension(:), allocatable :: ox, oy, fov
real, dimension(:), allocatable :: fox, foy
integer :: nlineas, status, ic
logical :: otrovato
real, dimension(2) :: fevv
real, dimension(:), allocatable :: fevx, fevy
integer :: N, i, j
real, dimension(2) :: fflu
real, dimension(:), allocatable :: fflux, ffluy
real :: xmax, xmin, ymax, ymin, xr, dns
!per voronoi
real :: yvmin, yvmax, xv, yv, ro, roi, rod, rot
real :: flji, fljd, flj
integer :: npasov, iv
real, dimension(2) :: vc
character(len=3) :: cont

!parametros simulacion
tpaso = 0.2
call random_seed()

!pasillo
xmax = 25.0
xmin = -25.0
ymin = -7.8
ymax = 7.8


!puertas
!dos puertas
	!allocate(px(2), py(2))
	!px = (/0.0, 0.0/)
	!py = (/1.75, -1.75/)
	!rp = 0.5
!una puerta
	allocate(px(1), py(1))
	px = (/0.0/)
	py = (/0.0/)
	rp = 1.4



!numero de individuos
dns = 0.4
N = dns * (xmax - xmin) * (ymax - ymin)
allocate(x(N), y(N), vx(N), vy(N))
allocate(edx(N), edy(N), fdx(N), fdy(N))
allocate(yd(N), xd(N), v0(N), v0max(N))
allocate(fevx(N), fevy(N))
allocate(fflux(N), ffluy(N))
allocate(fox(N), foy(N))

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
ftheta0 = 0.01

!parametros objetivo(dESTINO)
do i = 1, N
	call random_number(xr)
	if (xr >= 0.5) then
		xd(i) = 1000.0
	else if (xr < 0.5) then
		xd(i) = -1000.0
	end if
	yd(i) = 0.0
end do
tau = 0.5

!parametros obstaculos
ao = 10.0
oR = 0.2
allocate(fov(2))
open(unit=0, file="opuerta.dat", status="old", action="read")
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


!parametros voronoi
yvmin = -1.4
yvmax = 1.4
npasov = 5

!condiciones iniciales
do i = 1, N
	call random_number(xr)
	if (xr < 0.5) then
		x(i) = 2 * xr * (xmax - 1.0) + 2.0 - xmax
	else if (xr >= 0.5) then
		x(i) = 2 * xr * (xmax - 1.0) - xmax
	end if	
	call random_number(xr)
	y(i) = xr * (ymax - ymin) + ymin
	vx(i) = 0.0
	vy(i) = 0.0
end do

!abrir archivo para escribir datos
open (unit=1, file="puerta.dat", status="replace", action="write")
open (unit=2, file="puerta2.dat", status="replace", action="write")

!escribir primer paso (poner false si no se quiere

if (.false.) then
	do i = 1, N
		write(unit=1, fmt=*) x(i), y(i), vx(i), vy(i)
	end do
	write(unit=1, fmt=*)
	write(unit=1, fmt=*)
end if
!iteracion
do h = 0, 2200
	!sacar las fuerzas de todos
	do i = 1, N
		!velocidad deseada
			!primero que esten despues de la puerta
		if ((xd(i)*x(i)) >= 0.0) then
			edx(i) = (xd(i) - x(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
			edy(i) = (yd(i) - y(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
		else if ((xd(i)*x(i)) < 0.0) then
			call cercano(x(i), y(i), px, py, nc)
			if ((d(x(i), y(i), px(nc), py(nc))) < rp) then
				edx(i) = (xd(i) - x(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
				edy(i) = (yd(i) - y(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
			else
				edx(i) = (px(nc) - x(i)) / (norm((/(px(nc)-x(i)), (py(nc)-y(i))/)))
				edy(i) = (py(nc) - y(i)) / (norm((/(px(nc)-x(i)), (py(nc)-y(i))/)))
			end if
		end if			
		fdx(i) = (1.0 / tau) * (v0(i) * edx(i) - vx(i))
		fdy(i) = (1.0 / tau) * (v0(i) * edy(i) - vy(i))
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
			if (dentrocv(x(j), y(j), x(i), y(i), vx(i), vy(i))) then
				call fev(x(i), y(i), x(j), y(j), vx(j), vy(j), fevv)
				fevx(i) = fevx(i) + fevv(1)
				fevy(i) = fevy(i) + fevv(2)
			end if
		end do
		!fluctuaciones
		call ffluc(vx(i), vy(i), fflu)
		fflux(i) = fflu(1)
		ffluy(i) = fflu(2)
	end do
	!actualizar velocidades
	vx = vx + (fdx + fevx + fflux + fox) * tpaso
	vy = vy + (fdy + fevy + ffluy + foy) * tpaso
	do i = 1, N
		if ((norm((/vx(i), vy(i)/))) > v0max(i)) then
			vx(i) = vx(i) * (v0max(i))/(norm((/vx(i),vy(i)/)))
			vy(i) = vy(i) * (v0max(i))/(norm((/vx(i),vy(i)/)))
		end if
	end do
	x = x + vx * tpaso
	do i = 1, N
		if (x(i) >= xmax) then
			x(i) = x(i) - xmax + xmin
		else if (x(i) <= xmin) then
			x(i) = x(i) - xmin + xmax
		end if
	end do
	y = y + vy * tpaso
	!calculo de densidad/flujo con voronoi (punto/region a elegir)
	if ((h > 1500) .and. (.true.)) then
		rot = 0.0
		roi = 0.0
		rod = 0.0
		fljd = 0.0
		flji = 0.0
		flj = 0.0
		do iv = 0, npasov
				xv = 0.0
				yv = (yvmax - yvmin)/npasov * iv + yvmin
				call dvor(xv, yv, x, y, vx, vy, 2.0, -1, ro, vc)
				roi = roi + ro
				flji = flji + (ro * abs(vc(1)) * (yvmax - yvmin)/npasov)
				call dvor(xv, yv, x, y, vx, vy, 2.0, 1, ro, vc)
				rod = rod + ro
				fljd = fljd + (ro * abs(vc(1)) * (yvmax - yvmin)/npasov)
				call dvor(xv, yv, x, y, vx, vy, 2.0, 0, ro, vc)
				rot = rot + ro
				flj = flj + (ro * abs(vc(1)) * (yvmax - yvmin)/npasov)
		end do
		rot = rot / npasov
		rod = rod / npasov
		roi = roi / npasov
		flji = flji / (yvmax - yvmin)
		fljd = fljd / (yvmax - yvmin)
		flj = flj / (yvmax - yvmin)
	end if
	!escribir datos
	!posicion + velocidad
	if (.true. .and. (h > 2000)) then
		do i = 1, N
			write(unit=2, fmt=*) x(i), y(i), edx(i), vx(i), vy(i)
		end do
		write(unit=2, fmt=*)
		write(unit=2, fmt=*)
	end if
	!fundamental diagrama
	if ((h > 1500) .and. (.true.)) then
		write(unit=1, fmt=*) h*tpaso, roi, rod, rot, flji, fljd
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
