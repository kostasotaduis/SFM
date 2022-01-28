program i1

use util
use cv
use fobs
use fij
use fluc

real, dimension(:), allocatable :: x, y, vx, vy
real, dimension(:), allocatable :: xd, yd, edx, edy, fdx, fdy
real :: v0, tau, sigma0, deltav0, v0max
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
real :: xmax, xmin, ymax, ymin, xr

!parametros simulacion
tpaso = 0.2
call random_seed()

!numero de individuos
N = 100
allocate(x(N), y(N), vx(N), vy(N))
allocate(edx(N), edy(N), fdx(N), fdy(N))
allocate(yd(N), xd(N))
allocate(fevx(N), fevy(N))
allocate(fflux(N), ffluy(N))
allocate(fox(N), foy(N))

!parametros individuo
sigma0 = 0.26 ** 2.0
v0max = 1.5
do i = 1, N
	call gaus(sigma0, deltav0)
	v0 = 1.34 + deltav0
end do
rcv = 3.0
thetacv = 200 * pi / 180.0

!fluctuaciones
sigmafluc = 0.7
ftheta0 = 0.3

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
open(unit=0, file="obspasillo.dat", status="old", action="read")
nlineas = 0
do
	read (unit = 0, fmt=*, iostat=status)
	if (status < 0) then
		exit
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

!pasillo
xmax = 25.0
xmin = -25.0
ymin = -4.5
ymax = 4.5

!condiciones iniciales
do i = 1, N
	call random_number(xr)
	x(i) = xr * (xmax - xmin) + xmin
	call random_number(xr)
	y(i) = xr * (ymax - ymin) + ymin
	vx(i) = 0.0
	vy(i) = 0.0
end do

!abrir archivo para escribir datos
open (unit=1, file="pasillo.dat", status="replace", action="write")

!escribir primer paso (poner false si no se quiere

if (.false.) then
	do i = 1, N
		write(unit=1, fmt=*) x(i), y(i), vx(i), vy(i)
	end do
	write(unit=1, fmt=*)
	write(unit=1, fmt=*)
end if
!iteracion
do h = 0, 20000
	!sacar las fuerzas de todos
	do i = 1, N
		!velocidad deseada
		edx(i) = (xd(i) - x(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
		edy(i) = (yd(i) - y(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
		fdx(i) = (1.0 / tau) * (v0 * edx(i) - vx(i))
		fdy(i) = (1.0 / tau) * (v0 * edy(i) - vy(i))
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
				fevv = fev(x(i), y(i), x(j), y(j), vx(j), vy(j))
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
		if ((norm((/vx, vy/))) > v0max) then
			vx(i) = vx(i) * (v0max)/(norm((/vx(i),vy(i)/)))
			vy(i) = vy(i) * (v0max)/(norm((/vx(i),vy(i)/)))
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
	!escribir datos
	if (h == 100) then
		do i = 1, N
			write(unit=1, fmt=*) x(i), y(i), vx(i), vy(i)
		end do
		write(unit=1, fmt=*)
		write(unit=1, fmt=*)
	end if
end do

!escribir ultimo paso
if (.true.) then
	do i = 1, N
		write(unit=1, fmt=*) x(i), y(i), vx(i), vy(i)
	end do
	write(unit=1, fmt=*)
	write(unit=1, fmt=*)
end if

close(unit=1)
end program i1
