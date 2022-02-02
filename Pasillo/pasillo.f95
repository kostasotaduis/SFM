program i1

use util
use voronoi
use cv
use fobs
use fij
use fluc

real, dimension(:), allocatable :: x, y, vx, vy
real, dimension(:), allocatable :: xd, yd, edx, edy, fdx, fdy, v0, v0max
real :: tau, sigma0, deltav0
real :: tpaso
integer :: h, k, kk
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
integer :: npasov, iv, nkkt, topeh, medh
real, dimension(2) :: vc
!energias
real, dimension(:), allocatable :: U
real :: medU, medek
real, dimension(:), allocatable :: meddU, meddek

open (unit=0, file="pdata.dat", status="old", action="read")
rewind(unit=0)
read (unit=0, fmt=*) ymin, ymax, dns, ftheta0
close(unit=0)



!pasillo
xmax = 25.0
xmin = -25.0
ymin = -3.8
ymax = 3.8
dns = 0.5

N = nint((ymax - ymin) * (xmax - xmin) * dns)

!parametros simulacion
tpaso = 0.05
call random_seed()

!numero de individuos
!N = 200

!parametros individuo
sigma0 = 0.26
v000 = 1.34

rcv = 3.0
thetacv = 200 * pi / 180.0

!fluctuaciones
sigmafluc = 0.7
ftheta0 = 0.0


tau = 0.5

!parametros obstaculos
ao = 10.0
oR = 0.2
allocate(fov(2))
open(unit=0, file="opasillo.dat", status="old", action="read")
rewind(unit=0)
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

!parametros voronoi
yvmin = -2.5
yvmax = 2.5
npasov = 10

!condiciones iniciales
!abrir archivo para escribir datos
open (unit=1, file="pasillo.dat", status="replace", action="write")
open (unit=2, file="pasillo2.dat", status="replace", action="write")
open (unit=3, file="pasillo3.dat", status="replace", action="write")
allocate(meddU(100), meddek(100))

!escribir primer paso (poner false si no se quiere

if (.false.) then
	do i = 1, N
		write(unit=1, fmt=*) x(i), y(i), vx(i), vy(i)
	end do
	write(unit=1, fmt=*)
	write(unit=1, fmt=*)
end if

do k = 0, 5
dns = 0.2 + k * 0.1
N = nint((ymax - ymin) * (xmax - xmin) * dns)
allocate(x(N), y(N), vx(N), vy(N))
allocate(edx(N), edy(N), fdx(N), fdy(N))
allocate(yd(N), xd(N), v0(N), v0max(N))
allocate(fevx(N), fevy(N))
allocate(fflux(N), ffluy(N))
allocate(fox(N), foy(N))
allocate(U(N))

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

do i = 1, N
	call gaus(sigma0, deltav0)
	v0(i) = v000 + deltav0
	v0max(i) = 1.3 * v0(i)
end do


flj = 0.0
meddU = flj * meddU
meddek = flj * meddek
nkkt = 2
do kk = 1, nkkt
!condiciones iniciales
do i = 1, N
	call random_number(xr)
	x(i) = xr * (xmax - xmin) + xmin
	call random_number(xr)
	y(i) = xr * (ymax - ymin) + ymin
	vx(i) = 0.0
	vy(i) = 0.0
end do



!iteracion
topeh = 300
medh = topeh/size(meddU)
do h = 1, topeh
	!sacar las fuerzas de todos
	medU = 0.0
	medek = 0.0
	U = U * 0.0
	do i = 1, N
		!velocidad deseada
		edx(i) = (xd(i) - x(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
		edy(i) = (yd(i) - y(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
		fdx(i) = (1.0 / tau) * (v0(i) * edx(i) - vx(i))
		fdy(i) = (1.0 / tau) * (v0(i) * edy(i) - vy(i))
		!obstaculos
		call ocercano(x(i), y(i), vx(i), vy(i), ox, oy, otrovato, ic)
		if (otrovato) then
			fov = fo(x(i), y(i), ox(ic), oy(ic))
			fox(i) = fov(1)
			foy(i) = fov(2)
			U(i) = U(i) + uo(x(i), y(i), ox(ic), oy(ic))
		else
			fox(i) = 0.0
			foy(i) = 0.0
		end if
!print*, "hey"
		!interaccion con otros individuos
		fevx(i) = 0.0
		fevy(i) = 0.0
		do j = 1, N
			if (dentrocv(x(j), y(j), x(i), y(i), vx(i), vy(i))) then
				call fev(x(i), y(i), x(j), y(j), vx(i), vy(i), vx(j), vy(j), fevv)
				fevx(i) = fevx(i) + fevv(1)
				fevy(i) = fevy(i) + fevv(2)
				U(i) = U(i) + uev(x(i), y(i), x(j), y(j), vx(j), vy(j))
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
	medek = 0.0
	do i = 1, N
		if ((norm((/vx(i), vy(i)/))) > v0max(i)) then
			vx(i) = vx(i) * (v0max(i))/(norm((/vx(i),vy(i)/)))
			vy(i) = vy(i) * (v0max(i))/(norm((/vx(i),vy(i)/)))
		end if
		U(i) = U(i) + ek(vx(i), vy(i))
		medek = medek + ek(vx(i), vy(i))
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
	!sacar energia media
	medU = 0.0
	do i = 1, N
		medU = medU + U(i)
	end do
	medek = medek / N
	medU = medU / N
	!calculo de densidad/flujo con voronoi (punto/region a elegir)
	if (.false. .and. h > 3000) then
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
				!call dvor(xv, yv, x, y, vx, vy, 2.0, 0, ro, vc)
				!rot = rot + ro
				!flj = flj + (ro * abs(vc(1)) * (yvmax - yvmin)/npasov)
		end do
		rot = rot / npasov
		rod = rod / npasov
		roi = roi / npasov
		flji = flji / (ymax - ymin)
		fljd = fljd / (ymax - ymin)
		flj = flj / (ymax - ymin)
	end if
	!escribir datos
	!posicioes y velocidades
	if (.true. .and. (h > 0) .and. (h/5*5 == h)) then
		do i = 1, N
			write(unit=2, fmt=*) x(i), y(i), vx(i), vy(i)
		end do
		write(unit=2, fmt=*)
		write(unit=2, fmt=*)
	end if
	!densidad y flujo u otras cosas
	if ((h/10*10 == h) .and. (.true.)) then
		write(unit=1, fmt=*) h*tpaso, medU, medek
	end if
	if ((h/medh*medh == h) .and. (.true.)) then
		meddU(h/medh) = meddu(h/medh) + medU
		meddek(h/medh) = meddek(h/medh) + medek
	end if
end do
end do
meddu = meddU/nkkt
meddek = meddek/nkkt
do h = 1, 100
	write(unit=3, fmt=*) dns, h*10*tpaso, meddU(h), meddek(h)
end do
write(unit=3, fmt=*)
write(unit=3, fmt=*)
deallocate(x, y, vx, vy)
deallocate(edx, edy, fdx, fdy)
deallocate(yd, xd, v0, v0max)
deallocate(fevx, fevy)
deallocate(fflux, ffluy)
deallocate(fox, foy)
deallocate(U)


end do

!escribir ultimo paso
if (.false.) then
	do i = 1, N
		write(unit=1, fmt=*) x(i), y(i), vx(i), vy(i)
	end do
	write(unit=1, fmt=*)
	write(unit=1, fmt=*)
end if

close(unit=1)
end program i1
