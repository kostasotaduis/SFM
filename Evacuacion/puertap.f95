program i1

use util
use voronoi
use cv
use fobs
use fij
use fluc

real, dimension(:), allocatable :: x, y, vx, vy
real, dimension(:), allocatable :: xd, yd, edx, edy, fdx, fdy, v0, v0max, rc, m, p, pp
real, dimension(:), allocatable :: px, py
real :: rp, v000, m0
integer :: nc
real :: tau, sigma0, deltav0
real :: tpaso, mt
integer :: h, ndentro, k, kk, nt, nkkt
real, dimension(:), allocatable :: ox, oy, fov
real, dimension(:), allocatable :: fox, foy
integer :: nlineas, status, ic
logical :: otrovato
real, dimension(2) :: fevv, bodyy, fricc
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
tpaso = 0.005
call random_seed()

!habitación
xmax = -2.0
xmin = -14.0
ymin = -7.5
ymax = 7.5


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
	rp = 0.5



!numero de individuos
dns = 0.4
!N = dns * (xmax - xmin) * (ymax - ymin)
N = 200
allocate(x(N), y(N), vx(N), vy(N))
allocate(edx(N), edy(N), fdx(N), fdy(N))
allocate(yd(N), xd(N), v0(N), v0max(N), rc(N), m(N), p(N))
allocate(fevx(N), fevy(N))
allocate(fflux(N), ffluy(N))
allocate(fox(N), foy(N))

!parametros individuo
sigma0 = 0.26
v000 = 0.7
!do i = 1, N
!	call gaus(sigma0, deltav0)
!	v0(i) = v000 + deltav0
!	v0max(i) = 1.3 * v0(i)
!end do
rcv = 3.0
m0 = 70.0
do i = 1, N
	call random_number(deltav0)
	rc(i) = 0.25 + deltav0 * 0.1
	m(i) = m0 + deltav0 * 20 - 10.0
	call gaus(1.0, deltav0)
	m(i) = m(i) + deltav0
end do	
thetacv = 360 * pi / 180.0


!fluctuaciones
sigmafluc = 0.7
ftheta0 = 7.0

!parametros objetivo(dESTINO)
do i = 1, N
	call random_number(xr)
	if (xr >= 0.5) then
		xd(i) = 1000
	else if (xr < 0.5) then
		xd(i) = 1000
	end if
	yd(i) = 0.0
end do
tau = 0.5

!parametros obstaculos
ao = 7.0
oR = 0.15
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
sigma = 0.08
fev0 = 2E3 / m0
kappaf = 2.4E5
kappab = 1.2E5



!parametros voronoi
yvmin = -0.5
yvmax = 0.5
npasov = 5


!abrir archivo para escribir datos
open (unit=1, file="puertap.dat", status="replace", action="write")
open (unit=2, file="puertap2.dat", status="replace", action="write")
open (unit=3, file="tvv.dat", status="replace", action = "write")
open (unit=4, file="puertap3.dat", status="replace", action = "write")
open (unit=8, file="puertap4.dat", status="replace", action = "write")
!escribir primer paso (poner false si no se quiere

if (.false.) then
	do i = 1, N
		write(unit=2, fmt=*) x(i), y(i), vx(i), vy(i)
	end do
	write(unit=2, fmt=*)
	write(unit=2, fmt=*)
end if
v000 = 0.34
!iteracion
allocate(pp(501))
pp = 0.0 * pp
do k = 0, 0
v000 = 5.0 + k * 0.4
do i = 1, N
	call gaus(sigma0, deltav0)
	v0(i) = v000 + deltav0
	v0max(i) = 1.3 * v0(i)
end do
!condiciones iniciales

mt = 0.0
nt = 0
pp = 0.0 * pp
nkkt = 1
do kk = 1, nkkt
do i = 1, N
	do
		call random_number(xr)
		x(i) = xr * (xmax - xmin - 0.4) + xmin + 0.2
		if (x(i) < -4.8) then
			exit
		else if (x(i) > -4.2 .and. x(i) < -3.3) then
			exit
		else if (x(i) > -2.7 .and. x(i) < 0.0) then
			exit
		end if
	end do

	call random_number(xr)
	y(i) = xr * (ymax - ymin - 0.4) + ymin + 0.2
	vx(i) = 0.0
	vy(i) = 0.0
	edx(i) = 1.0
	edy(i) = 0.0
end do
do h = 1, 5000
	!sacar las fuerzas de todos
!print*, "he  y"
	p = 0.0 * p
	do i = 1, N
!print*, "hey11"
		!velocidad deseada
			!primero que esten despues de la puerta
		if ((xd(i)*x(i)) > 0.0) then
			edx(i) = (xd(i) - x(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
			edy(i) = (yd(i) - y(i)) / (norm((/(xd(i)-x(i)), (yd(i)-y(i))/)))
		else if ((xd(i)*x(i)) <= 0.0) then
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
!print*, "hey13"
		!obstaculos
		fox(i) = 0.0
		foy(i) = 0.0
		fov = 0.0 * fov
		call ocercano(x(i), y(i), vx(i), vy(i), ox, oy, otrovato, ic)
		if (otrovato) then
			fov = fo(x(i), y(i), ox(ic), oy(ic))
			fox(i) = fox(i) + fov(1)
			foy(i) = foy(i) + fov(2)
		end if
!print*, "hey16"
		do j = 1, size(ox)
			if (d(x(i), y(i), ox(j), oy(j)) <= rc(i)) then
				bodyy = 0.0 * bodyy
				fricc = 0.0 * fricc
				call body(x(i), y(i), ox(j), oy(j), rc(i), 0.0, bodyy)
				call fric(x(i), y(i), ox(j), oy(j), vx(i), vy(i), 0.0, 0.0, rc(i), 0.0, fricc)
				fox(i) = fox(i) + bodyy(1)/m(i) + fricc(1)/m(i)
				foy(i) = foy(i) + bodyy(2)/m(i) + fricc(2)/m(i)
				p(i) = p(i) + norm(bodyy)
			end if
		end do
!print*, "hey2"
		!interaccion con otros individuos
		fevx(i) = 0.0
		fevy(i) = 0.0
		do j = 1, N
			if (j == i) then
				cycle
			end if
			if (dentrocv(x(j), y(j), x(i), y(i), vx(i), vy(i)) == 1) then
				call fev(x(i), y(i), x(j), y(j), rc(i), rc(j), fevv)
				fevx(i) = fevx(i) + 0.5 * fevv(1)
				fevy(i) = fevy(i) + 0.5 * fevv(2)
			else if (dentrocv(x(j), y(j), x(i), y(i), vx(i), vy(i)) == 2) then
				call fev(x(i), y(i), x(j), y(j), rc(i), rc(j), fevv)
				fevx(i) = fevx(i) + fevv(1)
				fevy(i) = fevy(i) + fevv(2)
			end if
			if (d(x(j), y(j), x(i), y(i)) <= (rc(i) + rc(j))) then
				call body(x(i), y(i), x(j), y(j), rc(i), rc(j), bodyy)
				call fric(x(i), y(i), x(j), y(j), vx(i), vy(i), vx(j), vy(j), rc(i), rc(j), fricc)
				fevx(i) = fevx(i) + bodyy(1)/m(i) + fricc(1)/m(i)
				fevy(i) = fevy(i) + bodyy(2)/m(i) + fricc(2)/m(i)
				p(i) = p(i) + norm(bodyy)
!print*, "hubo bodys"
			end if
		end do
!print*, "hey25"
		!fluctuaciones
		call ffluc(fflu)
		fflux(i) = fflu(1)
		ffluy(i) = fflu(2)
		if ((p(i) > 5000.0) .and. (.true.)) then
			p(i) = 5000.0
		end if
!print*, "F(", i, "): ", fdx(i), fevx(i), fflux(i), fox(i)
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
  	y = y + vy * tpaso
!print*, "hey3"
	!calculo de densidad/flujo con voronoi (punto/region a elegir)
	if ((h/20*20 == h) .and. (.false.)) then
		rot = 0.0
		roi = 0.0
		rod = 0.0
		fljd = 0.0
		flji = 0.0
		flj = 0.0
		do iv = 0, npasov
				xv = 0.0
				yv = (yvmax - yvmin)/npasov * iv + yvmin
				call dvor(xv, yv, x, y, vx, vy, 1.0, 0, ro, vc)
				rot = rot + ro
				flj = flj + (ro * vc(1) * (yvmax - yvmin)/npasov)
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
	if (.true. .and. (h/20*20 == h)) then
		do i = 1, N
			write(unit=2, fmt=*) x(i), y(i), vx(i), vy(i), rc(i), p(i)
		end do
		write(unit=2, fmt=*)
		write(unit=2, fmt=*)
	end if
	!fundamental diagrama
	if ((h/20*20  == h) .and. (h > 0) .and. (.true.)) then
		pp(h/20) = pp(h/20) + sum(p)/(N*nkkt)
!		write(unit=1, fmt=*) h*tpaso, sum(p)/N, maxval(p)
		write(unit=1, fmt=*) h*tpaso, rot, flj
	end if
	!contar gente en la hab
	ndentro = 0
	do i = 1, N
		if ((x(i) >= -15.0) .and. (x(i) <= 0.0) .and. (y(i) >= -7.5) .and. (y(i) <= 7.5)) then
			ndentro = ndentro + 1
		end if
	end do
	if (ndentro < 20) then
		if (h<69999) then
			mt = mt + h*tpaso
			nt = nt + 1
!			print*, h*tpaso
		end if
		exit
	end if
end do
write(unit=1, fmt=*)
write(unit=1, fmt=*)
end do
!print*, mt, nt
write(unit=3, fmt=*) v000, (mt/nt)
if (.false.) then
do h = 1, size(pp)
	write(unit=4, fmt=*) h*20*tpaso, pp(h)
end do
write(unit=4, fmt=*)
write(unit=4, fmt=*)
write(unit=8, fmt=*) v000, sum(pp(50:450))/400
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
