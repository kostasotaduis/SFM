program prueba

use util
use cv
use fij


real, dimension(2) :: r1, r2, v1, v2
integer :: i, j, n
real :: xmin, xmax, ymin, ymax, bb


xmin = -2
xmax = 2
ymin = -2
ymax = 2

r2 = (/0.0, 0.0/)
v2 = (/1.0, 0.0/)

deltapaso = 1.0
sigma = 0.3
fev0 = 2.1

n = 50

open (unit=0, file="prueba.dat", status="replace", action="write")

do i = 0, n
	do j = 0, n
		r1(1) = i * (xmax - xmin) / n + xmin
		r1(2) = j * (ymax - ymin) / n + ymin
		bb = b((r1 - r2), v2)
		write(unit=0, fmt=*) r1(1), r1(2), e**(-bb/sigma)
	end do
end do


end program prueba
