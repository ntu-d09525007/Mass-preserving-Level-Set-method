function find_pos(iV,iM1,iM2,iM3,ix1,ix2,ix3) result(pos)
implicit none
real(8) :: iv,im1,im2,im3,ix1,ix2,ix3
real(8) :: v,x1,x2,x3
real(8) :: eps,m1,m2,m3,m,m12,mag
real(8) :: v1,v2,v31,v32,v3
real(8) :: alpha, pos
logical :: reverse
    
m1 = min(iM1,iM2,iM3)
m3 = max(iM1,iM2,iM3)
m2 = iM1+iM2+iM3-m1-m3

mag=dsqrt(m1*m1+m2*m2+m3*m3)

m1=m1/mag
m2=m2/mag
m3=m3/mag

eps=1.0d-12

if(abs(m1-im1)<eps)then
	x1=ix1
else if (abs(m1-im2)<eps)then
	x1=ix2
else
	x1=ix3
endif

if(abs(m2-im1)<eps)then
	x2=ix1
else if (abs(m2-im2)<eps)then
	x2=ix2
else
	x2=ix3
endif

if(abs(m3-im1)<eps)then
	x3=ix1
else if (abs(m3-im2)<eps)then
	x3=ix2
else
	x3=ix3
endif

if( v > 0.5 ) then
    reverse=.true.
    v=1.0d0-iv
else
    reverse=.false.
    v=iv
endif

m12=m1+m2
m=min(m12,m3)

V1=m1**2.0d0/max(6.0d0*m2*m3,eps)
V2=V1+(m2-m1)/(2.0d0*m3)
V31=(m3**2.0d0*(3.0d0*m12-m3)+m1**2.0d0*(m1-3.0d0*m3)+m2**2.0d0*(m2-3.0d0*m3))/(6.0d0*m1*m2*m3)
V32=m12/(2.0d0*m3)

if ( abs(m-m3)<eps ) then
    V3=V31
else
    V3=V32
endif

if (V1 > V) then
    alpha=(6.0d0*m1*m2*m3*V)**(1.0d0/3.0d0)
else if (V1<=V .and. V<V2)then
    alpha=0.5*(m1+dsqrt(m1**2.0d0+8.0d0*m2*m3*(V-V1)))
else if (V2<=V .and. V<V3)then
    alpha = cubic_root(-1.0d0, 3.0d0*m12, -3.0d0*(m1**2.0d0+m2**2.0d0), m1**3.0d0+m2**3.0d0-6.0d0*m1*m2*m3*V)
else
    if (abs(V3-V31)<eps) then
        alpha = cubic_root(-2.0d0, 3.0d0, -3.0d0*(m1**2.0d0+m2**2.0d0+m3**2.0d0), m1**3.0d0+m2**3.0d0+m3**3.0d0-6.0d0*m1*m2*m3*V)
    else
        alpha = m3*V + m12/2.0d0
    endif
endif

if( x1 < eps )then
    pos = (alpha-m2*x2-m3*x3)/m1
else if ( x2 < eps )then
    pos = (alpha-m1*x1-m3*x3)/m2
else
    pos = (alpha-m1*x1-m2*x2)/m3
endif

if(reverse)pos=1.0d0-pos

end function

function cubic_root(ia3,ia2,ia1,ia0) result(y)
implicit none
real(8) :: ia0,ia1,ia2,ia3
real(8) :: a0,a1,a2,y
real(8) :: p,q,theta

a2=ia2/ia3
a1=ia1/ia3
a0=ia0/ia3

p=a1/3.0d0-a2**2.0d0/9.0d0
q=(a1*a2-3.0d0*a0)/6.0d0-a2**3.0d0/27.0d0

theta = dacos(q/dsqrt(-p**3.0d0))/3.0d0

y = dsqrt(-p)*(dsqrt(3.0d0)*dsin(theta)-dcos(theta))-a2/3.0d0

end function
