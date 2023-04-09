subroutine level_set_rk3_solver()
use all
!$ use omp_lib
implicit none
integer :: i,j,k,id
integer(8) :: cpustart, cpuend
real(8) :: src

    call system_clock(cpustart)
    
    call level_set_rk3_source_setup

    call level_set_rk3_source(p%loc%tdata%x%l1)
    
    !$omp parallel do private(src), collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        src = p%loc%tdata%x%l1(i,j,k)
        p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) + src * p%glb%dt
    end do 
    end do 
    end do
    !$omp end parallel do 
    
    call bc(p%loc%phi%now)
    
    call level_set_rk3_source_setup
    
    call level_set_rk3_source(p%loc%tdata%x%l2)
    
    !$omp parallel do private(src), collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        src = ( -3.0d0*p%loc%tdata%x%l1(i,j,k) + p%loc%tdata%x%l2(i,j,k) ) / 4.0d0
        p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) + src * p%glb%dt
    end do 
    end do 
    end do
    !$omp end parallel do 

    call bc(p%loc%phi%now)
    
    call level_set_rk3_source_setup
 
    call level_set_rk3_source(p%loc%tdata%x%l3)
    
    !$omp parallel do private(src), collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        src = ( -p%loc%tdata%x%l1(i,j,k)-p%loc%tdata%x%l2(i,j,k)+8.0d0*p%loc%tdata%x%l3(i,j,k) ) / 12.0d0
        p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) + src * p%glb%dt
    end do 
    end do 
    end do
    !$omp end parallel do 

    call bc(p%loc%phi%now)
    
    call system_clock(cpuend)
    p%glb%ls_adv = p%glb%ls_adv + real(cpuend-cpustart,kind=8) / real( p%glb%cpurate, kind=8 )

end subroutine

subroutine level_set_rk3_source_setup
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%tdata%x%s1(i,j,k) = 0.5d0*(p%loc%nvel%x%old(i,j,k)+abs(p%loc%nvel%x%old(i,j,k)))*p%loc%phi%now(i,j,k)
    p%loc%tdata%x%s2(i,j,k) = 0.5d0*(p%loc%nvel%x%old(i,j,k)-abs(p%loc%nvel%x%old(i,j,k)))*p%loc%phi%now(i,j,k)

    p%loc%tdata%y%s1(i,j,k) = 0.5d0*(p%loc%nvel%y%old(i,j,k)+abs(p%loc%nvel%y%old(i,j,k)))*p%loc%phi%now(i,j,k)
    p%loc%tdata%y%s2(i,j,k) = 0.5d0*(p%loc%nvel%y%old(i,j,k)-abs(p%loc%nvel%y%old(i,j,k)))*p%loc%phi%now(i,j,k)

    p%loc%tdata%z%s1(i,j,k) = 0.5d0*(p%loc%nvel%z%old(i,j,k)+abs(p%loc%nvel%z%old(i,j,k)))*p%loc%phi%now(i,j,k)
    p%loc%tdata%z%s2(i,j,k) = 0.5d0*(p%loc%nvel%z%old(i,j,k)-abs(p%loc%nvel%z%old(i,j,k)))*p%loc%phi%now(i,j,k)
    
end do
end do
end do 
!$omp end parallel do 

call bc(p%loc%tdata%x%s1);call bc(p%loc%tdata%x%s2)
call bc(p%loc%tdata%y%s1);call bc(p%loc%tdata%y%s2)
call bc(p%loc%tdata%z%s1);call bc(p%loc%tdata%z%s2)

end subroutine 

subroutine level_set_rk3_source(s)
use all
implicit none
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: s
integer :: i,j,k

!$omp parallel do collapse(2)                     
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
    ! call p%loc%ccdsolvers%x%solve("uccd",p%loc%phi%now(:,j,k),p%loc%tdata%x%s1(:,j,k),p%loc%tdata%x%s2(:,j,k),p%loc%nvel%x%old(:,j,k))
    call crweno_flux_split(p%loc%tdata%x%s2(:,j,k),p%loc%tdata%x%s1(:,j,k),&
                          p%loc%tdata%x%ss2(:,j,k),p%loc%tdata%x%ss1(:,j,k),&
                          p%loc%is,p%loc%ie,p%glb%ghc)
end do 
end do
!$omp end parallel do 

!$omp parallel do collapse(2) 
do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie
    ! call p%loc%ccdsolvers%y%solve("uccd",p%loc%phi%now(i,:,k),p%loc%tdata%y%s1(i,:,k),p%loc%tdata%y%s2(i,:,k),p%loc%nvel%y%old(i,:,k))
    call crweno_flux_split(p%loc%tdata%y%s2(i,:,k),p%loc%tdata%y%s1(i,:,k),&
                          p%loc%tdata%y%ss2(i,:,k),p%loc%tdata%y%ss1(i,:,k),&
                          p%loc%js,p%loc%je,p%glb%ghc)
end do 
end do
!$omp end parallel do 

!$omp parallel do collapse(2) 
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    ! call p%loc%ccdsolvers%z%solve("uccd",p%loc%phi%now(i,j,:),p%loc%tdata%z%s1(i,j,:),p%loc%tdata%z%s2(i,j,:),p%loc%nvel%z%old(i,j,:))
    call crweno_flux_split(p%loc%tdata%z%s2(i,j,:),p%loc%tdata%z%s1(i,j,:),&
                          p%loc%tdata%z%ss2(i,j,:),p%loc%tdata%z%ss1(i,j,:),&
                          p%loc%ks,p%loc%ke,p%glb%ghc)
end do 
end do
!$omp end parallel do 

!$omp parallel do collapse(3) 
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    s(i,j,k) = - (p%loc%tdata%x%ss1(i,j,k)+p%loc%tdata%x%ss2(i,j,k)-p%loc%tdata%x%ss1(i-1,j,k)-p%loc%tdata%x%ss2(i-1,j,k)) / p%glb%dx &
              &- (p%loc%tdata%y%ss1(i,j,k)+p%loc%tdata%y%ss2(i,j,k)-p%loc%tdata%y%ss1(i,j-1,k)-p%loc%tdata%y%ss2(i,j-1,k)) / p%glb%dy &
              &- (p%loc%tdata%z%ss1(i,j,k)+p%loc%tdata%z%ss2(i,j,k)-p%loc%tdata%z%ss1(i,j,k-1)-p%loc%tdata%z%ss2(i,j,k-1)) / p%glb%dz
    ! s(i,j,k) = - p%loc%nvel%x%old(i,j,k)*p%loc%tdata%x%s1(i,j,k) &
    !         &  - p%loc%nvel%y%old(i,j,k)*p%loc%tdata%y%s1(i,j,k) &
    !         &  - p%loc%nvel%z%old(i,j,k)*p%loc%tdata%z%s1(i,j,k) 
end do 
end do
end do
!$omp end parallel do 


end subroutine

subroutine wenojs_flux_split(f,g,fp,gm,is,ie,ghc)
implicit none
integer :: i, is, ie, ghc
real(8),dimension(is-ghc:ie+ghc) :: f, g, fp, gm
real(8) :: a1,a2,a3,b1,b2,b3,w1,w2,w3,eps

EPS = 1.0D-13

do i = is-ghc+2, ie+ghc-3
    
    b1 = 13.0d0*(g(i-2)-2.0d0*g(i-1)+g(i))**2.0d0 + 3.0d0*(g(i-2)-4.0d0*g(i-1)+3.0d0*g(i))**2.0d0
    b2 = 13.0d0*(g(i-1)-2.0d0*g(i)+g(i+1))**2.0d0 + 3.0d0*(g(i-1)-g(i+1))**2.0d0
    b3 = 13.0d0*(g(i)-2.0d0*g(i+1)+g(i+2))**2.0d0 + 3.0d0*(3.0d0*g(i)-4.0d0*g(i+1)+g(i+2))**2.0d0
    
    a1 = 1.0d0/(EPS+b1)**2.0d0
    a2 = 6.0d0/(EPS+b2)**2.0d0
    a3 = 3.0d0/(EPS+b3)**2.0d0

    !a1 = 1.0d0*(1.0d0+abs(b3-b1)/(EPS+b1))
    !a2 = 6.0d0*(1.0d0+abs(b3-b1)/(EPS+b2))
    !a3 = 3.0d0*(1.0d0+abs(b3-b1)/(EPS+b3))
    
    w1 = a1/(a1+a2+a3)
    w2 = a2/(a1+a2+a3)
    w3 = a3/(a1+a2+a3)
    
    gm(i) = w1/3.0d0*g(i-2) - (7.0d0*w1+w2)/6.0d0*g(i-1) + (11.0d0*w1+5.0d0*w2+2.0d0*w3)/6.0d0*g(i) &
            + (2.0d0*w2+5.0d0*w3)/6.0d0*g(i+1) - w3/6.0d0*g(i+2)
            
end do
    
do i = is-ghc+2, ie+ghc-3
    
    b3 = 13.0d0*(f(i-1)-2.0d0*f(i)  +f(i+1))**2.0d0 + 3.0d0*(f(i-1)-4.0d0*f(i)+3.0d0*f(i+1))**2.0d0
    b2 = 13.0d0*(f(i)  -2.0d0*f(i+1)+f(i+2))**2.0d0 + 3.0d0*(f(i)-f(i+2))**2.0d0
    b1 = 13.0d0*(f(i+1)-2.0d0*f(i+2)+f(i+3))**2.0d0 + 3.0d0*(3.0d0*f(i+1)-4.0d0*f(i+2)+f(i+3))**2.0d0
    
    a1 = 1.0d0/(EPS+b1)**2.0d0
    a2 = 6.0d0/(EPS+b2)**2.0d0
    a3 = 3.0d0/(EPS+b3)**2.0d0

    !a1 = 1.0d0*(1.0d0+abs(b3-b1)/(EPS+b1))
    !a2 = 6.0d0*(1.0d0+abs(b3-b1)/(EPS+b2))
    !a3 = 3.0d0*(1.0d0+abs(b3-b1)/(EPS+b3))
    
    w1 = a1 / (a1+a2+a3)
    w2 = a2 / (a1+a2+a3)
    w3 = a3 / (a1+a2+a3)
    
    fp(i) =  w3*(-f(i-1)+5.0d0*f(i)+2.0d0*f(i+1))/6.0d0 &
            +w2*(2.0d0*f(i)+5.0d0*f(i+1)-f(i+2))/6.0d0 &
            +w1*(11.0d0*f(i+1)-7.0d0*f(i+2)+2.0d0*f(i+3))/6.0d0 
    
    
end do


end subroutine

subroutine crweno_flux_split(f,g,fp,gm,is,ie,ghc)
implicit none
integer :: i, is, ie, ghc
real(8),dimension(is-ghc:ie+ghc) :: f, g, fp, gm
real(8),dimension(is-ghc+3:ie+ghc-4) :: A,B,C,S
real(8) :: a1,a2,a3,b1,b2,b3,w1,w2,w3,eps,c1,c2,c3

EPS = 1.0D-13

c1 = 0.2089141306d0
c2 = 0.4999999998d0
c3 = 0.2910858692d0
    
call wenojs_flux_split(f,g,fp,gm,is,ie,ghc)

do i = is-ghc+3, ie+ghc-4
    
    b1 = 13.0d0*(g(i-2)-2.0d0*g(i-1)+g(i))**2.0d0   + 3.0d0*(    g(i-2)-4.0d0*g(i-1)+3.0d0*g(i))**2.0d0
    b2 = 13.0d0*(g(i-1)-2.0d0*g(i)  +g(i+1))**2.0d0 + 3.0d0*(    g(i-1)  -g(i+1))**2.0d0
    b3 = 13.0d0*(g(i)  -2.0d0*g(i+1)+g(i+2))**2.0d0 + 3.0d0*(3.0d0*g(i)-4.0d0*g(i+1)+g(i+2))**2.0d0
    
    a1 = c1*(1.0d0+abs(b3-b1)/(EPS+b1))
    a2 = c2*(1.0d0+abs(b3-b1)/(EPS+b2))
    a3 = c3*(1.0d0+abs(b3-b1)/(EPS+b3))
    
    w1 = a1/(a1+a2+a3)
    w2 = a2/(a1+a2+a3)
    w3 = a3/(a1+a2+a3)  
    
    A(i) = (2.0d0*w1+w2)/3.0d0
    B(i) = (w1+2.0d0*(w2+w3))/3.0d0
    C(i) =  w3/3.0d0
    S(i) = w1/6.0d0*g(i-1) + (5.0d0*(w1+w2)+w3)/6.0d0*g(i) + (w2+5.0d0*w3)/6.0d0*g(i+1)
    
end do  

    S(is-ghc+3) = S(is-ghc+3) - A(is-ghc+3)*gm(is-ghc+2)
    S(ie+ghc-4) = S(ie+ghc-4) - C(ie+ghc-4)*gm(ie+ghc-3)

    call solve_tridiagonal(A,B,C,S,gm(is-ghc+3:ie+ghc-4),is-ghc+3,ie+ghc-4)

do i = is-ghc+3, ie+ghc-4
    
    b3 = 13.0d0*(f(i-1)-2.0d0*f(i)  +f(i+1))**2.0d0 + 3.0d0*(f(i-1)-4.0d0*f(i)+3.0d0*f(i+1))**2.0d0
    b2 = 13.0d0*(f(i)  -2.0d0*f(i+1)+f(i+2))**2.0d0 + 3.0d0*(f(i)-f(i+2))**2.0d0
    b1 = 13.0d0*(f(i+1)-2.0d0*f(i+2)+f(i+3))**2.0d0 + 3.0d0*(3.0d0*f(i+1)-4.0d0*f(i+2)+f(i+3))**2.0d0
    
    a1 = c1*(1.0d0+abs(b3-b1)/(EPS+b1))
    a2 = c2*(1.0d0+abs(b3-b1)/(EPS+b2))
    a3 = c3*(1.0d0+abs(b3-b1)/(EPS+b3))
    
    w1 = a1 / (a1+a2+a3)
    w2 = a2 / (a1+a2+a3)
    w3 = a3 / (a1+a2+a3)
    
    A(i) = (w3)/3.0d0
    B(i) = (w1+2.0d0*(w2+w3))/3.0d0
    C(i) = (w2+2.0d0*w1)/3.0d0
    S(i) = (5.0d0*w3+w2)/6.0d0*f(i) + (w3+5.0d0*(w2+w1))/6.0d0*f(i+1) + w1/6.0d0*f(i+2)
    
end do
    
    S(is-ghc+3) = S(is-ghc+3) - A(is-ghc+3)*fp(is-ghc+2)
    S(ie+ghc-4) = S(ie+ghc-4) - C(ie+ghc-4)*fp(ie+ghc-3)

    call solve_tridiagonal(A,B,C,S,fp(is-ghc+3:ie+ghc-4),is-ghc+3,ie+ghc-4)
    
end subroutine
