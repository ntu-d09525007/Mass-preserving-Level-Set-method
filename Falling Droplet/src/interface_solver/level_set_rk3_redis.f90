subroutine level_set_rk3_redis(btn)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn,iter
real(8) :: time, error, timestop
integer(8) :: cpustart, cpuend

    id=0

    call system_clock(cpustart)

    call level_set_redis_init(btn)
    
    iter = 0
    time = 0.0_8

    if( btn .eq. 0  )then
        timestop = 2.5d0*max(p%glb%xend-p%glb%xstart,p%glb%yend-p%glb%ystart,p%glb%zend-p%glb%zstart)
    else
        timestop = 2.5d0 * p%glb%dx
    end if

    
do

    iter = iter + 1
    time = time + p%glb%rdt

    !$omp parallel do collapse(3)   
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        p%loc%phi%tmp(i,j,k) = p%loc%phi%now(i,j,k)
    end do
    end do 
    end do
    !$omp end parallel do 
           
    call level_set_rk3_redis_solver(btn)
    
    error=0.0_8
    
    !$omp parallel do collapse(3) 
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        error = max( error, abs(p%loc%phi%tmp(i,j,k)-p%loc%phi%now(i,j,k)) )
    end do
    end do 
    end do
    !$omp end parallel do 
    
    if( time>timestop .or.  error.le.1.0d-8) exit
    
    if( mod(iter,100) .eq. 0 )then
        write(*,'("LS Init:",I8,F8.5,ES15.4)')iter,time,error
    end if
    
end do 

    call system_clock(cpuend)
    p%glb%ls_red = p%glb%ls_red + real(btn,kind=8)*real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)

end subroutine

subroutine level_set_redis_init(btn)
use all
!$ use omp_lib
implicit none
integer :: btn,id,i,j,k
real(8) :: grad 

id=0

call ls_funs

!$omp parallel do collapse(3) 
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    p%loc%sign%tmp(i,j,k) = p%loc%sign%now(i,j,k)
end do
end do 
end do
    
if( btn.eq.0) call level_set_redis_stable()
        

end subroutine

subroutine level_set_redis_stable()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn
real(8) :: grad

id=0

!$omp parallel do collapse(3) 
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    p%loc%sign%tmp(i,j,k) = p%loc%phi%now(i,j,k)
end do
end do 
end do
!$omp end parallel do 

call level_set_redis_gradient()

grad = 0.0_8
!$omp parallel do collapse(3), reduction(max:grad)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    grad = max( grad, p%loc%grad%now(i,j,k) )
end do
end do 
end do
!$omp end parallel do

!$omp parallel do collapse(3) 
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie         
    p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) / grad            
end do
end do
end do
!$omp end parallel do 

call bc(p%loc%phi%now)

end subroutine

subroutine level_set_rk3_redis_solver(btn)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn
real(8) :: src

id=0

call level_set_redis_gradient
call level_set_redis_lambda(btn)

!$omp parallel do collapse(3), private(src)     
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%tdata%x%l1(i,j,k) = (p%loc%sign%tmp(i,j,k)-p%loc%tdata%x%s1(i,j,k))*p%loc%grad%now(i,j,k)-p%loc%sign%tmp(i,j,k)
    
    src = p%loc%tdata%x%l1(i,j,k)

    p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) - p%glb%rdt * src
    
end do
end do 
end do
!$omp end parallel do 

call bc(p%loc%phi%now)

call level_set_redis_gradient
call level_set_redis_lambda(btn)

!$omp parallel do collapse(3), private(src)  
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
        
    p%loc%tdata%x%l2(i,j,k) = (p%loc%sign%tmp(i,j,k)-p%loc%tdata%x%s1(i,j,k))*p%loc%grad%now(i,j,k)-p%loc%sign%tmp(i,j,k)
    
    src = ( -3.0_8*p%loc%tdata%x%l1(i,j,k)+p%loc%tdata%x%l2(i,j,k) ) / 4.0_8
    
    p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) - p%glb%rdt * src
    
end do
end do 
end do
!$omp end parallel do 

call bc(p%loc%phi%now)
 
call level_set_redis_gradient
call level_set_redis_lambda(btn)

!$omp parallel do collapse(3), private(src)    
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%tdata%x%l3(i,j,k) = (p%loc%sign%tmp(i,j,k)-p%loc%tdata%x%s1(i,j,k))*p%loc%grad%now(i,j,k)-p%loc%sign%tmp(i,j,k)
    
    src = ( -p%loc%tdata%x%l1(i,j,k)-p%loc%tdata%x%l2(i,j,k)+8.0_8*p%loc%tdata%x%l3(i,j,k) ) / 12.0_8
    
    p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) - p%glb%rdt * src
    
end do
end do 
end do
!$omp end parallel do 

call bc(p%loc%phi%now)
    
end subroutine

subroutine level_set_redis_gradient()
use all
!$ use omp_lib
implicit none
integer :: i,j,k
real(8) :: upp,upm,ump,umm,vpp,vpm,vmp,vmm,wpp,wpm,wmp,wmm
real(8) :: a,b,c

call bc(p%loc%phi%now)

!$omp parallel do collapse(3), private(a,b,c,upp,upm,ump,umm,vpp,vpm,vmp,vmm,wpp,wpm,wmp,wmm)
do k = p%loc%ks, p%loc%ke 
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    a=1.0/(12.0*p%glb%dx)*(-(p%loc%phi%now(i-1,j,k)-p%loc%phi%now(i-2,j,k)) &
                       +7.0*(p%loc%phi%now(i,j,k)  -p%loc%phi%now(i-1,j,k)) &
                       +7.0*(p%loc%phi%now(i+1,j,k)-p%loc%phi%now(i,j,k)) &
                           -(p%loc%phi%now(i+2,j,k)-p%loc%phi%now(i+1,j,k)))

    b=1.0/(12.0*p%glb%dy)*(-(p%loc%phi%now(i,j-1,k)-p%loc%phi%now(i,j-2,k)) &
                       +7.0*(p%loc%phi%now(i,j,k)-p%loc%phi%now(i,j-1,k)) &
                       +7.0*(p%loc%phi%now(i,j+1,k)-p%loc%phi%now(i,j,k)) &
                           -(p%loc%phi%now(i,j+2,k)-p%loc%phi%now(i,j+1,k)))

    c=1.0/(12.0*p%glb%dz)*(-(p%loc%phi%now(i,j,k-1)-p%loc%phi%now(i,j,k-2)) &
                       +7.0*(p%loc%phi%now(i,j,k)-p%loc%phi%now(i,j,k-1)) &
                       +7.0*(p%loc%phi%now(i,j,k+1)-p%loc%phi%now(i,j,k)) &
                           -(p%loc%phi%now(i,j,k+2)-p%loc%phi%now(i,j,k+1)))

    p%loc%tdata%x%s1(i,j,k)=a &
           +1.0/p%glb%dx*phyn((p%loc%phi%now(i+3,j,k)-2.0*p%loc%phi%now(i+2,j,k)+p%loc%phi%now(i+1,j,k)), &
                              (p%loc%phi%now(i+2,j,k)-2.0*p%loc%phi%now(i+1,j,k)+p%loc%phi%now(i  ,j,k)), &
                              (p%loc%phi%now(i+1,j,k)-2.0*p%loc%phi%now(i  ,j,k)+p%loc%phi%now(i-1,j,k)), &
                              (p%loc%phi%now(i  ,j,k)-2.0*p%loc%phi%now(i-1,j,k)+p%loc%phi%now(i-2,j,k)))

    p%loc%tdata%x%s2(i,j,k)=a &
           -1.0/p%glb%dx*phyn((p%loc%phi%now(i-3,j,k)-2.0*p%loc%phi%now(i-2,j,k)+p%loc%phi%now(i-1,j,k)), &
                              (p%loc%phi%now(i-2,j,k)-2.0*p%loc%phi%now(i-1,j,k)+p%loc%phi%now(i  ,j,k)), &
                              (p%loc%phi%now(i-1,j,k)-2.0*p%loc%phi%now(i  ,j,k)+p%loc%phi%now(i+1,j,k)), &
                              (p%loc%phi%now(i  ,j,k)-2.0*p%loc%phi%now(i+1,j,k)+p%loc%phi%now(i+2,j,k)))

    p%loc%tdata%y%s1(i,j,k)=b &
           +1.0/p%glb%dy*phyn((p%loc%phi%now(i,j+3,k)-2.0*p%loc%phi%now(i,j+2,k)+p%loc%phi%now(i,j+1,k)), &
                              (p%loc%phi%now(i,j+2,k)-2.0*p%loc%phi%now(i,j+1,k)+p%loc%phi%now(i,j  ,k)), &
                              (p%loc%phi%now(i,j+1,k)-2.0*p%loc%phi%now(i,j  ,k)+p%loc%phi%now(i,j-1,k)), &
                              (p%loc%phi%now(i,j  ,k)-2.0*p%loc%phi%now(i,j-1,k)+p%loc%phi%now(i,j-2,k)))

    p%loc%tdata%y%s2(i,j,k)=b &
           -1.0/p%glb%dy*phyn((p%loc%phi%now(i,j-3,k)-2.0*p%loc%phi%now(i,j-2,k)+p%loc%phi%now(i,j-1,k)), &
                              (p%loc%phi%now(i,j-2,k)-2.0*p%loc%phi%now(i,j-1,k)+p%loc%phi%now(i,j  ,k)), &
                              (p%loc%phi%now(i,j-1,k)-2.0*p%loc%phi%now(i,j  ,k)+p%loc%phi%now(i,j+1,k)), &
                              (p%loc%phi%now(i,j  ,k)-2.0*p%loc%phi%now(i,j+1,k)+p%loc%phi%now(i,j+2,k)))

    p%loc%tdata%z%s1(i,j,k)=c &
           +1.0/p%glb%dz*phyn((p%loc%phi%now(i,j,k+3)-2.0*p%loc%phi%now(i,j,k+2)+p%loc%phi%now(i,j,k+1)), &
                              (p%loc%phi%now(i,j,k+2)-2.0*p%loc%phi%now(i,j,k+1)+p%loc%phi%now(i,j,k  )), &
                              (p%loc%phi%now(i,j,k+1)-2.0*p%loc%phi%now(i,j,k  )+p%loc%phi%now(i,j,k-1)), &
                              (p%loc%phi%now(i,j,k  )-2.0*p%loc%phi%now(i,j,k-1)+p%loc%phi%now(i,j,k-2)))

    p%loc%tdata%z%s2(i,j,k)=c &
           -1.0/p%glb%dz*phyn((p%loc%phi%now(i,j,k-3)-2.0*p%loc%phi%now(i,j,k-2)+p%loc%phi%now(i,j,k-1)), &
                              (p%loc%phi%now(i,j,k-2)-2.0*p%loc%phi%now(i,j,k-1)+p%loc%phi%now(i,j,k  )), &
                              (p%loc%phi%now(i,j,k-1)-2.0*p%loc%phi%now(i,j,k  )+p%loc%phi%now(i,j,k+1)), &
                              (p%loc%phi%now(i,j,k  )-2.0*p%loc%phi%now(i,j,k+1)+p%loc%phi%now(i,j,k+2)))
    
    upm=-MIN(p%loc%tdata%x%s1(i,j,k),0.0_8)
    upp= MAX(p%loc%tdata%x%s1(i,j,k),0.0_8)
    umm=-MIN(p%loc%tdata%x%s2(i,j,k),0.0_8)
    ump= MAX(p%loc%tdata%x%s2(i,j,k),0.0_8)
    
    vpm=-MIN(p%loc%tdata%y%s1(i,j,k),0.0_8)
    vpp= MAX(p%loc%tdata%y%s1(i,j,k),0.0_8)
    vmm=-MIN(p%loc%tdata%y%s2(i,j,k),0.0_8)
    vmp= MAX(p%loc%tdata%y%s2(i,j,k),0.0_8)
    
    wpm=-MIN(p%loc%tdata%z%s1(i,j,k),0.0_8)
    wpp= MAX(p%loc%tdata%z%s1(i,j,k),0.0_8)
    wmm=-MIN(p%loc%tdata%z%s2(i,j,k),0.0_8)
    wmp= MAX(p%loc%tdata%z%s2(i,j,k),0.0_8)
    
    if( p%loc%sign%tmp(i,j,k) >= 0.0_8 )then
        p%loc%grad%now(i,j,k) = dsqrt( MAX(upm,ump)**2.0d0 + MAX(vpm,vmp)**2.0d0 + MAX(wpm,wmp)**2.0d0  )
    else 
        p%loc%grad%now(i,j,k) = dsqrt( MAX(upp,umm)**2.0d0 + MAX(vpp,vmm)**2.0d0 + MAX(wpp,wmm)**2.0d0 )
    end if

end do
end do
end do 
!$omp end parallel do

end subroutine

subroutine level_set_redis_lambda(btn)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn,ii,jj,kk
real(8) :: a,b,lam

if( btn==0 )then

    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie 
        p%loc%tdata%x%s1(i,j,k) = 0.0d0      
    end do
    end do 
    end do
    !$omp end parallel do
        
    return
    
endif

call ls_funs

!$omp parallel do collapse(3), private(lam)  
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    lam = p%loc%delta%now(i,j,k)
    !lam = (2.0d0*(1.0d0-p%glb%rho_12)*p%loc%heavy%now(i,j,k)+p%glb%rho_12)*p%loc%delta%now(i,j,k)
    
    p%loc%tdata%x%s2(i,j,k) = lam*p%loc%sign%tmp(i,j,k)*( p%loc%grad%now(i,j,k) - 1.0d0 ) 
    p%loc%tdata%x%s3(i,j,k) = p%loc%grad%now(i,j,k)*p%loc%delta%now(i,j,k)*lam
        
end do 
end do
end do
!$omp end parallel do

call bc(p%loc%tdata%x%s2)
call bc(p%loc%tdata%x%s3)

!$omp parallel do collapse(3), private(ii,jj,kk,a,b)     
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
        
    a = 51.0d0*p%loc%tdata%x%s2(i,j,k)
    b = 51.0d0*p%loc%tdata%x%s3(i,j,k)
        
    do kk = -1, 1
    do jj = -1, 1
    do ii = -1, 1
        a = a + p%loc%tdata%x%s2(i+ii,j+jj,k+kk)
        b = b + p%loc%tdata%x%s3(i+ii,j+jj,k+kk)
    end do
    end do 
    end do

    p%loc%tdata%x%s1(i,j,k) = 0.0d0
    if( abs(b)>1.0d-12 )p%loc%tdata%x%s1(i,j,k) = a/b*p%loc%delta%now(i,j,k)
    
end do
end do 
end do
!$omp end parallel do

end subroutine


function phyn(a,b,c,d)
implicit none
real(8) :: a,b,c,d,phyn
real(8) :: is0,is1,is2,alp0,alp1,alp2,w0,w2
real(8) :: eps
eps=1.0d-6
is0=13.0d0*(a-b)**2.0d0+3.0d0*(a-3.0d0*b)**2.0d0
is1=13.0d0*(b-c)**2.0d0+3.0d0*(b+c)**2.0d0
is2=13.0d0*(c-d)**2.0d0+3.0d0*(3.0d0*c-d)**2.0d0
alp0=1.0d0/(eps+is0)**2.0d0
alp1=6.0d0/(eps+is1)**2.0d0
alp2=3.0d0/(eps+is2)**2.0d0
w0=alp0/(alp0+alp1+alp2)
w2=alp2/(alp0+alp1+alp2)
phyn=w0/3.0d0*(a-2.0d0*b+c)+(w2-0.5d0)/6.0d0*(b-2.0d0*c+d)
end function
