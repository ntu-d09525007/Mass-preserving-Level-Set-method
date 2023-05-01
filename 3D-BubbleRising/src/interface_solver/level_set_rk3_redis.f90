subroutine level_set_rk3_redis(btn,T)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn,iter
real(8) :: time, error, timestop
real(8), optional :: T
integer(8) :: cpustart, cpuend

    call system_clock(cpustart)

    call level_set_redis_init(btn)
    
    iter = 0
    time = 0.0_8

    if( btn .eq. 0  )then
        timestop = 1.5d0*max(p%glb%xend-p%glb%xstart,p%glb%yend-p%glb%ystart,p%glb%zend-p%glb%zstart)
    else 
        timestop = 2.5d0 * p%glb%ls_wid
    end if

    if( present(T) ) timestop = T
    
do

    iter = iter + 1
    time = time + p%glb%rdt

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
            p%of(id)%loc%phi%tmp(i,j,k) = p%of(id)%loc%phi%now(i,j,k)
        end do
        end do 
        end do
        !$omp end parallel do
    enddo   
    !$omp end parallel do
    
    call level_set_rk3_redis_solver(btn)
    
    error=0.0_8
    
    !$omp parallel do private(i,j,k), reduction(max:error)
    do id = 0, p%glb%threads-1
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) reduction(max:error) private(i,j,k)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
            error = max( error, abs(p%of(id)%loc%phi%tmp(i,j,k)-p%of(id)%loc%phi%now(i,j,k)) )
        end do
        end do 
        end do
        !$omp end parallel do
        call p%of(id)%bc(0,p%of(id)%loc%phi%now)
        
    enddo
    !$omp end parallel do

    p%glb%red_error = error
    
    call pt%phi%sync
    
    if( time>timestop .or.  error.le.1.0d-6) exit
    
    if( mod(iter,100) .eq. 0 )then
        write(*,'("LS Init:",I8,F10.5,ES20.4)')iter,time,error
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

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        call p%of(id)%bc(0,p%of(id)%loc%phi%now)
    enddo
    !$omp end parallel do
    
    call pt%phi%sync
    call p%ls_funs

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
            p%of(id)%loc%sign%tmp(i,j,k) = p%of(id)%loc%sign%now(i,j,k)
        end do
        end do 
        end do
        !$omp end parallel do
    enddo
    !$omp end parallel do
        
    if( btn.eq.0) call level_set_redis_stable()
        

end subroutine

subroutine level_set_redis_stable()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn
real(8) :: grad

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        p%of(id)%loc%sign%tmp(i,j,k) = p%of(id)%loc%phi%now(i,j,k)
    end do
    end do 
    end do
    !$omp end parallel do
enddo
!$omp end parallel do

call level_set_redis_gradient()

grad = 0.0_8

!$omp parallel do private(i,j,k), reduction(max:grad)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k) reduction(max:grad)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        grad = max( grad, p%of(id)%loc%grad%now(i,j,k) )
    end do
    end do 
    end do
    !$omp end parallel do
enddo
!$omp end parallel do

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie         
        p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k) / grad            
    end do
    end do
    end do
    !$omp end parallel do
    call p%of(id)%bc(0,p%of(id)%loc%phi%now)
    
enddo
!$omp end parallel do

call pt%phi%sync


end subroutine

subroutine level_set_rk3_redis_solver(btn)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn
real(8) :: src

call level_set_redis_gradient
call level_set_redis_lambda(btn)

!$omp parallel do private(i,j,k,src)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,src)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        p%of(id)%loc%tdata%x%l1(i,j,k) = (p%of(id)%loc%sign%tmp(i,j,k)-p%of(id)%loc%tdata%x%s1(i,j,k))*p%of(id)%loc%grad%now(i,j,k)-p%of(id)%loc%sign%tmp(i,j,k)
        
        src = p%of(id)%loc%tdata%x%l1(i,j,k)

        p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k) - p%glb%rdt * src
        
    end do
    end do 
    end do
    !$omp end parallel do
    call p%of(id)%bc(0,p%of(id)%loc%phi%now)

enddo
!$omp end parallel do

call pt%phi%sync

call level_set_redis_gradient
call level_set_redis_lambda(btn)

!$omp parallel do private(i,j,k,src)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,src)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
            
        p%of(id)%loc%tdata%x%l2(i,j,k) = (p%of(id)%loc%sign%tmp(i,j,k)-p%of(id)%loc%tdata%x%s1(i,j,k))*p%of(id)%loc%grad%now(i,j,k)-p%of(id)%loc%sign%tmp(i,j,k)
        
        src = ( -3.0_8*p%of(id)%loc%tdata%x%l1(i,j,k)+p%of(id)%loc%tdata%x%l2(i,j,k) ) / 4.0_8
        
        p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k) - p%glb%rdt * src
        
    end do
    end do 
    end do
    !$omp end parallel do
    call p%of(id)%bc(0,p%of(id)%loc%phi%now)

enddo
!$omp end parallel do

call pt%phi%sync    

call level_set_redis_gradient
call level_set_redis_lambda(btn)

!$omp parallel do private(i,j,k,src)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,src)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        p%of(id)%loc%tdata%x%l3(i,j,k) = (p%of(id)%loc%sign%tmp(i,j,k)-p%of(id)%loc%tdata%x%s1(i,j,k))*p%of(id)%loc%grad%now(i,j,k)-p%of(id)%loc%sign%tmp(i,j,k)
        
        src = ( -p%of(id)%loc%tdata%x%l1(i,j,k)-p%of(id)%loc%tdata%x%l2(i,j,k)+8.0_8*p%of(id)%loc%tdata%x%l3(i,j,k) ) / 12.0_8
        
        p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k) - p%glb%rdt * src
        
    end do
    end do 
    end do
    !$omp end parallel do
    call p%of(id)%bc(0,p%of(id)%loc%phi%now)
    
enddo
!$omp end parallel do

call pt%phi%sync    
    
end subroutine

subroutine level_set_redis_gradient()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: upp,upm,ump,umm,vpp,vpm,vmp,vmm,wpp,wpm,wmp,wmm
real(8) :: a,b,c

!$omp parallel do 
do id = 0, p%glb%threads-1
    call p%of(id)%bc(0,p%of(id)%loc%phi%now)
enddo
!$omp end parallel do

call pt%phi%sync

!$omp parallel do private(i,j,k,a,b,c,upp,upm,ump,umm,vpp,vpm,vmp,vmm,wpp,wpm,wmp,wmm)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
    !$omp& private(i,j,k,a,b,c,upp,upm,ump,umm,vpp,vpm,vmp,vmm,wpp,wpm,wmp,wmm)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke 
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie

        a=1.0/(12.0*p%glb%dx)*(-(p%of(id)%loc%phi%now(i-1,j,k)-p%of(id)%loc%phi%now(i-2,j,k)) &
                           +7.0*(p%of(id)%loc%phi%now(i,j,k)  -p%of(id)%loc%phi%now(i-1,j,k)) &
                           +7.0*(p%of(id)%loc%phi%now(i+1,j,k)-p%of(id)%loc%phi%now(i,j,k)) &
                               -(p%of(id)%loc%phi%now(i+2,j,k)-p%of(id)%loc%phi%now(i+1,j,k)))

        b=1.0/(12.0*p%glb%dy)*(-(p%of(id)%loc%phi%now(i,j-1,k)-p%of(id)%loc%phi%now(i,j-2,k)) &
                           +7.0*(p%of(id)%loc%phi%now(i,j,k)-p%of(id)%loc%phi%now(i,j-1,k)) &
                           +7.0*(p%of(id)%loc%phi%now(i,j+1,k)-p%of(id)%loc%phi%now(i,j,k)) &
                               -(p%of(id)%loc%phi%now(i,j+2,k)-p%of(id)%loc%phi%now(i,j+1,k)))

        c=1.0/(12.0*p%glb%dz)*(-(p%of(id)%loc%phi%now(i,j,k-1)-p%of(id)%loc%phi%now(i,j,k-2)) &
                           +7.0*(p%of(id)%loc%phi%now(i,j,k)-p%of(id)%loc%phi%now(i,j,k-1)) &
                           +7.0*(p%of(id)%loc%phi%now(i,j,k+1)-p%of(id)%loc%phi%now(i,j,k)) &
                               -(p%of(id)%loc%phi%now(i,j,k+2)-p%of(id)%loc%phi%now(i,j,k+1)))

        p%of(id)%loc%tdata%x%s1(i,j,k)=a &
               +1.0/p%glb%dx*phyn((p%of(id)%loc%phi%now(i+3,j,k)-2.0*p%of(id)%loc%phi%now(i+2,j,k)+p%of(id)%loc%phi%now(i+1,j,k)), &
                                  (p%of(id)%loc%phi%now(i+2,j,k)-2.0*p%of(id)%loc%phi%now(i+1,j,k)+p%of(id)%loc%phi%now(i  ,j,k)), &
                                  (p%of(id)%loc%phi%now(i+1,j,k)-2.0*p%of(id)%loc%phi%now(i  ,j,k)+p%of(id)%loc%phi%now(i-1,j,k)), &
                                  (p%of(id)%loc%phi%now(i  ,j,k)-2.0*p%of(id)%loc%phi%now(i-1,j,k)+p%of(id)%loc%phi%now(i-2,j,k)))

        p%of(id)%loc%tdata%x%s2(i,j,k)=a &
               -1.0/p%glb%dx*phyn((p%of(id)%loc%phi%now(i-3,j,k)-2.0*p%of(id)%loc%phi%now(i-2,j,k)+p%of(id)%loc%phi%now(i-1,j,k)), &
                                  (p%of(id)%loc%phi%now(i-2,j,k)-2.0*p%of(id)%loc%phi%now(i-1,j,k)+p%of(id)%loc%phi%now(i  ,j,k)), &
                                  (p%of(id)%loc%phi%now(i-1,j,k)-2.0*p%of(id)%loc%phi%now(i  ,j,k)+p%of(id)%loc%phi%now(i+1,j,k)), &
                                  (p%of(id)%loc%phi%now(i  ,j,k)-2.0*p%of(id)%loc%phi%now(i+1,j,k)+p%of(id)%loc%phi%now(i+2,j,k)))

        p%of(id)%loc%tdata%y%s1(i,j,k)=b &
               +1.0/p%glb%dy*phyn((p%of(id)%loc%phi%now(i,j+3,k)-2.0*p%of(id)%loc%phi%now(i,j+2,k)+p%of(id)%loc%phi%now(i,j+1,k)), &
                                  (p%of(id)%loc%phi%now(i,j+2,k)-2.0*p%of(id)%loc%phi%now(i,j+1,k)+p%of(id)%loc%phi%now(i,j  ,k)), &
                                  (p%of(id)%loc%phi%now(i,j+1,k)-2.0*p%of(id)%loc%phi%now(i,j  ,k)+p%of(id)%loc%phi%now(i,j-1,k)), &
                                  (p%of(id)%loc%phi%now(i,j  ,k)-2.0*p%of(id)%loc%phi%now(i,j-1,k)+p%of(id)%loc%phi%now(i,j-2,k)))

        p%of(id)%loc%tdata%y%s2(i,j,k)=b &
               -1.0/p%glb%dy*phyn((p%of(id)%loc%phi%now(i,j-3,k)-2.0*p%of(id)%loc%phi%now(i,j-2,k)+p%of(id)%loc%phi%now(i,j-1,k)), &
                                  (p%of(id)%loc%phi%now(i,j-2,k)-2.0*p%of(id)%loc%phi%now(i,j-1,k)+p%of(id)%loc%phi%now(i,j  ,k)), &
                                  (p%of(id)%loc%phi%now(i,j-1,k)-2.0*p%of(id)%loc%phi%now(i,j  ,k)+p%of(id)%loc%phi%now(i,j+1,k)), &
                                  (p%of(id)%loc%phi%now(i,j  ,k)-2.0*p%of(id)%loc%phi%now(i,j+1,k)+p%of(id)%loc%phi%now(i,j+2,k)))

        p%of(id)%loc%tdata%z%s1(i,j,k)=c &
               +1.0/p%glb%dz*phyn((p%of(id)%loc%phi%now(i,j,k+3)-2.0*p%of(id)%loc%phi%now(i,j,k+2)+p%of(id)%loc%phi%now(i,j,k+1)), &
                                  (p%of(id)%loc%phi%now(i,j,k+2)-2.0*p%of(id)%loc%phi%now(i,j,k+1)+p%of(id)%loc%phi%now(i,j,k  )), &
                                  (p%of(id)%loc%phi%now(i,j,k+1)-2.0*p%of(id)%loc%phi%now(i,j,k  )+p%of(id)%loc%phi%now(i,j,k-1)), &
                                  (p%of(id)%loc%phi%now(i,j,k  )-2.0*p%of(id)%loc%phi%now(i,j,k-1)+p%of(id)%loc%phi%now(i,j,k-2)))

        p%of(id)%loc%tdata%z%s2(i,j,k)=c &
               -1.0/p%glb%dz*phyn((p%of(id)%loc%phi%now(i,j,k-3)-2.0*p%of(id)%loc%phi%now(i,j,k-2)+p%of(id)%loc%phi%now(i,j,k-1)), &
                                  (p%of(id)%loc%phi%now(i,j,k-2)-2.0*p%of(id)%loc%phi%now(i,j,k-1)+p%of(id)%loc%phi%now(i,j,k  )), &
                                  (p%of(id)%loc%phi%now(i,j,k-1)-2.0*p%of(id)%loc%phi%now(i,j,k  )+p%of(id)%loc%phi%now(i,j,k+1)), &
                                  (p%of(id)%loc%phi%now(i,j,k  )-2.0*p%of(id)%loc%phi%now(i,j,k+1)+p%of(id)%loc%phi%now(i,j,k+2)))
        
        upm=-MIN(p%of(id)%loc%tdata%x%s1(i,j,k),0.0_8)
        upp= MAX(p%of(id)%loc%tdata%x%s1(i,j,k),0.0_8)
        umm=-MIN(p%of(id)%loc%tdata%x%s2(i,j,k),0.0_8)
        ump= MAX(p%of(id)%loc%tdata%x%s2(i,j,k),0.0_8)
        
        vpm=-MIN(p%of(id)%loc%tdata%y%s1(i,j,k),0.0_8)
        vpp= MAX(p%of(id)%loc%tdata%y%s1(i,j,k),0.0_8)
        vmm=-MIN(p%of(id)%loc%tdata%y%s2(i,j,k),0.0_8)
        vmp= MAX(p%of(id)%loc%tdata%y%s2(i,j,k),0.0_8)
        
        wpm=-MIN(p%of(id)%loc%tdata%z%s1(i,j,k),0.0_8)
        wpp= MAX(p%of(id)%loc%tdata%z%s1(i,j,k),0.0_8)
        wmm=-MIN(p%of(id)%loc%tdata%z%s2(i,j,k),0.0_8)
        wmp= MAX(p%of(id)%loc%tdata%z%s2(i,j,k),0.0_8)
        
        if( p%of(id)%loc%sign%tmp(i,j,k) >= 0.0_8 )then
            p%of(id)%loc%grad%now(i,j,k) = dsqrt( MAX(upm,ump)**2.0d0 + MAX(vpm,vmp)**2.0d0 + MAX(wpm,wmp)**2.0d0  )
        else 
            p%of(id)%loc%grad%now(i,j,k) = dsqrt( MAX(upp,umm)**2.0d0 + MAX(vpp,vmm)**2.0d0 + MAX(wpp,wmm)**2.0d0 )
        end if
    
    end do
    end do
    end do 
    !$omp end parallel do
enddo   
!$omp end parallel do

end subroutine

subroutine level_set_redis_lambda(btn)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,btn,ii,jj,kk
real(8) :: a,b,lam

if( btn==0 )then

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie 
            p%of(id)%loc%tdata%x%s1(i,j,k) = 0.0d0      
        end do
        end do 
        end do
        !$omp end parallel do
    enddo
    !$omp end parallel do   
    
    return
    
endif

call p%ls_funs

!$omp parallel do private(i,j,k,lam)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,lam)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        lam = p%of(id)%loc%delta%now(i,j,k)
        !lam = (2.0d0*(1.0d0-p%glb%rho_12)*p%of(id)%loc%heavy%now(i,j,k)+p%glb%rho_12)*p%of(id)%loc%delta%now(i,j,k)
        
        p%of(id)%loc%tdata%x%s2(i,j,k) = lam*p%of(id)%loc%sign%tmp(i,j,k)*( p%of(id)%loc%grad%now(i,j,k) - 1.0d0 ) 
        p%of(id)%loc%tdata%x%s3(i,j,k) = p%of(id)%loc%grad%now(i,j,k)*p%of(id)%loc%delta%now(i,j,k)*lam
            
    end do 
    end do
    end do
    !$omp end parallel do
    call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s2)
    call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s3)

enddo       
!$omp end parallel do
    
call pt%tdatax%sync

!$omp parallel do private(i,j,k,ii,jj,kk,a,b)
do id = 0, p%glb%threads-1
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,ii,jj,kk,a,b)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
            
        a = 51.0d0*p%of(id)%loc%tdata%x%s2(i,j,k)
        b = 51.0d0*p%of(id)%loc%tdata%x%s3(i,j,k)
            
        do kk = -1, 1
        do jj = -1, 1
        do ii = -1, 1
            a = a + p%of(id)%loc%tdata%x%s2(i+ii,j+jj,k+kk)
            b = b + p%of(id)%loc%tdata%x%s3(i+ii,j+jj,k+kk)
        end do
        end do 
        end do

        p%of(id)%loc%tdata%x%s1(i,j,k) = 0.0d0
        if( abs(b)>1.0d-12 )p%of(id)%loc%tdata%x%s1(i,j,k) = a/b*p%of(id)%loc%delta%now(i,j,k)
        
    end do
    end do 
    end do
    !$omp end parallel do
enddo
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
