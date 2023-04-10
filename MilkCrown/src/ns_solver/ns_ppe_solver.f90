subroutine ppe_sor_init
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

    !$omp parallel do collapse(3)  
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        p%loc%coe%r(i,j,k) = 2.0d0 / (p%loc%rho%now(i,j,k)+p%loc%rho%now(i+1,j,k))
        p%loc%coe%l(i,j,k) = 2.0d0 / (p%loc%rho%now(i,j,k)+p%loc%rho%now(i-1,j,k))
        p%loc%coe%f(i,j,k) = 2.0d0 / (p%loc%rho%now(i,j,k)+p%loc%rho%now(i,j+1,k))
        p%loc%coe%b(i,j,k) = 2.0d0 / (p%loc%rho%now(i,j,k)+p%loc%rho%now(i,j-1,k))
        p%loc%coe%u(i,j,k) = 2.0d0 / (p%loc%rho%now(i,j,k)+p%loc%rho%now(i,j,k+1))
        p%loc%coe%d(i,j,k) = 2.0d0 / (p%loc%rho%now(i,j,k)+p%loc%rho%now(i,j,k-1))
        
        p%loc%coe%r(i,j,k) = p%loc%coe%r(i,j,k) / p%glb%dx**2.0d0
        p%loc%coe%l(i,j,k) = p%loc%coe%l(i,j,k) / p%glb%dx**2.0d0
        p%loc%coe%f(i,j,k) = p%loc%coe%f(i,j,k) / p%glb%dy**2.0d0
        p%loc%coe%b(i,j,k) = p%loc%coe%b(i,j,k) / p%glb%dy**2.0d0
        p%loc%coe%u(i,j,k) = p%loc%coe%u(i,j,k) / p%glb%dz**2.0d0
        p%loc%coe%d(i,j,k) = p%loc%coe%d(i,j,k) / p%glb%dz**2.0d0
        
        p%loc%coe%c(i,j,k) = - ( p%loc%coe%r(i,j,k) + p%loc%coe%l(i,j,k) + &
                                &p%loc%coe%f(i,j,k) + p%loc%coe%b(i,j,k) + &
                                &p%loc%coe%u(i,j,k) + p%loc%coe%d(i,j,k) )
                        
        if( i==1 )then
            p%loc%coe%c(i,j,k)=p%loc%coe%c(i,j,k)+p%loc%coe%l(i,j,k)
            p%loc%coe%l(i,j,k)=0.0d0
        endif
        
        if( i==p%glb%node_x )then
            p%loc%coe%c(i,j,k)=p%loc%coe%c(i,j,k)+p%loc%coe%r(i,j,k)
            p%loc%coe%r(i,j,k)=0.0d0
        endif
        
        if( j==1 )then
            p%loc%coe%c(i,j,k)=p%loc%coe%c(i,j,k)+p%loc%coe%b(i,j,k)
            p%loc%coe%b(i,j,k)=0.0d0
        endif
        
        if( j==p%glb%node_y )then
            p%loc%coe%c(i,j,k)=p%loc%coe%c(i,j,k)+p%loc%coe%f(i,j,k)
            p%loc%coe%f(i,j,k)=0.0d0
        endif
        
        if( k==1 )then
            p%loc%coe%c(i,j,k)=p%loc%coe%c(i,j,k)+p%loc%coe%d(i,j,k)
            p%loc%coe%d(i,j,k)=0.0d0
        endif
        
        if( k==p%glb%node_z )then
            p%loc%coe%c(i,j,k)=p%loc%coe%c(i,j,k)+p%loc%coe%u(i,j,k)
            p%loc%coe%u(i,j,k)=0.0d0
        endif
        
    end do
    end do
    end do
    !$omp end parallel do
    
end subroutine

subroutine ppe_sor_solver(tol)
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,iter
integer(8) :: cpustart, cpuend
real(8) :: sump, err, w, pcal, tol

    call system_clock(cpustart)

    !$omp parallel do collapse(3)    
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        p%loc%coe%src(i,j,k) = ( ( p%loc%vel%x%now(i,j,k) - p%loc%vel%x%now(i-1,j,k) ) / p%glb%dx + &
                                 ( p%loc%vel%y%now(i,j,k) - p%loc%vel%y%now(i,j-1,k) ) / p%glb%dy + & 
                                 ( p%loc%vel%z%now(i,j,k) - p%loc%vel%z%now(i,j,k-1) ) / p%glb%dz ) / p%glb%dt
                                                
    end do
    end do
    end do 
    !$omp end parallel do
    
    w = p%glb%p_w1
    
do
    
    p%glb%piter=p%glb%piter+1

    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        p%loc%p%tmp(i,j,k) = p%loc%p%now(i,j,k)
        
        p%loc%p%now(i,j,k) = p%loc%coe%src(i,j,k) - p%loc%coe%r(i,j,k)*p%loc%p%now(i+1,j,k) &
                                              &   - p%loc%coe%l(i,j,k)*p%loc%p%now(i-1,j,k) &
                                              &   - p%loc%coe%f(i,j,k)*p%loc%p%now(i,j+1,k) &
                                              &   - p%loc%coe%b(i,j,k)*p%loc%p%now(i,j-1,k) &
                                              &   - p%loc%coe%u(i,j,k)*p%loc%p%now(i,j,k+1) &
                                              &   - p%loc%coe%d(i,j,k)*p%loc%p%now(i,j,k-1)
                                                        
        p%loc%p%now(i,j,k) = p%loc%p%now(i,j,k) / p%loc%coe%c(i,j,k)  

    end do
    end do
    end do
    !$omp end parallel do
    
    sump=0.0d0
    !$omp parallel do collapse(3), reduction(+:sump)    
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
           
        sump = sump + p%loc%p%now(i,j,k)

    end do
    end do
    end do
    !$omp end parallel do
    
    sump = sump / ( p%glb%node_x * p%glb%node_y * p%glb%node_z )

    err=0.0d0    
    !$omp parallel do collapse(3), reduction(max:err)  
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        
        p%loc%p%now(i,j,k) = p%loc%p%now(i,j,k) - sump

        p%loc%p%now(i,j,k) = w * p%loc%p%now(i,j,k) + (1.0d0-w)*p%loc%p%tmp(i,j,k)

        err = max( err,abs(p%loc%p%now(i,j,k)-p%loc%p%tmp(i,j,k)) )

    end do
    end do
    end do
    !$omp end parallel do

    call bc(p%loc%p%now)

    if( err < tol ) exit
    !if( err < p%glb%p_b ) w = p%glb%p_w2
    if( err > 10 .and. p%glb%piter > 100000 )then
        write(*,*)"The solution can not converge in PPE :",err
        stop
    end if
    
    if( mod(p%glb%piter,5000) .eq. 0 )then
        write(*,'("PPE iter:",I8,",error:",ES15.7)')p%glb%piter,err
    endif
    
end do

    p%glb%ppe_linf = err
    
    call system_clock(cpuend)
    p%glb%ppe = p%glb%ppe + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)
    
    call ns_momentum_correction
    
end subroutine

subroutine ns_momentum_correction
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: px,py,pz,rho,ux,vy,wz

    !$omp parallel do collapse(3), private(px,py,pz,rho)   
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        
        rho = (p%loc%rho%now(i,j,k)+p%loc%rho%now(i+1,j,k))/2.0d0
        px  = (p%loc%p%now(i+1,j,k)-p%loc%p%now(i,j,k)) / p%glb%dx  
        p%loc%vel%x%now(i,j,k) = p%loc%vel%x%now(i,j,k) - p%glb%dt*px/rho
        
        rho = (p%loc%rho%now(i,j,k)+p%loc%rho%now(i,j+1,k))/2.0d0
        py  = (p%loc%p%now(i,j+1,k)-p%loc%p%now(i,j,k)) / p%glb%dy        
        p%loc%vel%y%now(i,j,k) = p%loc%vel%y%now(i,j,k) - p%glb%dt*py/rho
        
        rho = (p%loc%rho%now(i,j,k)+p%loc%rho%now(i,j,k+1))/2.0d0
        pz  = (p%loc%p%now(i,j,k+1)-p%loc%p%now(i,j,k)) / p%glb%dz        
        p%loc%vel%z%now(i,j,k) = p%loc%vel%z%now(i,j,k) - p%glb%dt*pz/rho
        
    end do
    end do
    end do
    !$omp end parallel do

    call velbc(p%loc%vel%x%now,p%loc%vel%y%now,p%loc%vel%z%now)
  
end subroutine
