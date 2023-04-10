subroutine vof_wlic_solver()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,ii,ib
real(8) :: eps,isgn,alpha,beta,w,a1,a3,a4,a5,xc
integer(8) :: cpustart, cpuend

    call system_clock(cpustart)

    eps = 1.0d-12

    call surface_norms2

    ! x direction 
    !$omp parallel do collapse(3), private(ii,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc)
    do k = p%loc%ks-1, p%loc%ke
    do j = p%loc%js-1, p%loc%je
    do i = p%loc%is-1, p%loc%ie
        
        if( p%loc%vel%x%old(i,j,k) > 0.0d0 )then
            ii = i 
            isgn = 1.0d0
        else
            ii = i+1
            isgn = 0.0d0
        endif
        
        beta = 2.3d0
        
        if( p%loc%vof%now(ii,j,k) > 1.0d0-eps .or. p%loc%vof%now(ii,j,k) < eps )then
           
           p%loc%tdata%x%l1(i,j,k) = p%loc%vof%now(ii,j,k) * p%loc%vel%x%old(i,j,k) * p%glb%dt
          
        else
        
            ib = max(1,ii-1)
            
            if( p%loc%vof%now(ib,j,k) < p%loc%vof%now(ii+1,j,k) )then
                alpha = 1.0d0
            else
                alpha = -1.0d0
            endif
            
            a1 = dexp( beta*( 2.0d0 * p%loc%vof%now(ii,j,k) - 1.0d0 ) / alpha )
            a3 = dexp( beta )
            xc = 0.5d0 / beta * dlog( (a3*a3-a1*a3)/(a1*a3-1.0d0) )
            a4 = dcosh( beta * ( isgn - p%loc%vel%x%old(i,j,k)*p%glb%dt/p%glb%dx - xc ) )
            a5 = dcosh( beta * ( isgn - xc ) )
            
            w  = abs(p%loc%normals%x%now(ii,j,k)) + abs(p%loc%normals%y%now(ii,j,k)) + abs(p%loc%normals%z%now(ii,j,k))
            w  = abs(p%loc%normals%x%now(ii,j,k)) / w
        
            p%loc%tdata%x%l1(i,j,k) = 0.5d0*( p%loc%vel%x%old(i,j,k)*p%glb%dt - alpha*p%glb%dx/beta*dlog(a4/a5) )
        
            p%loc%tdata%x%l1(i,j,k) = p%loc%tdata%x%l1(i,j,k)*w + ( 1.0d0 - w ) * p%loc%vof%now(ii,j,k) * p%loc%vel%x%old(i,j,k) * p%glb%dt
                                            
        endif
        
    end do
    end do
    end do
    !$omp end parallel do
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        
        p%loc%vof%now(i,j,k) = p%loc%vof%now(i,j,k) - ( p%loc%tdata%x%l1(i,j,k) - p%loc%tdata%x%l1(i-1,j,k) ) / p%glb%dx + &
                                    & p%loc%vof%old(i,j,k) * p%glb%dt * ( p%loc%vel%x%old(i,j,k) - p%loc%vel%x%old(i-1,j,k) ) / p%glb%dx
        
    end do
    end do 
    end do 
    !$omp end parallel do
    
    call bc(p%loc%vof%now)

    ! y direction 
    !$omp parallel do collapse(3), private(ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc)
    do k = p%loc%ks-1, p%loc%ke
    do j = p%loc%js-1, p%loc%je
    do i = p%loc%is-1, p%loc%ie
        
        if( p%loc%vel%y%old(i,j,k) > 0.0d0 )then
            ii = j 
            isgn = 1.0d0
        else
            ii = j+1
            isgn = 0.0d0
        endif
        
        beta = 2.3d0
        
        if( p%loc%vof%now(i,ii,k) > 1.0d0-eps .or. p%loc%vof%now(i,ii,k) < eps )then
           
           p%loc%tdata%y%l1(i,j,k) = p%loc%vof%now(i,ii,k) * p%loc%vel%y%old(i,j,k) * p%glb%dt
          
        else
        
            ib = max(1,ii-1)
            
            if( p%loc%vof%now(i,ib,k) < p%loc%vof%now(i,ii+1,k) )then
                alpha = 1.0d0
            else
                alpha = -1.0d0
            endif
            
            a1 = dexp( beta*( 2.0d0 * p%loc%vof%now(i,ii,k) - 1.0d0 ) / alpha )
            a3 = dexp( beta )
            xc = 0.5d0 / beta * dlog( ( a3*a3-a1*a3)/(a1*a3-1.0d0) )
            a4 = dcosh( beta * ( isgn - p%loc%vel%y%old(i,j,k)*p%glb%dt/p%glb%dy - xc ) )
            a5 = dcosh( beta * ( isgn - xc ) )
            
            w  = abs(p%loc%normals%x%now(i,ii,k)) + abs(p%loc%normals%y%now(i,ii,k)) + abs(p%loc%normals%z%now(i,ii,k))
            w  = abs(p%loc%normals%y%now(i,ii,k)) / w
        
            p%loc%tdata%y%l1(i,j,k) = 0.5d0*( p%loc%vel%y%old(i,j,k)*p%glb%dt - alpha*p%glb%dy/beta*dlog(a4/a5) )
        
            p%loc%tdata%y%l1(i,j,k) = p%loc%tdata%y%l1(i,j,k)*w + ( 1.0d0 - w ) * p%loc%vof%now(i,ii,k) * p%loc%vel%y%old(i,j,k) * p%glb%dt
                                            
        endif
        
    end do
    end do
    end do
    !$omp end parallel do
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        
        p%loc%vof%now(i,j,k) = p%loc%vof%now(i,j,k) - ( p%loc%tdata%y%l1(i,j,k) - p%loc%tdata%y%l1(i,j-1,k) ) / p%glb%dy + &
                                    & p%loc%vof%old(i,j,k) * p%glb%dt * ( p%loc%vel%y%old(i,j,k) - p%loc%vel%y%old(i,j-1,k) ) / p%glb%dy
        
    end do
    end do 
    end do 
    !$omp end parallel do
    
    call bc(p%loc%vof%now)

    ! z direction 
    !$omp parallel do collapse(3), private(ii,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc)
    do k = p%loc%ks-1, p%loc%ke
    do j = p%loc%js-1, p%loc%je
    do i = p%loc%is-1, p%loc%ie
        
        if( p%loc%vel%z%old(i,j,k) > 0.0d0 )then
            ii = k 
            isgn = 1.0d0
        else
            ii = k+1
            isgn = 0.0d0
        endif
        
        beta = 2.3d0
        
        if( p%loc%vof%now(i,j,ii) > 1.0d0-eps .or. p%loc%vof%now(i,j,ii) < eps )then
           
           p%loc%tdata%z%l1(i,j,k) = p%loc%vof%now(i,j,ii) * p%loc%vel%z%old(i,j,k) * p%glb%dt
          
        else
        
            ib = max(1,ii-1)
            
            if( p%loc%vof%now(i,j,ib) < p%loc%vof%now(i,j,ii+1) )then
                alpha = 1.0d0
            else
                alpha = -1.0d0
            endif
            
            a1 = dexp( beta*( 2.0d0 * p%loc%vof%now(i,j,ii) - 1.0d0 ) / alpha )
            a3 = dexp( beta )
            xc = 0.5d0 / beta * dlog( ( a3*a3-a1*a3)/(a1*a3-1.0d0) )
            a4 = dcosh( beta * ( isgn - p%loc%vel%z%old(i,j,k)*p%glb%dt/p%glb%dz - xc ) )
            a5 = dcosh( beta * ( isgn - xc ) )
            
            w  = abs(p%loc%normals%x%now(i,j,ii)) + abs(p%loc%normals%y%now(i,j,ii)) +abs(p%loc%normals%z%now(i,j,ii))
            w  = abs(p%loc%normals%z%now(i,j,ii)) / w
        
            p%loc%tdata%z%l1(i,j,k) = 0.5d0*( p%loc%vel%z%old(i,j,k)*p%glb%dt - alpha*p%glb%dz/beta*dlog(a4/a5) )
        
            p%loc%tdata%z%l1(i,j,k) = p%loc%tdata%z%l1(i,j,k)*w + ( 1.0d0 - w ) * p%loc%vof%now(i,j,ii) * p%loc%vel%z%old(i,j,k) * p%glb%dt
                                            
        endif
        
    end do
    end do
    end do
    !$omp end parallel do
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        
        p%loc%vof%now(i,j,k) = p%loc%vof%now(i,j,k) - ( p%loc%tdata%z%l1(i,j,k) - p%loc%tdata%z%l1(i,j,k-1) ) / p%glb%dz + &
                                    & p%loc%vof%old(i,j,k) * p%glb%dt * ( p%loc%vel%z%old(i,j,k) - p%loc%vel%z%old(i,j,k-1) ) / p%glb%dy
        
        if(p%loc%vof%now(i,j,k)<eps)p%loc%vof%now(i,j,k)=0.0d0
        if(p%loc%vof%now(i,j,k)>1.0-eps)p%loc%vof%now(i,j,k)=1.0d0
                
    end do
    end do 
    end do 
    !$omp end parallel do
    
    call bc(p%loc%vof%now)

    call system_clock(cpuend)
    p%glb%ls_adv = p%glb%ls_adv + real(cpuend-cpustart,kind=8) / real( p%glb%cpurate, kind=8 )
    
end subroutine

subroutine clsvof_recon()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: time
integer(8) :: cpustart, cpuend

    call system_clock(cpustart)
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        p%loc%phi%tmp(i,j,k) = p%loc%phi%now(i,j,k)   
        p%loc%phi%now(i,j,k) = 2.0d0*p%loc%vof%now(i,j,k) - 1.0d0
    end do
    end do
    end do
    !$omp end parallel do
    
    call level_set_redis_init(0)
    
    time = 0.0d0
    
do

    time = time + p%glb%rdt
    
    call level_set_rk3_redis_solver(0)
    
    if( time>3.0d0*p%glb%dx ) exit
    
end do 

    !$omp parallel do collapse(3)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        if( abs(p%loc%phi%tmp(i,j,k)) < p%glb%ls_wid )p%loc%phi%tmp(i,j,k)=p%loc%phi%now(i,j,k)
        p%loc%phi%now(i,j,k) = p%loc%phi%tmp(i,j,k)
    end do
    end do
    end do
    !$omp end parallel do

    call system_clock(cpuend)
    p%glb%ls_red = p%glb%ls_red + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)   
    
    call level_set_rk3_redis(1)

end subroutine





