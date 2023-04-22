subroutine vof_wlic_solver()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,ii,jj,kk,ib
real(8) :: eps,isgn,alpha,beta,w,a1,a3,a4,a5,xc,s
integer(8) :: cpustart, cpuend

    call system_clock(cpustart)

    eps = 1.0d-10

    call p%surface_norms2
    call pt%normals%sync

    ! x direction 
    !$omp parallel do private(i,j,k,ii,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc) 
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
        !$omp& private(i,j,k,ii,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc) 
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is-1, p%of(id)%loc%ie
            
            if( p%of(id)%loc%vel%x%old(i,j,k) > 0.0d0 )then
                ii = i 
                isgn = 1.0d0
            else
                ii = i+1
                isgn = 0.0d0
            endif
            
            beta = 2.3d0
            
            if( p%of(id)%loc%vof%now(ii,j,k) > 1.0d0-eps .or. p%of(id)%loc%vof%now(ii,j,k) < eps )then
               
               p%of(id)%loc%tdata%x%s1(i,j,k) = p%of(id)%loc%vof%now(ii,j,k) * p%of(id)%loc%vel%x%old(i,j,k) * p%glb%dt
              
            else
            
                ib = max(1,ii-1)
                
                if( p%of(id)%loc%vof%now(ib,j,k) < p%of(id)%loc%vof%now(ii+1,j,k) )then
                    alpha = 1.0d0
                else
                    alpha = -1.0d0
                endif
                
                a1 = dexp( beta*( 2.0d0 * p%of(id)%loc%vof%now(ii,j,k) - 1.0d0 ) / alpha )
                a3 = dexp( beta )
                xc = 0.5d0 / beta * dlog( (a3*a3-a1*a3)/(a1*a3-1.0d0) )
                a4 = dcosh( beta * ( isgn - p%of(id)%loc%vel%x%old(i,j,k)*p%glb%dt/p%glb%dx - xc ) )
                a5 = dcosh( beta * ( isgn - xc ) )
                
                w  = abs(p%of(id)%loc%normals%x%now(ii,j,k)) + abs(p%of(id)%loc%normals%y%now(ii,j,k)) + abs(p%of(id)%loc%normals%z%now(ii,j,k))
                w  = abs(p%of(id)%loc%normals%x%now(ii,j,k)) / w
            
                p%of(id)%loc%tdata%x%s1(i,j,k) = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k)*p%glb%dt - alpha*p%glb%dx/beta*dlog(a4/a5) )
            
                p%of(id)%loc%tdata%x%s1(i,j,k) = p%of(id)%loc%tdata%x%s1(i,j,k)*w + ( 1.0d0 - w ) * p%of(id)%loc%vof%now(ii,j,k) * p%of(id)%loc%vel%x%old(i,j,k) * p%glb%dt
                                                
            endif
            
        end do
        end do
        end do
        !$omp end parallel do

    enddo
    !$omp end parallel do

    call pt%tdatax%nodes(1)%sync

    !$omp parallel do private(i,j,k,s)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,s)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            s = ( p%of(id)%loc%tdata%x%s1(i,j,k) - p%of(id)%loc%tdata%x%s1(i-1,j,k) ) / p%glb%dx

            p%of(id)%loc%vof%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k) - s + &
                                        & p%of(id)%loc%vof%old(i,j,k) * p%glb%dt * ( p%of(id)%loc%vel%x%old(i,j,k) - p%of(id)%loc%vel%x%old(i-1,j,k) ) / p%glb%dx
            
        end do
        end do 
        end do 
        !$omp end parallel do
        
        call p%of(id)%bc(0,p%of(id)%loc%vof%now)
        
    end do
    !$omp end parallel do
    
    call pt%vof%sync

    ! y direction 
    !$omp parallel do private(i,j,k,jj,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
        !$omp& private(i,j,k,jj,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js-1, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
            
            if( p%of(id)%loc%vel%y%old(i,j,k) > 0.0d0 )then
                jj = j 
                isgn = 1.0d0
            else
                jj = j+1
                isgn = 0.0d0
            endif
            
            beta = 2.3d0
            
            if( p%of(id)%loc%vof%now(i,jj,k) > 1.0d0-eps .or. p%of(id)%loc%vof%now(i,jj,k) < eps )then
               
               p%of(id)%loc%tdata%y%s1(i,j,k) = p%of(id)%loc%vof%now(i,jj,k) * p%of(id)%loc%vel%y%old(i,j,k) * p%glb%dt
              
            else
            
                ib = max(1,jj-1)
                
                if( p%of(id)%loc%vof%now(i,ib,k) < p%of(id)%loc%vof%now(i,jj+1,k) )then
                    alpha = 1.0d0
                else
                    alpha = -1.0d0
                endif
                
                a1 = dexp( beta*( 2.0d0 * p%of(id)%loc%vof%now(i,jj,k) - 1.0d0 ) / alpha )
                a3 = dexp( beta )
                xc = 0.5d0 / beta * dlog( ( a3*a3-a1*a3)/(a1*a3-1.0d0) )
                a4 = dcosh( beta * ( isgn - p%of(id)%loc%vel%y%old(i,j,k)*p%glb%dt/p%glb%dy - xc ) )
                a5 = dcosh( beta * ( isgn - xc ) )
                
                w  = abs(p%of(id)%loc%normals%x%now(i,jj,k)) + abs(p%of(id)%loc%normals%y%now(i,jj,k)) + abs(p%of(id)%loc%normals%z%now(i,jj,k))
                w  = abs(p%of(id)%loc%normals%y%now(i,jj,k)) / w
            
                p%of(id)%loc%tdata%y%s1(i,j,k) = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k)*p%glb%dt - alpha*p%glb%dy/beta*dlog(a4/a5) )
            
                p%of(id)%loc%tdata%y%s1(i,j,k) = p%of(id)%loc%tdata%y%s1(i,j,k)*w + ( 1.0d0 - w ) * p%of(id)%loc%vof%now(i,jj,k) * p%of(id)%loc%vel%y%old(i,j,k) * p%glb%dt
                                                
            endif
            
        end do
        end do
        end do
        !$omp end parallel do

    enddo
    !$omp end parallel do

    call pt%tdatay%nodes(1)%sync 

    !$omp parallel do private(i,j,k,s)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            s = ( p%of(id)%loc%tdata%y%s1(i,j,k) - p%of(id)%loc%tdata%y%s1(i,j-1,k) ) / p%glb%dy
            
            p%of(id)%loc%vof%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k) - s + &
                                        & p%of(id)%loc%vof%old(i,j,k) * p%glb%dt * ( p%of(id)%loc%vel%y%old(i,j,k) - p%of(id)%loc%vel%y%old(i,j-1,k) ) / p%glb%dy
            
        end do
        end do 
        end do 
        !$omp end parallel do
        
        call p%of(id)%bc(0,p%of(id)%loc%vof%now)
    
    enddo
    !$omp end parallel do 
    
    call pt%vof%sync

    ! z direction 
    !$omp parallel do private(i,j,k,kk,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc) 
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
        !$omp& private(i,j,k,kk,ib,isgn,alpha,beta,w,a1,a3,a4,a5,xc) 
        do k = p%of(id)%loc%ks-1, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
            
            if( p%of(id)%loc%vel%z%old(i,j,k) > 0.0d0 )then
                kk = k 
                isgn = 1.0d0
            else
                kk = k+1
                isgn = 0.0d0
            endif
            
            beta = 2.3d0
            
            if( p%of(id)%loc%vof%now(i,j,kk) > 1.0d0-eps .or. p%of(id)%loc%vof%now(i,j,kk) < eps )then
               
               p%of(id)%loc%tdata%z%s1(i,j,k) = p%of(id)%loc%vof%now(i,j,kk) * p%of(id)%loc%vel%z%old(i,j,k) * p%glb%dt
              
            else
            
                ib = max(1,kk-1)
                
                if( p%of(id)%loc%vof%now(i,j,ib) < p%of(id)%loc%vof%now(i,j,kk+1) )then
                    alpha = 1.0d0
                else
                    alpha = -1.0d0
                endif
                
                a1 = dexp( beta*( 2.0d0 * p%of(id)%loc%vof%now(i,j,kk) - 1.0d0 ) / alpha )
                a3 = dexp( beta )
                xc = 0.5d0 / beta * dlog( ( a3*a3-a1*a3)/(a1*a3-1.0d0) )
                a4 = dcosh( beta * ( isgn - p%of(id)%loc%vel%z%old(i,j,k)*p%glb%dt/p%glb%dz - xc ) )
                a5 = dcosh( beta * ( isgn - xc ) )
                
                w  = abs(p%of(id)%loc%normals%x%now(i,j,kk)) + abs(p%of(id)%loc%normals%y%now(i,j,kk)) + abs(p%of(id)%loc%normals%z%now(i,j,kk))
                w  = abs(p%of(id)%loc%normals%z%now(i,j,kk)) / w
            
                p%of(id)%loc%tdata%z%s1(i,j,k) = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k)*p%glb%dt - alpha*p%glb%dz/beta*dlog(a4/a5) )
            
                p%of(id)%loc%tdata%z%s1(i,j,k) = p%of(id)%loc%tdata%z%s1(i,j,k)*w + ( 1.0d0 - w ) * p%of(id)%loc%vof%now(i,j,kk) * p%of(id)%loc%vel%z%old(i,j,k) * p%glb%dt
                                                
            endif
            
        end do
        end do
        end do
        !$omp end parallel do

    enddo
    !$omp end parallel do

    call pt%tdataz%nodes(1)%sync 

    !$omp parallel do private(i,j,k,s)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
        !$omp& private(i,j,k,s)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            s = ( p%of(id)%loc%tdata%z%s1(i,j,k) - p%of(id)%loc%tdata%z%s1(i,j,k-1) ) / p%glb%dz

            p%of(id)%loc%vof%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k) - s + &
                                        & p%of(id)%loc%vof%old(i,j,k) * p%glb%dt * ( p%of(id)%loc%vel%z%old(i,j,k) - p%of(id)%loc%vel%z%old(i,j,k-1) ) / p%glb%dz
            
            p%of(id)%loc%vof%now(i,j,k) = min(1.0d0,max(0.0d0,p%of(id)%loc%vof%now(i,j,k)))
                    
        end do
        end do 
        end do 
        !$omp end parallel do
        
        call p%of(id)%bc(0,p%of(id)%loc%vof%now)
        
    end do
    !$omp end parallel do
    
    call pt%vof%sync
    
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
    
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            p%of(id)%loc%vof%tmp(i,j,k) = p%of(id)%loc%phi%now(i,j,k)   
            p%of(id)%loc%phi%now(i,j,k) = 2.0 * p%of(id)%loc%vof%now(i,j,k) - 1.0d0
        end do
        end do
        end do
    enddo
    !$omp end parallel do

    call level_set_rk3_redis(0,5.0d0*p%glb%ls_wid)

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1

        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            if( abs(p%of(id)%loc%vof%tmp(i,j,k)) > 1.5*p%glb%ls_wid )p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%vof%tmp(i,j,k)
        end do
        end do
        end do

        call p%of(id)%bc(0,p%of(id)%loc%phi%now)
        
    end do
    !$omp end parallel do

    call pt%phi%sync
    call level_set_rk3_redis(1)

    call system_clock(cpuend)
    p%glb%ls_red = p%glb%ls_red + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)   
    
end subroutine





