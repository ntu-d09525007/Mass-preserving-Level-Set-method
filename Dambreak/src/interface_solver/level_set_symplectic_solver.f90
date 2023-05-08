subroutine level_set_symplectic_solver()
use all
!$ use omp_lib
implicit none
integer :: i,j,k,id,iter
integer(8) :: cpustart, cpuend
real(8) :: err,perr

    call system_clock(cpustart)

    !$omp parallel do
    do id = 0, p%glb%threads-1  
        
        call p%of(id)%loc%tdata%init(.false.,p%of(id)%loc%phi%old)
        
    enddo
    !$omp end parallel do
    
    iter=0
    
    do 
    
        iter = iter + 1
        
        call level_set_source(.false.)
        
        err=0.0_8
        
        !$omp parallel do reduction(max:err)
        do id = 0, p%glb%threads-1
    
            call p%of(id)%loc%tdata%solve_srk6(err)
            !call p%of(id)%loc%tdata%solve_srk4(err)
            
        enddo
        !$omp end parallel do
        
    
        if( err < p%glb%t_tol )exit
        
        if( mod(iter,500) == 0)write(*,*)"LS solver,",iter,err
        
    end do
    
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        call p%of(id)%loc%tdata%final_srk6()
        
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%tdata%x%target(i,j,k)
        end do
        end do
        end do
        
        call p%of(id)%bc(0,p%of(id)%loc%phi%now)
    
    enddo
    !$omp end parallel do

    call pt%phi%sync
    
    call system_clock(cpuend)
    p%glb%ls_adv = p%glb%ls_adv + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)

end subroutine


subroutine level_set_source(btn)
use all 
!$ use omp_lib
implicit none
integer :: i,j,k,id,lid
logical :: btn

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s1)
        call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s2)
        call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s3)
    
    enddo
    !$omp end parallel do
    
    call pt%tdatax%sync
    
    !$omp parallel do private(i,j,k,lid)
    do id = 0, p%glb%threads-1
        
        ! calculate x derivatives
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc

            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()
        
            call p%of(id)%loc%ccdsolvers(lid)%x%solve("uccd",&
                &p%of(id)%loc%tdata%x%s1(:,j,k),p%of(id)%loc%tdata%x%ss1(:,j,k),p%of(id)%loc%nvel%x%tmp(:,j,k),p%of(id)%loc%nvel%x%old(:,j,k))
                                        
            call p%of(id)%loc%ccdsolvers(lid)%x%solve("uccd",&
                &p%of(id)%loc%tdata%x%s2(:,j,k),p%of(id)%loc%tdata%x%ss2(:,j,k),p%of(id)%loc%nvel%x%tmp(:,j,k),p%of(id)%loc%nvel%x%old(:,j,k))
                                        
            call p%of(id)%loc%ccdsolvers(lid)%x%solve("uccd",&
               &p%of(id)%loc%tdata%x%s3(:,j,k),p%of(id)%loc%tdata%x%ss3(:,j,k),p%of(id)%loc%nvel%x%tmp(:,j,k),p%of(id)%loc%nvel%x%old(:,j,k))
            
        end do
        end do
        !$omp end parallel do
        
        !$omp parallel do private(i,j,k) collapse(3) num_threads(p%glb%nthreads)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            p%of(id)%loc%tdata%x%l1(i,j,k) = - p%of(id)%loc%tdata%x%ss1(i,j,k) * p%of(id)%loc%nvel%x%old(i,j,k)
            p%of(id)%loc%tdata%x%l2(i,j,k) = - p%of(id)%loc%tdata%x%ss2(i,j,k) * p%of(id)%loc%nvel%x%old(i,j,k)
            p%of(id)%loc%tdata%x%l3(i,j,k) = - p%of(id)%loc%tdata%x%ss3(i,j,k) * p%of(id)%loc%nvel%x%old(i,j,k)
            
        end do
        end do
        end do
        !$omp end parallel do
        
        ! calculate y derivatives
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%y%solve("uccd",&
                &p%of(id)%loc%tdata%x%s1(i,:,k),p%of(id)%loc%tdata%x%ss1(i,:,k),p%of(id)%loc%nvel%y%tmp(i,:,k),p%of(id)%loc%nvel%y%old(i,:,k))
                                        
            call p%of(id)%loc%ccdsolvers(lid)%y%solve("uccd",&
                &p%of(id)%loc%tdata%x%s2(i,:,k),p%of(id)%loc%tdata%x%ss2(i,:,k),p%of(id)%loc%nvel%y%tmp(i,:,k),p%of(id)%loc%nvel%y%old(i,:,k))
                                        
            call p%of(id)%loc%ccdsolvers(lid)%y%solve("uccd",&
               &p%of(id)%loc%tdata%x%s3(i,:,k),p%of(id)%loc%tdata%x%ss3(i,:,k),p%of(id)%loc%nvel%y%tmp(i,:,k),p%of(id)%loc%nvel%y%old(i,:,k))
        end do
        end do
        !$omp end parallel do
        
        !$omp parallel do private(i,j,k) num_threads(p%glb%nthreads)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            p%of(id)%loc%tdata%x%l1(i,j,k) = p%of(id)%loc%tdata%x%l1(i,j,k) - p%of(id)%loc%tdata%x%ss1(i,j,k) * p%of(id)%loc%nvel%y%old(i,j,k)
            p%of(id)%loc%tdata%x%l2(i,j,k) = p%of(id)%loc%tdata%x%l2(i,j,k) - p%of(id)%loc%tdata%x%ss2(i,j,k) * p%of(id)%loc%nvel%y%old(i,j,k)
            p%of(id)%loc%tdata%x%l3(i,j,k) = p%of(id)%loc%tdata%x%l3(i,j,k) - p%of(id)%loc%tdata%x%ss3(i,j,k) * p%of(id)%loc%nvel%y%old(i,j,k)
            
        end do
        end do
        end do
        !$omp end parallel do

        ! calculate z derivatives
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%z%solve("uccd",&
                    &p%of(id)%loc%tdata%x%s1(i,j,:),p%of(id)%loc%tdata%x%ss1(i,j,:),p%of(id)%loc%nvel%z%tmp(i,j,:),p%of(id)%loc%nvel%z%old(i,j,:))

            call p%of(id)%loc%ccdsolvers(lid)%z%solve("uccd",&
                    &p%of(id)%loc%tdata%x%s2(i,j,:),p%of(id)%loc%tdata%x%ss2(i,j,:),p%of(id)%loc%nvel%z%tmp(i,j,:),p%of(id)%loc%nvel%z%old(i,j,:))

            call p%of(id)%loc%ccdsolvers(lid)%z%solve("uccd",&
                   &p%of(id)%loc%tdata%x%s3(i,j,:),p%of(id)%loc%tdata%x%ss3(i,j,:),p%of(id)%loc%nvel%z%tmp(i,j,:),p%of(id)%loc%nvel%z%old(i,j,:))
        end do
        end do
        !$omp end parallel do
        
        !$omp parallel do private(i,j,k) num_threads(p%glb%nthreads)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            p%of(id)%loc%tdata%x%l1(i,j,k) = p%of(id)%loc%tdata%x%l1(i,j,k) - p%of(id)%loc%tdata%x%ss1(i,j,k) * p%of(id)%loc%nvel%z%old(i,j,k)
            p%of(id)%loc%tdata%x%l2(i,j,k) = p%of(id)%loc%tdata%x%l2(i,j,k) - p%of(id)%loc%tdata%x%ss2(i,j,k) * p%of(id)%loc%nvel%z%old(i,j,k)
            p%of(id)%loc%tdata%x%l3(i,j,k) = p%of(id)%loc%tdata%x%l3(i,j,k) - p%of(id)%loc%tdata%x%ss3(i,j,k) * p%of(id)%loc%nvel%z%old(i,j,k)
            
        end do
        end do
        end do
        !$omp end parallel do
    
    enddo
    !$omp end parallel do
    
end subroutine
