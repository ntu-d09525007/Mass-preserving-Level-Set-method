subroutine level_set_symplectic_solver()
use all
!$ use omp_lib
implicit none
integer :: i,j,k,id,iter
integer(8) :: cpustart, cpuend
real(8) :: err,perr

id=0

    call system_clock(cpustart)

    call p%loc%tdata%init(.false.,p%loc%phi%old)

    iter=0
    
    do 
    
        iter = iter + 1
        
        call level_set_source(.false.)
        
        err=0.0_8
        
        call p%loc%tdata%solve_srk6(err)
        !call p%loc%tdata%solve_srk4(err)
        
        if( err < p%glb%t_tol )exit
        
        if( mod(iter,500) == 0)write(*,*)"LS solver,",iter,err
        
    end do
     
    call p%loc%tdata%final_srk4()
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        p%loc%phi%now(i,j,k) = p%loc%tdata%x%target(i,j,k)
    end do
    end do
    end do
    !$omp end parallel do
    
    call bc(p%loc%phi%now)

    call system_clock(cpuend)
    p%glb%ls_adv = p%glb%ls_adv + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)

end subroutine


subroutine level_set_source(btn)
use all 
!$ use omp_lib
implicit none
integer :: i,j,k,id
logical :: btn
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                 p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                 p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc)  :: tmp

    call bc(p%loc%tdata%x%s1)
    call bc(p%loc%tdata%x%s2)
    call bc(p%loc%tdata%x%s3)
   
    ! calculate x derivatives
    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    
        call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%x%s1(:,j,k),p%loc%tdata%x%ss1(:,j,k),tmp(:,j,k),p%loc%nvel%x%old(:,j,k))
        call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%x%s2(:,j,k),p%loc%tdata%x%ss2(:,j,k),tmp(:,j,k),p%loc%nvel%x%old(:,j,k))
        call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%x%s3(:,j,k),p%loc%tdata%x%ss3(:,j,k),tmp(:,j,k),p%loc%nvel%x%old(:,j,k))
        
    end do
    end do
    !$omp end parallel do
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        
        p%loc%tdata%x%l1(i,j,k) = - p%loc%tdata%x%ss1(i,j,k) * p%loc%nvel%x%old(i,j,k)
        p%loc%tdata%x%l2(i,j,k) = - p%loc%tdata%x%ss2(i,j,k) * p%loc%nvel%x%old(i,j,k)
        p%loc%tdata%x%l3(i,j,k) = - p%loc%tdata%x%ss3(i,j,k) * p%loc%nvel%x%old(i,j,k)
        
    end do
    end do
    end do
    !$omp end parallel do
    
    ! calculate y derivatives
    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

        call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%x%s1(i,:,k),p%loc%tdata%x%ss1(i,:,k),tmp(i,:,k),p%loc%nvel%y%old(i,:,k))
        call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%x%s2(i,:,k),p%loc%tdata%x%ss2(i,:,k),tmp(i,:,k),p%loc%nvel%y%old(i,:,k))
        call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%x%s3(i,:,k),p%loc%tdata%x%ss3(i,:,k),tmp(i,:,k),p%loc%nvel%y%old(i,:,k))
        
    end do
    end do
    !$omp end parallel do
    
    !$omp parallel do collapse(3)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    
        p%loc%tdata%x%l1(i,j,k) = p%loc%tdata%x%l1(i,j,k) - p%loc%tdata%x%ss1(i,j,k) * p%loc%nvel%y%old(i,j,k)
        p%loc%tdata%x%l2(i,j,k) = p%loc%tdata%x%l2(i,j,k) - p%loc%tdata%x%ss2(i,j,k) * p%loc%nvel%y%old(i,j,k)
        p%loc%tdata%x%l3(i,j,k) = p%loc%tdata%x%l3(i,j,k) - p%loc%tdata%x%ss3(i,j,k) * p%loc%nvel%y%old(i,j,k)
        
    end do
    end do
    end do
    !$omp end parallel do

    ! calculate z derivatives
    !$omp parallel do collapse(2)
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        
        call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%x%s1(i,j,:),p%loc%tdata%x%ss1(i,j,:),tmp(i,j,:),p%loc%nvel%z%old(i,j,:))
        call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%x%s2(i,j,:),p%loc%tdata%x%ss2(i,j,:),tmp(i,j,:),p%loc%nvel%z%old(i,j,:))
        call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%x%s3(i,j,:),p%loc%tdata%x%ss3(i,j,:),tmp(i,j,:),p%loc%nvel%z%old(i,j,:))

    end do
    end do
    !$omp end parallel do

    !$omp parallel do collapse(3)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    
        p%loc%tdata%x%l1(i,j,k) = p%loc%tdata%x%l1(i,j,k) - p%loc%tdata%x%ss1(i,j,k) * p%loc%nvel%z%old(i,j,k)
        p%loc%tdata%x%l2(i,j,k) = p%loc%tdata%x%l2(i,j,k) - p%loc%tdata%x%ss2(i,j,k) * p%loc%nvel%z%old(i,j,k)
        p%loc%tdata%x%l3(i,j,k) = p%loc%tdata%x%l3(i,j,k) - p%loc%tdata%x%ss3(i,j,k) * p%loc%nvel%z%old(i,j,k)
        
    end do
    end do
    end do
    !$omp end parallel do
    
end subroutine
