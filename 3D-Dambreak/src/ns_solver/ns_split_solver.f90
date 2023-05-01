subroutine ns_split_solver
use all
!$ use omp_lib
implicit none

p%glb%piter=0

call ppe_sor_init
call ns_split_adv
call ns_split_diff
call ns_check_convergence_vel
call ppe_sor_solver(p%glb%p_tol)
    
end subroutine

subroutine ns_split_adv
use all
!$ use omp_lib
implicit none
integer :: i,j,k,id,iter
integer(8) :: cpustart, cpuend
real(8) :: err


    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        call p%of(id)%loc%tdata%init(.true.,p%of(id)%loc%vel%x%old,p%of(id)%loc%vel%y%old,p%of(id)%loc%vel%z%old)

        call p%of(id)%find_stag_vel( p%of(id)%loc%velsrc%x%now, p%of(id)%loc%velsrc%y%now, p%of(id)%loc%velsrc%z%now, &
                                    &p%of(id)%loc%velsrc%x%old, p%of(id)%loc%velsrc%y%old, p%of(id)%loc%velsrc%z%old, &
                                    &p%of(id)%loc%vel%x%old   , p%of(id)%loc%vel%y%old   , p%of(id)%loc%vel%z%old )

    enddo           
    !$omp end parallel do
    
    call pt%velsrc%sync
    call pt%velsrc_old%sync
    
    iter=0
    
    do 
    
        iter = iter + 1
        
        call ns_split_adv_source()
        
        err=0.0_8  
        !$omp parallel do reduction(max:err)
        do id = 0, p%glb%threads-1
            call p%of(id)%loc%tdata%solve_srk6(err)
        enddo
        !$omp end parallel do
    
        if( err < p%glb%t_tol )exit
        
        if( mod(iter,100) == 0)write(*,*)"NS adv solver,",iter,err
        
    end do
    
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        call p%of(id)%loc%tdata%final_srk6()
        
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            p%of(id)%loc%vel%x%now(i,j,k) = p%of(id)%loc%tdata%x%target(i,j,k)
            p%of(id)%loc%vel%y%now(i,j,k) = p%of(id)%loc%tdata%y%target(i,j,k)
            p%of(id)%loc%vel%z%now(i,j,k) = p%of(id)%loc%tdata%z%target(i,j,k)
            
        end do
        end do
        end do

        call p%of(id)%velbc(p%of(id)%loc%vel%x%now,p%of(id)%loc%vel%y%now,p%of(id)%loc%vel%z%now)

    enddo
    !$omp end parallel do

    call pt%vel%sync
        
end subroutine

subroutine ns_split_adv_source
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

    !$omp parallel do 
    do id = 0, p%glb%threads-1

        call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s1);call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s2);call p%of(id)%bc(0,p%of(id)%loc%tdata%x%s3)
        call p%of(id)%bc(0,p%of(id)%loc%tdata%y%s1);call p%of(id)%bc(0,p%of(id)%loc%tdata%y%s2);call p%of(id)%bc(0,p%of(id)%loc%tdata%y%s3)
        call p%of(id)%bc(0,p%of(id)%loc%tdata%z%s1);call p%of(id)%bc(0,p%of(id)%loc%tdata%z%s2);call p%of(id)%bc(0,p%of(id)%loc%tdata%z%s3)
      
    enddo           
    !$omp end parallel do
   
    call pt%tdatax%sync
    call pt%tdatay%sync
    call pt%tdataz%sync
    
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je

            ! ux ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%x%s1(:,j,k),&
                                                &p%of(id)%loc%tdata%x%ss1(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%vel%x%old(:,j,k))   

            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%x%s2(:,j,k),&
                                                &p%of(id)%loc%tdata%x%ss2(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%vel%x%old(:,j,k)) 
                                                
            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%x%s3(:,j,k),&
                                                &p%of(id)%loc%tdata%x%ss3(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%vel%x%old(:,j,k)) 
            
            ! vx ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%y%s1(:,j,k),&
                                                &p%of(id)%loc%tdata%y%ss1(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%velsrc%x%now(:,j,k))

            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%y%s2(:,j,k),&
                                                &p%of(id)%loc%tdata%y%ss2(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%velsrc%x%now(:,j,k))

            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%y%s3(:,j,k),&
                                                &p%of(id)%loc%tdata%y%ss3(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%velsrc%x%now(:,j,k))

            ! wx ------------------------------------------------------------------------ 
            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%z%s1(:,j,k),&
                                                &p%of(id)%loc%tdata%z%ss1(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%velsrc%x%old(:,j,k))

            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%z%s2(:,j,k),&
                                                &p%of(id)%loc%tdata%z%ss2(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%velsrc%x%old(:,j,k))

            call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%tdata%z%s3(:,j,k),&
                                                &p%of(id)%loc%tdata%z%ss3(:,j,k),&
                                                &p%of(id)%loc%vel%x%tmp(:,j,k),&
                                                &p%of(id)%loc%velsrc%x%old(:,j,k))
            
        end do
        end do
        
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
            p%of(id)%loc%tdata%x%l1(i,j,k) = - p%of(id)%loc%vel%x%old(i,j,k)*p%of(id)%loc%tdata%x%ss1(i,j,k)
            p%of(id)%loc%tdata%x%l2(i,j,k) = - p%of(id)%loc%vel%x%old(i,j,k)*p%of(id)%loc%tdata%x%ss2(i,j,k)
            p%of(id)%loc%tdata%x%l3(i,j,k) = - p%of(id)%loc%vel%x%old(i,j,k)*p%of(id)%loc%tdata%x%ss3(i,j,k)

            p%of(id)%loc%tdata%y%l1(i,j,k) = - p%of(id)%loc%velsrc%x%now(i,j,k)*p%of(id)%loc%tdata%y%ss1(i,j,k)
            p%of(id)%loc%tdata%y%l2(i,j,k) = - p%of(id)%loc%velsrc%x%now(i,j,k)*p%of(id)%loc%tdata%y%ss2(i,j,k)
            p%of(id)%loc%tdata%y%l3(i,j,k) = - p%of(id)%loc%velsrc%x%now(i,j,k)*p%of(id)%loc%tdata%y%ss3(i,j,k)

            p%of(id)%loc%tdata%z%l1(i,j,k) = - p%of(id)%loc%velsrc%x%old(i,j,k)*p%of(id)%loc%tdata%z%ss1(i,j,k)
            p%of(id)%loc%tdata%z%l2(i,j,k) = - p%of(id)%loc%velsrc%x%old(i,j,k)*p%of(id)%loc%tdata%z%ss2(i,j,k)
            p%of(id)%loc%tdata%z%l3(i,j,k) = - p%of(id)%loc%velsrc%x%old(i,j,k)*p%of(id)%loc%tdata%z%ss3(i,j,k)
            
        end do
        end do
        end do
            
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            ! uy ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%tdata%x%s1(i,:,k),&
                                                &p%of(id)%loc%tdata%x%ss1(i,:,k),&
                                                &p%of(id)%loc%vel%y%tmp(i,:,k),&
                                                &p%of(id)%loc%velsrc%y%now(i,:,k))

            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%tdata%x%s2(i,:,k),&
                                                &p%of(id)%loc%tdata%x%ss2(i,:,k),&
                                                &p%of(id)%loc%vel%y%tmp(i,:,k),&
                                                &p%of(id)%loc%velsrc%y%now(i,:,k))

            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%tdata%x%s3(i,:,k),&
                                                &p%of(id)%loc%tdata%x%ss3(i,:,k),&
                                                &p%of(id)%loc%vel%y%tmp(i,:,k),&
                                                &p%of(id)%loc%velsrc%y%now(i,:,k))

            ! vy ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%tdata%y%s1(i,:,k),&
                                                &p%of(id)%loc%tdata%y%ss1(i,:,k),&
                                                &p%of(id)%loc%vel%y%tmp(i,:,k),&
                                                &p%of(id)%loc%vel%y%old(i,:,k))

            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%tdata%y%s2(i,:,k),&
                                                &p%of(id)%loc%tdata%y%ss2(i,:,k),&
                                                &p%of(id)%loc%vel%y%tmp(i,:,k),&
                                                &p%of(id)%loc%vel%y%old(i,:,k))

            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%tdata%y%s3(i,:,k),&
                                                &p%of(id)%loc%tdata%y%ss3(i,:,k),&
                                                &p%of(id)%loc%vel%y%tmp(i,:,k),&
                                                &p%of(id)%loc%vel%y%old(i,:,k))

            ! wy ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%velsrc%y%old(i,:,k),&
                                                &p%of(id)%loc%tdata%z%s1(i,:,k),&
                                                &p%of(id)%loc%tdata%z%ss1(i,:,k))   

            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%velsrc%y%old(i,:,k),&
                                                &p%of(id)%loc%tdata%z%s2(i,:,k),&
                                                &p%of(id)%loc%tdata%z%ss2(i,:,k))
                                                
            call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%velsrc%y%old(i,:,k),&
                                                &p%of(id)%loc%tdata%z%s3(i,:,k),&
                                                &p%of(id)%loc%tdata%z%ss3(i,:,k))
                                                
        end do
        end do
        
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
            p%of(id)%loc%tdata%x%l1(i,j,k) = p%of(id)%loc%tdata%x%l1(i,j,k) - p%of(id)%loc%velsrc%y%now(i,j,k)*p%of(id)%loc%tdata%x%ss1(i,j,k)
            p%of(id)%loc%tdata%x%l2(i,j,k) = p%of(id)%loc%tdata%x%l2(i,j,k) - p%of(id)%loc%velsrc%y%now(i,j,k)*p%of(id)%loc%tdata%x%ss2(i,j,k)
            p%of(id)%loc%tdata%x%l3(i,j,k) = p%of(id)%loc%tdata%x%l3(i,j,k) - p%of(id)%loc%velsrc%y%now(i,j,k)*p%of(id)%loc%tdata%x%ss3(i,j,k)

            p%of(id)%loc%tdata%y%l1(i,j,k) = p%of(id)%loc%tdata%y%l1(i,j,k) - p%of(id)%loc%vel%y%old(i,j,k)*p%of(id)%loc%tdata%y%ss1(i,j,k)
            p%of(id)%loc%tdata%y%l2(i,j,k) = p%of(id)%loc%tdata%y%l2(i,j,k) - p%of(id)%loc%vel%y%old(i,j,k)*p%of(id)%loc%tdata%y%ss2(i,j,k)
            p%of(id)%loc%tdata%y%l3(i,j,k) = p%of(id)%loc%tdata%y%l3(i,j,k) - p%of(id)%loc%vel%y%old(i,j,k)*p%of(id)%loc%tdata%y%ss3(i,j,k)
            
            p%of(id)%loc%tdata%z%l1(i,j,k) = p%of(id)%loc%tdata%z%l1(i,j,k) - p%of(id)%loc%velsrc%y%old(i,j,k)*p%of(id)%loc%tdata%z%ss1(i,j,k)
            p%of(id)%loc%tdata%z%l2(i,j,k) = p%of(id)%loc%tdata%z%l2(i,j,k) - p%of(id)%loc%velsrc%y%old(i,j,k)*p%of(id)%loc%tdata%z%ss2(i,j,k)
            p%of(id)%loc%tdata%z%l3(i,j,k) = p%of(id)%loc%tdata%z%l3(i,j,k) - p%of(id)%loc%velsrc%y%old(i,j,k)*p%of(id)%loc%tdata%z%ss3(i,j,k)
            
        end do
        end do
        end do

        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            ! uz ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%x%s1(i,j,:),&
                                                &p%of(id)%loc%tdata%x%ss1(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%velsrc%z%now(i,j,:))

            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%x%s2(i,j,:),&
                                                &p%of(id)%loc%tdata%x%ss2(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%velsrc%z%now(i,j,:))

            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%x%s3(i,j,:),&
                                                &p%of(id)%loc%tdata%x%ss3(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%velsrc%z%now(i,j,:)) 

            ! vz ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%y%s1(i,j,:),&
                                                &p%of(id)%loc%tdata%y%ss1(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%velsrc%z%old(i,j,:))

            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%y%s2(i,j,:),&
                                                &p%of(id)%loc%tdata%y%ss2(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%velsrc%z%old(i,j,:))

            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%y%s3(i,j,:),&
                                                &p%of(id)%loc%tdata%y%ss3(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%velsrc%z%old(i,j,:))

            ! wz ------------------------------------------------------------------------
            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%z%s1(i,j,:),&
                                                &p%of(id)%loc%tdata%z%ss1(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%vel%z%old(i,j,:)) 

            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%z%s2(i,j,:),&
                                                &p%of(id)%loc%tdata%z%ss2(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%vel%z%old(i,j,:)) 

            call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%tdata%z%s3(i,j,:),&
                                                &p%of(id)%loc%tdata%z%ss3(i,j,:),&
                                                &p%of(id)%loc%vel%z%tmp(i,j,:),&
                                                &p%of(id)%loc%vel%z%old(i,j,:)) 
                                                
        end do
        end do

        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
            p%of(id)%loc%tdata%x%l1(i,j,k) = p%of(id)%loc%tdata%x%l1(i,j,k) - p%of(id)%loc%velsrc%z%now(i,j,k)*p%of(id)%loc%tdata%x%ss1(i,j,k)
            p%of(id)%loc%tdata%x%l2(i,j,k) = p%of(id)%loc%tdata%x%l2(i,j,k) - p%of(id)%loc%velsrc%z%now(i,j,k)*p%of(id)%loc%tdata%x%ss2(i,j,k)
            p%of(id)%loc%tdata%x%l3(i,j,k) = p%of(id)%loc%tdata%x%l3(i,j,k) - p%of(id)%loc%velsrc%z%now(i,j,k)*p%of(id)%loc%tdata%x%ss3(i,j,k)

            p%of(id)%loc%tdata%y%l1(i,j,k) = p%of(id)%loc%tdata%y%l1(i,j,k) - p%of(id)%loc%velsrc%z%old(i,j,k)*p%of(id)%loc%tdata%y%ss1(i,j,k)
            p%of(id)%loc%tdata%y%l2(i,j,k) = p%of(id)%loc%tdata%y%l2(i,j,k) - p%of(id)%loc%velsrc%z%old(i,j,k)*p%of(id)%loc%tdata%y%ss2(i,j,k)
            p%of(id)%loc%tdata%y%l3(i,j,k) = p%of(id)%loc%tdata%y%l3(i,j,k) - p%of(id)%loc%velsrc%z%old(i,j,k)*p%of(id)%loc%tdata%y%ss3(i,j,k)
            
            p%of(id)%loc%tdata%z%l1(i,j,k) = p%of(id)%loc%tdata%z%l1(i,j,k) - p%of(id)%loc%vel%z%old(i,j,k)*p%of(id)%loc%tdata%z%ss1(i,j,k)
            p%of(id)%loc%tdata%z%l2(i,j,k) = p%of(id)%loc%tdata%z%l2(i,j,k) - p%of(id)%loc%vel%z%old(i,j,k)*p%of(id)%loc%tdata%z%ss2(i,j,k)
            p%of(id)%loc%tdata%z%l3(i,j,k) = p%of(id)%loc%tdata%z%l3(i,j,k) - p%of(id)%loc%vel%z%old(i,j,k)*p%of(id)%loc%tdata%z%ss3(i,j,k)
            
        end do
        end do
        end do
     
    enddo           
    !$omp end parallel do
    
end subroutine

subroutine ns_split_diff
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,iter
real(8) :: err

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
   
   do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc 
   do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
   do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        p%of(id)%loc%vel%x%old2(i,j,k) = p%of(id)%loc%vel%x%now(i,j,k)
        p%of(id)%loc%vel%y%old2(i,j,k) = p%of(id)%loc%vel%y%now(i,j,k)
        p%of(id)%loc%vel%z%old2(i,j,k) = p%of(id)%loc%vel%z%now(i,j,k)
   enddo
   enddo
   enddo
        
enddo    
!$omp end parallel do

!==============================================================================

do iter = 1, 5

call ns_linearize

!$omp parallel do 
do id = 0, p%glb%threads-1
        
    call p%of(id)%find_stag_vel( p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                                &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                                &p%of(id)%loc%vel%x%old2, p%of(id)%loc%vel%y%old2, p%of(id)%loc%vel%z%old2 )
enddo       
!$omp end parallel do

call pt%tdatax%sync
call pt%tdatay%sync
call pt%tdataz%sync

!$omp parallel do
do id = 0, p%glb%threads-1
    
    call ns_split_diff_source( p%of(id), &
                              &p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                              &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                              &p%of(id)%loc%vel%x%old2, p%of(id)%loc%vel%y%old2, p%of(id)%loc%vel%z%old2, &
                              &p%of(id)%loc%velsrc%x%tmp, p%of(id)%loc%velsrc%y%tmp, p%of(id)%loc%velsrc%z%tmp, &
                              &p%of(id)%loc%rho%old,p%of(id)%loc%mu%old,p%of(id)%loc%delta%old,p%of(id)%loc%normals%curv%old, &
                              &p%of(id)%loc%normals%x%old, p%of(id)%loc%normals%y%old, p%of(id)%loc%normals%z%old, &
                              &.true., 0.5d0)
    
enddo    
!$omp end parallel do

!$omp parallel do 
do id = 0, p%glb%threads-1
        
    call p%of(id)%find_stag_vel( p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                                &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                                &p%of(id)%loc%vel%x%now, p%of(id)%loc%vel%y%now, p%of(id)%loc%vel%z%now )
enddo       
!$omp end parallel do

call pt%tdatax%sync
call pt%tdatay%sync
call pt%tdataz%sync

!$omp parallel do
do id = 0, p%glb%threads-1
    
    call ns_split_diff_source( p%of(id), &
                              &p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                              &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                              &p%of(id)%loc%vel%x%now, p%of(id)%loc%vel%y%now, p%of(id)%loc%vel%z%now, &
                              &p%of(id)%loc%velsrc%x%tmp, p%of(id)%loc%velsrc%y%tmp, p%of(id)%loc%velsrc%z%tmp, &
                              &p%of(id)%loc%rho%old,p%of(id)%loc%mu%old,p%of(id)%loc%delta%old,p%of(id)%loc%normals%curv%old, &
                              &p%of(id)%loc%normals%x%old, p%of(id)%loc%normals%y%old, p%of(id)%loc%normals%z%old, &
                              &.false., 0.5d0)
    
enddo    
!$omp end parallel do


!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        p%of(id)%loc%vel%x%now(i,j,k) = p%of(id)%loc%vel%x%old2(i,j,k) + p%of(id)%loc%velsrc%x%tmp(i,j,k) * p%glb%dt                      
        p%of(id)%loc%vel%y%now(i,j,k) = p%of(id)%loc%vel%y%old2(i,j,k) + p%of(id)%loc%velsrc%y%tmp(i,j,k) * p%glb%dt                      
        p%of(id)%loc%vel%z%now(i,j,k) = p%of(id)%loc%vel%z%old2(i,j,k) + p%of(id)%loc%velsrc%z%tmp(i,j,k) * p%glb%dt

    end do
    end do
    end do
        
    call p%of(id)%velbc(p%of(id)%loc%vel%x%now,p%of(id)%loc%vel%y%now,p%of(id)%loc%vel%z%now)
    
enddo
!$omp end parallel do
    
call pt%vel%sync

end do

end subroutine

subroutine ns_split_diff_source(q,u,v,w,uu,vv,ww,us,vs,ws,sx,sy,sz,&
    irho,imu,idelta,icurv,iphix,iphiy,iphiz,reset,alpha)
use all
implicit none
type(job) :: q
real(8), dimension(q%loc%is-q%glb%ghc:q%loc%ie+q%glb%ghc,&
                  &q%loc%js-q%glb%ghc:q%loc%je+q%glb%ghc,&
                  &q%loc%ks-q%glb%ghc:q%loc%ke+q%glb%ghc) :: u,v,w,uu,vv,ww,us,vs,ws,sx,sy,sz,irho,imu,idelta,icurv,iphix,iphiy,iphiz,tmp
logical :: reset
real(8) :: alpha
real(8) :: rho,mu,delta,curv,xx,yy,zz
real(8) :: ux,uy,uz,vx,vy,vz,wx,wy,wz,phix,phiy,phiz
real(8) :: dif_x,dif_y,dif_z             
integer :: i,j,k

if(reset)then

    do k = q%loc%ks, q%loc%ke
    do j = q%loc%js, q%loc%je
    do i = q%loc%is, q%loc%ie
        sx(i,j,k) = 0.0d0
        sy(i,j,k) = 0.0d0
        sz(i,j,k) = 0.0d0
    enddo
    enddo
    enddo

endif

! ---------------------------------------------- Calculate derivatives

    do k = q%loc%ks, q%loc%ke
    do j = q%loc%js, q%loc%je
        call q%loc%ccdsolvers%x%solve("uccd",us(:,j,k),q%loc%vel_ten%xx(:,j,k),q%loc%vel_ten%xxx(:,j,k),us(:,j,k))
        call q%loc%ccdsolvers%x%solve("uccd",vs(:,j,k),q%loc%vel_ten%yx(:,j,k),q%loc%vel_ten%yxx(:,j,k), u(:,j,k))
        call q%loc%ccdsolvers%x%solve("uccd",ws(:,j,k),q%loc%vel_ten%zx(:,j,k),q%loc%vel_ten%zxx(:,j,k),uu(:,j,k))
        
        call q%loc%ccdsolvers%x%solve("uccd", v(:,j,k),q%loc%tdata%y%l1(:,j,k),tmp(:,j,k),us(:,j,k))
        call q%loc%ccdsolvers%x%solve("uccd", w(:,j,k),q%loc%tdata%z%l1(:,j,k),tmp(:,j,k),us(:,j,k))
    end do
    end do

    do k = q%loc%ks, q%loc%ke
    do i = q%loc%is, q%loc%ie
        call q%loc%ccdsolvers%y%solve("uccd",us(i,:,k),q%loc%vel_ten%xy(i,:,k),q%loc%vel_ten%xyy(i,:,k), v(i,:,k))
        call q%loc%ccdsolvers%y%solve("uccd",vs(i,:,k),q%loc%vel_ten%yy(i,:,k),q%loc%vel_ten%yyy(i,:,k),vs(i,:,k))
        call q%loc%ccdsolvers%y%solve("uccd",ws(i,:,k),q%loc%vel_ten%zy(i,:,k),q%loc%vel_ten%zyy(i,:,k),vv(i,:,k))
        
        call q%loc%ccdsolvers%y%solve("uccd", u(i,:,k),q%loc%tdata%x%l2(i,:,k),tmp(i,:,k),vs(i,:,k))
        call q%loc%ccdsolvers%y%solve("uccd",ww(i,:,k),q%loc%tdata%z%l2(i,:,k),tmp(i,:,k),vs(i,:,k))
    end do
    end do

    do j = q%loc%js, q%loc%je
    do i = q%loc%is, q%loc%ie
        call q%loc%ccdsolvers%z%solve("uccd",us(i,:,k),q%loc%vel_ten%xz(i,j,:),q%loc%vel_ten%xzz(i,j,:), w(i,:,k))
        call q%loc%ccdsolvers%z%solve("uccd",vs(i,:,k),q%loc%vel_ten%yz(i,j,:),q%loc%vel_ten%yzz(i,j,:),ww(i,:,k))
        call q%loc%ccdsolvers%z%solve("uccd",ws(i,:,k),q%loc%vel_ten%zz(i,j,:),q%loc%vel_ten%zzz(i,j,:),ws(i,:,k))
        
        call q%loc%ccdsolvers%z%solve("uccd",uu(i,j,:),q%loc%tdata%x%l3(i,j,:),tmp(i,j,:),ws(i,j,:))
        call q%loc%ccdsolvers%z%solve("uccd",vv(i,j,:),q%loc%tdata%y%l3(i,j,:),tmp(i,j,:),ws(i,j,:))
    end do
    end do

! ----------------------------------------------
    
    do k = q%loc%ks, q%loc%ke
    do j = q%loc%js, q%loc%je
    do i = q%loc%is, q%loc%ie
        
        rho = (irho(i,j,k)+irho(i+1,j,k))/2.0d0
        mu  = (imu(i,j,k)+imu(i+1,j,k))/2.0d0
        delta = (idelta(i,j,k)+idelta(i+1,j,k))/2.0d0
        curv = (icurv(i,j,k)+icurv(i+1,j,k))/2.0d0
        
        xx = q%loc%vel_ten%xxx(i,j,k); yy = q%loc%vel_ten%xyy(i,j,k); zz = q%loc%vel_ten%xzz(i,j,k)
        ux = q%loc%vel_ten%xx(i,j,k) ; uy = q%loc%vel_ten%xy(i,j,k) ; uz = q%loc%vel_ten%xz(i,j,k)
        vx = q%loc%tdata%y%l1(i,j,k)
        wx = q%loc%tdata%z%l1(i,j,k)
        
        phix = 0.5d0*( iphix(i,j,k)+iphix(i+1,j,k) )
        phiy = 0.5d0*( iphiy(i,j,k)+iphiy(i+1,j,k) )
        phiz = 0.5d0*( iphiz(i,j,k)+iphiz(i+1,j,k) )
        
        dif_x = mu/rho*xx/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phix*2.0d0*ux/(rho*q%glb%re)
        dif_y = mu/rho*yy/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phiy*(uy+vx)/(rho*q%glb%re)
        dif_z = mu/rho*zz/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phiz*(uz+wx)/(rho*q%glb%re)
            
        sx(i,j,k) = sx(i,j,k) + alpha * ( dif_x + dif_y + dif_z + q%glb%gx*q%glb%btn_g/q%glb%fr - q%glb%btn_sf*curv*delta*phix / (q%glb%we*rho) )
        
    end do
    end do
    end do

    do k = q%loc%ks, q%loc%ke
    do j = q%loc%js, q%loc%je
    do i = q%loc%is, q%loc%ie
        
        rho = (irho(i,j,k)+irho(i,j+1,k))/2.0d0
        mu  = (imu(i,j,k)+imu(i,j+1,k))/2.0d0
        delta = (idelta(i,j,k)+idelta(i,j+1,k))/2.0d0
        curv = (icurv(i,j,k)+icurv(i,j+1,k))/2.0d0
   
        xx = q%loc%vel_ten%yxx(i,j,k); yy = q%loc%vel_ten%yyy(i,j,k); zz = q%loc%vel_ten%yzz(i,j,k)
        vx = q%loc%vel_ten%yx(i,j,k) ; vy = q%loc%vel_ten%yy(i,j,k) ; vz = q%loc%vel_ten%yz(i,j,k)
        uy = q%loc%tdata%x%l2(i,j,k)
        wy = q%loc%tdata%z%l2(i,j,k)
        
        phix = 0.5d0*( iphix(i,j,k)+iphix(i,j+1,k) )
        phiy = 0.5d0*( iphiy(i,j,k)+iphiy(i,j+1,k) )
        phiz = 0.5d0*( iphiz(i,j,k)+iphiz(i,j+1,k) )
            
        dif_x = mu/rho*xx/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phix*(uy+vx)/(rho*q%glb%re)
        dif_y = mu/rho*yy/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phiy*2.0d0*vy/(rho*q%glb%re)
        dif_z = mu/rho*zz/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phiz*(wy+vz)/(rho*q%glb%re)
            
        sy(i,j,k) = sy(i,j,k) + alpha * ( dif_x + dif_y + dif_z + q%glb%gy*q%glb%btn_g/q%glb%fr - q%glb%btn_sf*curv*delta*phiy / (q%glb%we*rho) )
        
    end do
    end do
    end do

    do k = q%loc%ks, q%loc%ke
    do j = q%loc%js, q%loc%je
    do i = q%loc%is, q%loc%ie
        
        rho = (irho(i,j,k)+irho(i,j,k+1))/2.0d0
        mu  = (imu(i,j,k)+imu(i,j,k+1))/2.0d0
        delta = (idelta(i,j,k)+idelta(i,j,k+1))/2.0d0
        curv = (icurv(i,j,k)+icurv(i,j,k+1))/2.0d0

        xx = q%loc%vel_ten%zxx(i,j,k); yy = q%loc%vel_ten%zyy(i,j,k); zz = q%loc%vel_ten%zzz(i,j,k)
        wx = q%loc%vel_ten%zx(i,j,k) ; wy = q%loc%vel_ten%zy(i,j,k) ; wz = q%loc%vel_ten%zz(i,j,k)
        uz = q%loc%tdata%x%l3(i,j,k)
        vz = q%loc%tdata%y%l3(i,j,k)
        
        phix = 0.5d0*( iphix(i,j,k)+iphix(i,j,k+1) )
        phiy = 0.5d0*( iphiy(i,j,k)+iphiy(i,j,k+1) )
        phiz = 0.5d0*( iphiz(i,j,k)+iphiz(i,j,k+1) )
            
        dif_x = mu/rho*xx/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phix*(uz+wx)/(rho*q%glb%re)
        dif_y = mu/rho*yy/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phiy*(vz+wy)/(rho*q%glb%re)
        dif_z = mu/rho*zz/q%glb%re + (1.0d0-q%glb%mu_12)*delta*phiz*2.0d0*wz/(rho*q%glb%re)
            
        sz(i,j,k) = sz(i,j,k) + alpha * ( dif_x + dif_y + dif_z + q%glb%gz*q%glb%btn_g/q%glb%fr - q%glb%btn_sf*curv*delta*phiz / (q%glb%we*rho) )
        
    end do
    end do
    end do
    
end subroutine


