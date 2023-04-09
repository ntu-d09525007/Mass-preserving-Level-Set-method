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

id=0
   
call p%loc%tdata%init(.true.,p%loc%vel%x%old,p%loc%vel%y%old,p%loc%vel%z%old)

call find_stag_vel( p%loc%velsrc%x%now, p%loc%velsrc%y%now, p%loc%velsrc%z%now, &
                   &p%loc%velsrc%x%old, p%loc%velsrc%y%old, p%loc%velsrc%z%old, &
                   &p%loc%vel%x%old   , p%loc%vel%y%old   , p%loc%vel%z%old )
   
iter=0
    
do 

    iter = iter + 1
    
    call ns_split_adv_source()
    
    err=0.0_8  
    call p%loc%tdata%solve_srk6(err)

    if( err < p%glb%t_tol )exit
    
    if( mod(iter,100) == 0)write(*,*)"NS adv solver,",iter,err
    
end do

call p%loc%tdata%final_srk6()

!$omp parallel do collapse(3)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    p%loc%vel%x%now(i,j,k) = p%loc%tdata%x%target(i,j,k)
    p%loc%vel%y%now(i,j,k) = p%loc%tdata%y%target(i,j,k)
    p%loc%vel%z%now(i,j,k) = p%loc%tdata%z%target(i,j,k)
    
end do
end do
end do
!$omp end parallel do

call velbc(p%loc%vel%x%now,p%loc%vel%y%now,p%loc%vel%z%now)
        
end subroutine

subroutine ns_split_adv_source
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc)  :: tmp

id=0

call bc(p%loc%tdata%x%s1);call bc(p%loc%tdata%x%s2);call bc(p%loc%tdata%x%s3)
call bc(p%loc%tdata%y%s1);call bc(p%loc%tdata%y%s2);call bc(p%loc%tdata%y%s3)
call bc(p%loc%tdata%z%s1);call bc(p%loc%tdata%z%s2);call bc(p%loc%tdata%z%s3)

!$omp parallel do collapse(2)        
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je

    ! ux ------------------------------------------------------------------------
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%x%s1(:,j,k),p%loc%tdata%x%ss1(:,j,k),tmp(:,j,k),p%loc%vel%x%old(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%x%s2(:,j,k),p%loc%tdata%x%ss2(:,j,k),tmp(:,j,k),p%loc%vel%x%old(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%x%s3(:,j,k),p%loc%tdata%x%ss3(:,j,k),tmp(:,j,k),p%loc%vel%x%old(:,j,k))
    
    ! vx ------------------------------------------------------------------------
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%y%s1(:,j,k),p%loc%tdata%y%ss1(:,j,k),tmp(:,j,k),p%loc%velsrc%x%now(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%y%s2(:,j,k),p%loc%tdata%y%ss2(:,j,k),tmp(:,j,k),p%loc%velsrc%x%now(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%y%s3(:,j,k),p%loc%tdata%y%ss3(:,j,k),tmp(:,j,k),p%loc%velsrc%x%now(:,j,k))

    ! wx ------------------------------------------------------------------------ 
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%z%s1(:,j,k),p%loc%tdata%z%ss1(:,j,k),tmp(:,j,k),p%loc%velsrc%x%old(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%z%s2(:,j,k),p%loc%tdata%z%ss2(:,j,k),tmp(:,j,k),p%loc%velsrc%x%old(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",p%loc%tdata%z%s3(:,j,k),p%loc%tdata%z%ss3(:,j,k),tmp(:,j,k),p%loc%velsrc%x%old(:,j,k))       
    
end do
end do
!$omp end parallel do

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%tdata%x%l1(i,j,k) = - p%loc%vel%x%old(i,j,k)*p%loc%tdata%x%ss1(i,j,k)
    p%loc%tdata%x%l2(i,j,k) = - p%loc%vel%x%old(i,j,k)*p%loc%tdata%x%ss2(i,j,k)
    p%loc%tdata%x%l3(i,j,k) = - p%loc%vel%x%old(i,j,k)*p%loc%tdata%x%ss3(i,j,k)

    p%loc%tdata%y%l1(i,j,k) = - p%loc%velsrc%x%now(i,j,k)*p%loc%tdata%y%ss1(i,j,k)
    p%loc%tdata%y%l2(i,j,k) = - p%loc%velsrc%x%now(i,j,k)*p%loc%tdata%y%ss2(i,j,k)
    p%loc%tdata%y%l3(i,j,k) = - p%loc%velsrc%x%now(i,j,k)*p%loc%tdata%y%ss3(i,j,k)

    p%loc%tdata%z%l1(i,j,k) = - p%loc%velsrc%x%old(i,j,k)*p%loc%tdata%z%ss1(i,j,k)
    p%loc%tdata%z%l2(i,j,k) = - p%loc%velsrc%x%old(i,j,k)*p%loc%tdata%z%ss2(i,j,k)
    p%loc%tdata%z%l3(i,j,k) = - p%loc%velsrc%x%old(i,j,k)*p%loc%tdata%z%ss3(i,j,k)
    
end do
end do
end do
!$omp end parallel do

!$omp parallel do collapse(2)   
do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie

    ! uy ------------------------------------------------------------------------
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%y%s1(i,:,k),p%loc%tdata%y%ss1(i,:,k),tmp(i,:,k),p%loc%velsrc%y%now(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%y%s2(i,:,k),p%loc%tdata%y%ss2(i,:,k),tmp(i,:,k),p%loc%velsrc%y%now(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%y%s3(i,:,k),p%loc%tdata%y%ss3(i,:,k),tmp(i,:,k),p%loc%velsrc%y%now(i,:,k))
    
    ! vy ------------------------------------------------------------------------
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%y%s1(i,:,k),p%loc%tdata%y%ss1(i,:,k),tmp(i,:,k),p%loc%vel%y%old(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%y%s2(i,:,k),p%loc%tdata%y%ss2(i,:,k),tmp(i,:,k),p%loc%vel%y%old(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%y%s3(i,:,k),p%loc%tdata%y%ss3(i,:,k),tmp(i,:,k),p%loc%vel%y%old(i,:,k))

    ! wy ------------------------------------------------------------------------ 
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%z%s1(i,:,k),p%loc%tdata%z%ss1(i,:,k),tmp(i,:,k),p%loc%velsrc%y%old(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%z%s2(i,:,k),p%loc%tdata%z%ss2(i,:,k),tmp(i,:,k),p%loc%velsrc%y%old(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",p%loc%tdata%z%s3(i,:,k),p%loc%tdata%z%ss3(i,:,k),tmp(i,:,k),p%loc%velsrc%y%old(i,:,k)) 

end do
end do
!$omp end parallel do

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%tdata%x%l1(i,j,k) = p%loc%tdata%x%l1(i,j,k) - p%loc%velsrc%y%now(i,j,k)*p%loc%tdata%x%ss1(i,j,k)
    p%loc%tdata%x%l2(i,j,k) = p%loc%tdata%x%l2(i,j,k) - p%loc%velsrc%y%now(i,j,k)*p%loc%tdata%x%ss2(i,j,k)
    p%loc%tdata%x%l3(i,j,k) = p%loc%tdata%x%l3(i,j,k) - p%loc%velsrc%y%now(i,j,k)*p%loc%tdata%x%ss3(i,j,k)

    p%loc%tdata%y%l1(i,j,k) = p%loc%tdata%y%l1(i,j,k) - p%loc%vel%y%old(i,j,k)*p%loc%tdata%y%ss1(i,j,k)
    p%loc%tdata%y%l2(i,j,k) = p%loc%tdata%y%l2(i,j,k) - p%loc%vel%y%old(i,j,k)*p%loc%tdata%y%ss2(i,j,k)
    p%loc%tdata%y%l3(i,j,k) = p%loc%tdata%y%l3(i,j,k) - p%loc%vel%y%old(i,j,k)*p%loc%tdata%y%ss3(i,j,k)
    
    p%loc%tdata%z%l1(i,j,k) = p%loc%tdata%z%l1(i,j,k) - p%loc%velsrc%y%old(i,j,k)*p%loc%tdata%z%ss1(i,j,k)
    p%loc%tdata%z%l2(i,j,k) = p%loc%tdata%z%l2(i,j,k) - p%loc%velsrc%y%old(i,j,k)*p%loc%tdata%z%ss2(i,j,k)
    p%loc%tdata%z%l3(i,j,k) = p%loc%tdata%z%l3(i,j,k) - p%loc%velsrc%y%old(i,j,k)*p%loc%tdata%z%ss3(i,j,k)
    
end do
end do
end do
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    
    ! uz ------------------------------------------------------------------------
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%x%s1(i,j,:),p%loc%tdata%x%ss1(i,j,:),tmp(i,j,:),p%loc%velsrc%z%now(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%x%s2(i,j,:),p%loc%tdata%x%ss2(i,j,:),tmp(i,j,:),p%loc%velsrc%z%now(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%x%s3(i,j,:),p%loc%tdata%x%ss3(i,j,:),tmp(i,j,:),p%loc%velsrc%z%now(i,j,:))
    
    ! vz ------------------------------------------------------------------------
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%y%s1(i,j,:),p%loc%tdata%y%ss1(i,j,:),tmp(i,j,:),p%loc%velsrc%z%old(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%y%s2(i,j,:),p%loc%tdata%y%ss2(i,j,:),tmp(i,j,:),p%loc%velsrc%z%old(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%y%s3(i,j,:),p%loc%tdata%y%ss3(i,j,:),tmp(i,j,:),p%loc%velsrc%z%old(i,j,:))

    ! wz ------------------------------------------------------------------------ 
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%z%s1(i,j,:),p%loc%tdata%z%ss1(i,j,:),tmp(i,j,:),p%loc%vel%z%old(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%z%s2(i,j,:),p%loc%tdata%z%ss2(i,j,:),tmp(i,j,:),p%loc%vel%z%old(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",p%loc%tdata%z%s3(i,j,:),p%loc%tdata%z%ss3(i,j,:),tmp(i,j,:),p%loc%vel%z%old(i,j,:))

                                        
end do
end do
!$omp end parallel do

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%tdata%x%l1(i,j,k) = p%loc%tdata%x%l1(i,j,k) - p%loc%velsrc%z%now(i,j,k)*p%loc%tdata%x%ss1(i,j,k)
    p%loc%tdata%x%l2(i,j,k) = p%loc%tdata%x%l2(i,j,k) - p%loc%velsrc%z%now(i,j,k)*p%loc%tdata%x%ss2(i,j,k)
    p%loc%tdata%x%l3(i,j,k) = p%loc%tdata%x%l3(i,j,k) - p%loc%velsrc%z%now(i,j,k)*p%loc%tdata%x%ss3(i,j,k)

    p%loc%tdata%y%l1(i,j,k) = p%loc%tdata%y%l1(i,j,k) - p%loc%velsrc%z%old(i,j,k)*p%loc%tdata%y%ss1(i,j,k)
    p%loc%tdata%y%l2(i,j,k) = p%loc%tdata%y%l2(i,j,k) - p%loc%velsrc%z%old(i,j,k)*p%loc%tdata%y%ss2(i,j,k)
    p%loc%tdata%y%l3(i,j,k) = p%loc%tdata%y%l3(i,j,k) - p%loc%velsrc%z%old(i,j,k)*p%loc%tdata%y%ss3(i,j,k)
    
    p%loc%tdata%z%l1(i,j,k) = p%loc%tdata%z%l1(i,j,k) - p%loc%vel%z%old(i,j,k)*p%loc%tdata%z%ss1(i,j,k)
    p%loc%tdata%z%l2(i,j,k) = p%loc%tdata%z%l2(i,j,k) - p%loc%vel%z%old(i,j,k)*p%loc%tdata%z%ss2(i,j,k)
    p%loc%tdata%z%l3(i,j,k) = p%loc%tdata%z%l3(i,j,k) - p%loc%vel%z%old(i,j,k)*p%loc%tdata%z%ss3(i,j,k)
    
end do
end do
end do
!$omp end parallel do
    
end subroutine

subroutine ns_split_diff
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k,iter
real(8) :: err

id=0

!$omp parallel do collapse(3)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc 
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    p%loc%vel%x%old2(i,j,k) = p%loc%vel%x%now(i,j,k)
    p%loc%vel%y%old2(i,j,k) = p%loc%vel%y%now(i,j,k)
    p%loc%vel%z%old2(i,j,k) = p%loc%vel%z%now(i,j,k)
enddo
enddo
enddo  
!$omp end parallel do

!==============================================================================

do iter = 1, 5

call ns_linearize

!$omp parallel do collapse(3)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc 
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    p%loc%vel%x%now(i,j,k) = 0.5d0*( p%loc%vel%x%now(i,j,k) + p%loc%vel%x%old2(i,j,k) ) 
    p%loc%vel%y%now(i,j,k) = 0.5d0*( p%loc%vel%y%now(i,j,k) + p%loc%vel%y%old2(i,j,k) )
    p%loc%vel%z%now(i,j,k) = 0.5d0*( p%loc%vel%z%now(i,j,k) + p%loc%vel%z%old2(i,j,k) )
enddo
enddo
enddo
!$omp end parallel do

call find_stag_vel( p%loc%velsrc%x%now, p%loc%velsrc%y%now, p%loc%velsrc%z%now, &
                &p%loc%velsrc%x%old, p%loc%velsrc%y%old, p%loc%velsrc%z%old, &
                &p%loc%vel%x%now   , p%loc%vel%y%now   , p%loc%vel%z%now )

call ns_split_diff_source( p%loc%velsrc%x%now, p%loc%velsrc%y%now, p%loc%velsrc%z%now, &
                          &p%loc%velsrc%x%old, p%loc%velsrc%y%old, p%loc%velsrc%z%old, &
                          &p%loc%vel%x%now   , p%loc%vel%y%now   , p%loc%vel%z%now   , &
                          &p%loc%velsrc%x%tmp, p%loc%velsrc%y%tmp, p%loc%velsrc%z%tmp, &
                          &p%loc%rho%old,p%loc%mu%old,p%loc%delta%old,p%loc%normals%curv%old)

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    p%loc%vel%x%now(i,j,k) = p%loc%vel%x%old2(i,j,k) + p%loc%velsrc%x%tmp(i,j,k) * p%glb%dt                      
    p%loc%vel%y%now(i,j,k) = p%loc%vel%y%old2(i,j,k) + p%loc%velsrc%y%tmp(i,j,k) * p%glb%dt                      
    p%loc%vel%z%now(i,j,k) = p%loc%vel%z%old2(i,j,k) + p%loc%velsrc%z%tmp(i,j,k) * p%glb%dt

end do
end do
end do
!$omp end parallel do
    
call velbc(p%loc%vel%x%now,p%loc%vel%y%now,p%loc%vel%z%now)
    
end do

end subroutine

subroutine ns_split_diff_source(u,v,w,uu,vv,ww,us,vs,ws,sx,sy,sz,irho,imu,idelta,icurv)
use all
implicit none
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w,uu,vv,ww,us,vs,ws,sx,sy,sz,irho,imu,idelta,icurv,tmp
real(8) :: rho,mu,delta,curv,xx,yy,zz
real(8) :: ux,uy,uz,vx,vy,vz,wx,wy,wz,phix,phiy,phiz
real(8) :: dif_x,dif_y,dif_z             
integer :: i,j,k

! ---------------------------------------------- Calculate derivatives
!$omp parallel do collapse(2)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
    call p%loc%ccdsolvers%x%solve("srkccd",us(:,j,k),p%loc%tdata%x%s1(:,j,k),p%loc%tdata%x%ss1(:,j,k),us(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",vs(:,j,k),p%loc%tdata%y%s1(:,j,k),p%loc%tdata%y%ss1(:,j,k),vs(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",ws(:,j,k),p%loc%tdata%z%s1(:,j,k),p%loc%tdata%z%ss1(:,j,k),ws(:,j,k))
    
    call p%loc%ccdsolvers%x%solve("srkccd",v(:,j,k),p%loc%tdata%y%l1(:,j,k),tmp(:,j,k),v(:,j,k))
    call p%loc%ccdsolvers%x%solve("srkccd",w(:,j,k),p%loc%tdata%z%l1(:,j,k),tmp(:,j,k),w(:,j,k))
end do
end do
!$omp end parallel do

!$omp parallel do collapse(2)
do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%y%solve("srkccd",us(i,:,k),p%loc%tdata%x%s2(i,:,k),p%loc%tdata%x%ss2(i,:,k),us(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",vs(i,:,k),p%loc%tdata%y%s2(i,:,k),p%loc%tdata%y%ss2(i,:,k),vs(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",ws(i,:,k),p%loc%tdata%z%s2(i,:,k),p%loc%tdata%z%ss2(i,:,k),ws(i,:,k))
    
    call p%loc%ccdsolvers%y%solve("srkccd", u(i,:,k),p%loc%tdata%x%l1(i,:,k),tmp(i,:,k),u(i,:,k))
    call p%loc%ccdsolvers%y%solve("srkccd",ww(i,:,k),p%loc%tdata%z%l2(i,:,k),tmp(i,:,k),ww(i,:,k))
end do
end do
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%z%solve("srkccd",us(i,j,:),p%loc%tdata%x%s3(i,j,:),p%loc%tdata%x%ss3(i,j,:),us(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",vs(i,j,:),p%loc%tdata%y%s3(i,j,:),p%loc%tdata%y%ss3(i,j,:),vs(i,j,:))
    call p%loc%ccdsolvers%z%solve("srkccd",ws(i,j,:),p%loc%tdata%z%s3(i,j,:),p%loc%tdata%z%ss3(i,j,:),ws(i,j,:))
    
    call p%loc%ccdsolvers%y%solve("srkccd",uu(i,j,:),p%loc%tdata%x%l2(i,j,:),tmp(i,j,:),uu(i,j,:))
    call p%loc%ccdsolvers%y%solve("srkccd",vv(i,j,:),p%loc%tdata%y%l2(i,j,:),tmp(i,j,:),vv(i,j,:))
end do
end do
!$omp end parallel do
! ----------------------------------------------

!$omp parallel do collapse(3), private(rho,mu,delta,curv,xx,yy,zz,ux,uy,uz,vx,vy,vz,phix,phiy,phiz,dif_x,dif_y,dif_z)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    rho = (irho(i,j,k)+irho(i+1,j,k))/2.0d0
    mu  = (imu(i,j,k)+imu(i+1,j,k))/2.0d0
    delta = (idelta(i,j,k)+idelta(i+1,j,k))/2.0d0
    curv = (icurv(i,j,k)+icurv(i+1,j,k))/2.0d0
    
    xx = p%loc%tdata%x%ss1(i,j,k); yy = p%loc%tdata%x%ss2(i,j,k); zz = p%loc%tdata%x%ss3(i,j,k)
    ux = p%loc%tdata%x%s1(i,j,k) ; uy = p%loc%tdata%x%s2(i,j,k) ; uz = p%loc%tdata%x%s3(i,j,k)
    vx = p%loc%tdata%y%l1(i,j,k)
    wx = p%loc%tdata%z%l1(i,j,k)
    
    phix = 0.5d0*( p%loc%normals%x%old(i,j,k)+p%loc%normals%x%old(i+1,j,k) )
    phiy = 0.5d0*( p%loc%normals%y%old(i,j,k)+p%loc%normals%y%old(i+1,j,k) )
    phiz = 0.5d0*( p%loc%normals%z%old(i,j,k)+p%loc%normals%z%old(i+1,j,k) )
    
    dif_x = mu/rho*xx/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phix*2.0d0*ux/(rho*p%glb%re)
    dif_y = mu/rho*yy/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phiy*(uy+vx)/(rho*p%glb%re)
    dif_z = mu/rho*zz/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phiz*(uz+wx)/(rho*p%glb%re)
        
    sx(i,j,k) = dif_x + dif_y + dif_z + p%glb%gx*p%glb%btn_g/p%glb%fr - p%glb%btn_sf*curv*delta*phix / (p%glb%we*rho)
    
end do
end do
end do
!$omp end parallel do

!$omp parallel do collapse(3), private(rho,mu,delta,curv,xx,yy,zz,ux,uy,uz,vx,vy,vz,phix,phiy,phiz,dif_x,dif_y,dif_z)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    rho = (irho(i,j,k)+irho(i,j+1,k))/2.0d0
    mu  = (imu(i,j,k)+imu(i,j+1,k))/2.0d0
    delta = (idelta(i,j,k)+idelta(i,j+1,k))/2.0d0
    curv = (icurv(i,j,k)+icurv(i,j+1,k))/2.0d0

    xx = p%loc%tdata%y%ss1(i,j,k); yy = p%loc%tdata%y%ss2(i,j,k); zz = p%loc%tdata%y%ss3(i,j,k)
    vx = p%loc%tdata%y%s1(i,j,k) ; vy = p%loc%tdata%y%s2(i,j,k) ; vz = p%loc%tdata%y%s3(i,j,k)
    uy = p%loc%tdata%x%l1(i,j,k)
    wy = p%loc%tdata%z%l2(i,j,k)
    
    phix = 0.5d0*( p%loc%normals%x%old(i,j,k)+p%loc%normals%x%old(i,j+1,k) )
    phiy = 0.5d0*( p%loc%normals%y%old(i,j,k)+p%loc%normals%y%old(i,j+1,k) )
    phiz = 0.5d0*( p%loc%normals%z%old(i,j,k)+p%loc%normals%z%old(i,j+1,k) )
        
    dif_x = mu/rho*xx/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phix*(uy+vx)/(rho*p%glb%re)
    dif_y = mu/rho*yy/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phiy*2.0d0*vy/(rho*p%glb%re)
    dif_z = mu/rho*zz/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phiz*(wy+vz)/(rho*p%glb%re)
        
    sy(i,j,k) = dif_x + dif_y + dif_z + p%glb%gy*p%glb%btn_g/p%glb%fr - p%glb%btn_sf*curv*delta*phiy / (p%glb%we*rho)
    
end do
end do
end do
!$omp end parallel do

!$omp parallel do collapse(3), private(rho,mu,delta,curv,xx,yy,zz,ux,uy,uz,vx,vy,vz,phix,phiy,phiz,dif_x,dif_y,dif_z)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    rho = (irho(i,j,k)+irho(i,j,k+1))/2.0d0
    mu  = (imu(i,j,k)+imu(i,j,k+1))/2.0d0
    delta = (idelta(i,j,k)+idelta(i,j,k+1))/2.0d0
    curv = (icurv(i,j,k)+icurv(i,j,k+1))/2.0d0

    xx = p%loc%tdata%z%ss1(i,j,k); yy = p%loc%tdata%z%ss2(i,j,k); zz = p%loc%tdata%z%ss3(i,j,k)
    wx = p%loc%tdata%z%s1(i,j,k) ; wy = p%loc%tdata%z%s2(i,j,k) ; wz = p%loc%tdata%z%s3(i,j,k)
    uz = p%loc%tdata%x%l2(i,j,k)
    vz = p%loc%tdata%y%l2(i,j,k)
    
    phix = 0.5d0*( p%loc%normals%x%old(i,j,k)+p%loc%normals%x%old(i,j,k+1) )
    phiy = 0.5d0*( p%loc%normals%y%old(i,j,k)+p%loc%normals%y%old(i,j,k+1) )
    phiz = 0.5d0*( p%loc%normals%z%old(i,j,k)+p%loc%normals%z%old(i,j,k+1) )
        
    dif_x = mu/rho*xx/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phix*(uz+wx)/(rho*p%glb%re)
    dif_y = mu/rho*yy/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phiy*(vz+wy)/(rho*p%glb%re)
    dif_z = mu/rho*zz/p%glb%re + (1.0d0-p%glb%mu_12)*delta*phiz*2.0d0*wz/(rho*p%glb%re)
        
    sz(i,j,k) = dif_x + dif_y + dif_z + p%glb%gz*p%glb%btn_g/p%glb%fr - p%glb%btn_sf*curv*delta*phiz / (p%glb%we*rho)
    
end do
end do
end do
!$omp end parallel do
    
end subroutine


