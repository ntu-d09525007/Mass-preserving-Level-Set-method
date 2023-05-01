subroutine ns_ab_adv_source
implicit none

call ns_ab_adv_source_sec
!call ns_ab_adv_source_quick
! call ns_ab_adv_source_uccd
    
end subroutine

subroutine ns_ab_adv_source_sec
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: u,v,w,xp,xm,yp,ym,zp,zm

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
        
    call p%of(id)%find_stag_vel( p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                                &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                                &p%of(id)%loc%vel%x%tmp, p%of(id)%loc%vel%y%tmp, p%of(id)%loc%vel%z%tmp )
enddo       
!$omp end parallel do
    
call pt%tdatax%sync
call pt%tdatay%sync
call pt%tdataz%sync
    
!$omp parallel do private(i,j,k,u,v,w,xp,xm,yp,ym,zp,zm)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,u,v,w,xp,xm,yp,ym,zp,zm)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        u = p%of(id)%loc%vel%x%tmp(i,j,k)
        v = p%of(id)%loc%tdata%y%s1(i,j,k)
        w = p%of(id)%loc%tdata%z%s1(i,j,k)
            
        xp = 0.5d0*(-p%of(id)%loc%vel%x%old(i+2,j,k)+4.0d0*p%of(id)%loc%vel%x%old(i+1,j,k)-3.0d0*p%of(id)%loc%vel%x%old(i,j,k))/p%glb%dx 
        xm = 0.5d0*( p%of(id)%loc%vel%x%old(i-2,j,k)-4.0d0*p%of(id)%loc%vel%x%old(i-1,j,k)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k))/p%glb%dx
        
        yp = 0.5d0*(-p%of(id)%loc%vel%x%old(i,j+2,k)+4.0d0*p%of(id)%loc%vel%x%old(i,j+1,k)-3.0d0*p%of(id)%loc%vel%x%old(i,j,k))/p%glb%dy
        ym = 0.5d0*( p%of(id)%loc%vel%x%old(i,j-2,k)-4.0d0*p%of(id)%loc%vel%x%old(i,j-1,k)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k))/p%glb%dy
            
        zp = 0.5d0*(-p%of(id)%loc%vel%x%old(i,j,k+2)+4.0d0*p%of(id)%loc%vel%x%old(i,j,k+1)-3.0d0*p%of(id)%loc%vel%x%old(i,j,k))/p%glb%dz
        zm = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k-2)-4.0d0*p%of(id)%loc%vel%x%old(i,j,k-1)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k))/p%glb%dz

        p%of(id)%loc%velsrc%x%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                                        &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                                        &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0  
                
        !-----------------------------------------------------------
            
        u = p%of(id)%loc%tdata%x%s1(i,j,k)
        v = p%of(id)%loc%vel%y%tmp(i,j,k)
        w = p%of(id)%loc%tdata%z%s2(i,j,k)
            
        xp = 0.5d0*(-p%of(id)%loc%vel%y%old(i+2,j,k)+4.0d0*p%of(id)%loc%vel%y%old(i+1,j,k)-3.0d0*p%of(id)%loc%vel%y%old(i,j,k))/p%glb%dx 
        xm = 0.5d0*( p%of(id)%loc%vel%y%old(i-2,j,k)-4.0d0*p%of(id)%loc%vel%y%old(i-1,j,k)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k))/p%glb%dx
            
        yp = 0.5d0*(-p%of(id)%loc%vel%y%old(i,j+2,k)+4.0d0*p%of(id)%loc%vel%y%old(i,j+1,k)-3.0d0*p%of(id)%loc%vel%y%old(i,j,k))/p%glb%dy
        ym = 0.5d0*( p%of(id)%loc%vel%y%old(i,j-2,k)-4.0d0*p%of(id)%loc%vel%y%old(i,j-1,k)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k))/p%glb%dy
            
        zp = 0.5d0*(-p%of(id)%loc%vel%y%old(i,j,k+2)+4.0d0*p%of(id)%loc%vel%y%old(i,j,k+1)-3.0d0*p%of(id)%loc%vel%y%old(i,j,k))/p%glb%dz
        zm = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k-2)-4.0d0*p%of(id)%loc%vel%y%old(i,j,k-1)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k))/p%glb%dz

        p%of(id)%loc%velsrc%y%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                                        &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                                        &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0
            
        !-----------------------------------------------------------
            
        u = p%of(id)%loc%tdata%x%s2(i,j,k)
        v = p%of(id)%loc%tdata%y%s2(i,j,k)
        w = p%of(id)%loc%vel%z%tmp(i,j,k)

        xp = 0.5d0*(-p%of(id)%loc%vel%z%old(i+2,j,k)+4.0d0*p%of(id)%loc%vel%z%old(i+1,j,k)-3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dx 
        xm = 0.5d0*( p%of(id)%loc%vel%z%old(i-2,j,k)-4.0d0*p%of(id)%loc%vel%z%old(i-1,j,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dx
            
        yp = 0.5d0*(-p%of(id)%loc%vel%z%old(i,j+2,k)+4.0d0*p%of(id)%loc%vel%z%old(i,j+1,k)-3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dy
        ym = 0.5d0*( p%of(id)%loc%vel%z%old(i,j-2,k)-4.0d0*p%of(id)%loc%vel%z%old(i,j-1,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dy
            
        zp = 0.5d0*(-p%of(id)%loc%vel%z%old(i,j,k+2)+4.0d0*p%of(id)%loc%vel%z%old(i,j,k+1)-3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dz
        zm = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k-2)-4.0d0*p%of(id)%loc%vel%z%old(i,j,k-1)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dz
            
        p%of(id)%loc%velsrc%z%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                                        &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                                        &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0
                                          
    end do
    end do 
    end do
    !$omp end parallel do

enddo   
!$omp end parallel do
    
end subroutine

subroutine ns_ab_adv_source_quick
!--------------------------------------
! u*Px = [ u_{i}+u_{i+1} ] * [ P_{i+1/2}-P_{i-1/2} ] / dx
!--------------------------------------
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: uh, vh, wh

    !$omp parallel do private(i,j,k,uh,vh,wh)
    do id = 0, p%glb%threads-1
        
        do k = p%of(id)%loc%ks-1, p%of(id)%loc%ke
        do j = p%of(id)%loc%js-1, p%of(id)%loc%je
        do i = p%of(id)%loc%is-1, p%of(id)%loc%ie
        
            uh = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i+1,j,k) )
            vh = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i+1,j,k) )
            wh = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i+1,j,k) )
            
            if( uh>=0.0d0 )then
                p%of(id)%loc%tdata%x%s1(i,j,k) = ( -p%of(id)%loc%vel%x%old(i-1,j,k)+6.0d0*p%of(id)%loc%vel%x%old(i,j,k)+3.0d0*p%of(id)%loc%vel%x%old(i+1,j,k) )/8.0d0
            else
                p%of(id)%loc%tdata%x%s1(i,j,k) = ( -p%of(id)%loc%vel%x%old(i+2,j,k)+6.0d0*p%of(id)%loc%vel%x%old(i+1,j,k)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k) )/8.0d0
            endif
            
            if( vh>=0.0d0 )then
                p%of(id)%loc%tdata%x%s2(i,j,k) = ( -p%of(id)%loc%vel%x%old(i,j-1,k)+6.0d0*p%of(id)%loc%vel%x%old(i,j,k)+3.0d0*p%of(id)%loc%vel%x%old(i,j+1,k) )/8.0d0
            else
                p%of(id)%loc%tdata%x%s2(i,j,k) = ( -p%of(id)%loc%vel%x%old(i,j+2,k)+6.0d0*p%of(id)%loc%vel%x%old(i,j+1,k)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k) )/8.0d0
            endif
            
            if( wh>=0.0d0 )then
                p%of(id)%loc%tdata%x%s3(i,j,k) = ( -p%of(id)%loc%vel%x%old(i,j,k-1)+6.0d0*p%of(id)%loc%vel%x%old(i,j,k)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k+1) )/8.0d0
            else
                p%of(id)%loc%tdata%x%s3(i,j,k) = ( -p%of(id)%loc%vel%x%old(i,j,k+2)+6.0d0*p%of(id)%loc%vel%x%old(i,j,k+1)+3.0d0*p%of(id)%loc%vel%x%old(i,j,k) )/8.0d0
            endif
            
            !-----------------------------------------------------------
            
            uh = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i,j+1,k) )
            vh = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i,j+1,k) )
            wh = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i,j+1,k) )
            
            if( uh>=0.0d0 )then
                p%of(id)%loc%tdata%y%s1(i,j,k) = ( -p%of(id)%loc%vel%y%old(i-1,j,k)+6.0d0*p%of(id)%loc%vel%y%old(i,j,k)+3.0d0*p%of(id)%loc%vel%y%old(i+1,j,k) )/8.0d0
            else
                p%of(id)%loc%tdata%y%s1(i,j,k) = ( -p%of(id)%loc%vel%y%old(i+2,j,k)+6.0d0*p%of(id)%loc%vel%y%old(i+1,j,k)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k) )/8.0d0
            endif
            
            if( vh>=0.0d0 )then
                p%of(id)%loc%tdata%y%s2(i,j,k) = ( -p%of(id)%loc%vel%y%old(i,j-1,k)+6.0d0*p%of(id)%loc%vel%y%old(i,j,k)+3.0d0*p%of(id)%loc%vel%y%old(i,j+1,k) )/8.0d0
            else
                p%of(id)%loc%tdata%y%s2(i,j,k) = ( -p%of(id)%loc%vel%y%old(i,j+2,k)+6.0d0*p%of(id)%loc%vel%y%old(i,j+1,k)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k) )/8.0d0
            endif
            
            if( wh>=0.0d0 )then
                p%of(id)%loc%tdata%y%s3(i,j,k) = ( -p%of(id)%loc%vel%y%old(i,j,k-1)+6.0d0*p%of(id)%loc%vel%y%old(i,j,k)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k+1) )/8.0d0
            else
                p%of(id)%loc%tdata%y%s3(i,j,k) = ( -p%of(id)%loc%vel%y%old(i,j,k+2)+6.0d0*p%of(id)%loc%vel%y%old(i,j,k+1)+3.0d0*p%of(id)%loc%vel%y%old(i,j,k) )/8.0d0
            endif
            
            !-----------------------------------------------------------

            uh = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i,j,k+1) )
            vh = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i,j,k+1) )
            wh = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i,j,k+1) )
            
            if( uh>=0.0d0 )then
                p%of(id)%loc%tdata%z%s1(i,j,k) = ( -p%of(id)%loc%vel%z%old(i-1,j,k)+6.0d0*p%of(id)%loc%vel%z%old(i,j,k)+3.0d0*p%of(id)%loc%vel%z%old(i+1,j,k) )/8.0d0
            else
                p%of(id)%loc%tdata%z%s1(i,j,k) = ( -p%of(id)%loc%vel%z%old(i+2,j,k)+6.0d0*p%of(id)%loc%vel%z%old(i+1,j,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k) )/8.0d0
            endif
            
            if( vh>=0.0d0 )then
                p%of(id)%loc%tdata%z%s2(i,j,k) = ( -p%of(id)%loc%vel%z%old(i,j-1,k)+6.0d0*p%of(id)%loc%vel%z%old(i,j,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j+1,k) )/8.0d0
            else
                p%of(id)%loc%tdata%z%s2(i,j,k) = ( -p%of(id)%loc%vel%z%old(i,j+2,k)+6.0d0*p%of(id)%loc%vel%z%old(i,j+1,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k) )/8.0d0
            endif
            
            if( wh>=0.0d0 )then
                p%of(id)%loc%tdata%z%s3(i,j,k) = ( -p%of(id)%loc%vel%z%old(i,j,k-1)+6.0d0*p%of(id)%loc%vel%z%old(i,j,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k+1) )/8.0d0
            else
                p%of(id)%loc%tdata%z%s3(i,j,k) = ( -p%of(id)%loc%vel%z%old(i,j,k+2)+6.0d0*p%of(id)%loc%vel%z%old(i,j,k+1)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k) )/8.0d0
            endif
            
        end do
        end do
        end do

    enddo
    !$omp end parallel do 

    call pt%tdatax%sync
    call pt%tdatay%sync
    call pt%tdataz%sync

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
            uh = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i+1,j,k) )
            vh = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i+1,j,k) )
            wh = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i+1,j,k) )
        
            p%of(id)%loc%velsrc%x%now(i,j,k) = - uh*( p%of(id)%loc%tdata%x%s1(i,j,k)-p%of(id)%loc%tdata%x%s1(i-1,j,k) )/p%glb%dx &
                                              &- vh*( p%of(id)%loc%tdata%x%s2(i,j,k)-p%of(id)%loc%tdata%x%s2(i,j-1,k) )/p%glb%dy &
                                              &- wh*( p%of(id)%loc%tdata%x%s3(i,j,k)-p%of(id)%loc%tdata%x%s3(i,j,k-1) )/p%glb%dz
            
            uh = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i,j+1,k) )
            vh = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i,j+1,k) )
            wh = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i,j+1,k) )
            
            p%of(id)%loc%velsrc%y%now(i,j,k) = - uh*( p%of(id)%loc%tdata%y%s1(i,j,k)-p%of(id)%loc%tdata%y%s1(i-1,j,k) )/p%glb%dx &
                                              &- vh*( p%of(id)%loc%tdata%y%s2(i,j,k)-p%of(id)%loc%tdata%y%s2(i,j-1,k) )/p%glb%dy &
                                              &- wh*( p%of(id)%loc%tdata%y%s3(i,j,k)-p%of(id)%loc%tdata%y%s3(i,j,k-1) )/p%glb%dz
            
            uh = 0.5d0*( p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i,j,k+1) )
            vh = 0.5d0*( p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i,j,k+1) )
            wh = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i,j,k+1) )
            
            p%of(id)%loc%velsrc%z%now(i,j,k) = - uh*( p%of(id)%loc%tdata%z%s1(i,j,k)-p%of(id)%loc%tdata%z%s1(i-1,j,k) )/p%glb%dx &
                                              &- vh*( p%of(id)%loc%tdata%z%s2(i,j,k)-p%of(id)%loc%tdata%z%s2(i,j-1,k) )/p%glb%dy &
                                              &- wh*( p%of(id)%loc%tdata%z%s3(i,j,k)-p%of(id)%loc%tdata%z%s3(i,j,k-1) )/p%glb%dz
                                              
        end do
        end do
        end do
    
    enddo
    !$omp end parallel do
    
end subroutine

subroutine ns_ab_adv_source_uccd()
use all
implicit none
integer :: id,i,j,k
real(8) :: u,v,w

!$omp parallel do 
do id = 0, p%glb%threads-1
        
    call p%of(id)%find_stag_vel( p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                                &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                                &p%of(id)%loc%vel%x%old, p%of(id)%loc%vel%y%old, p%of(id)%loc%vel%z%old )
enddo       
!$omp end parallel do

call pt%tdatax%sync
call pt%tdatay%sync
call pt%tdataz%sync


!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1

    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je

        call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%vel%x%old(:,j,k),&
                                            &p%of(id)%loc%vel_ten%xx(:,j,k),&
                                            &p%of(id)%loc%vel_ten%xxx(:,j,k),&
                                            &p%of(id)%loc%vel%x%old(:,j,k)) 

        call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%vel%y%old(:,j,k),&
                                            &p%of(id)%loc%vel_ten%yx(:,j,k),&
                                            &p%of(id)%loc%vel_ten%yxx(:,j,k),&
                                            &p%of(id)%loc%tdata%x%s1(:,j,k)) 

        call p%of(id)%loc%ccdsolvers%x%solve("uccd",p%of(id)%loc%vel%z%old(:,j,k),&
                                            &p%of(id)%loc%vel_ten%zx(:,j,k),&
                                            &p%of(id)%loc%vel_ten%zxx(:,j,k),&
                                            &p%of(id)%loc%tdata%x%s2(:,j,k)) 

    enddo
    enddo

enddo
!$omp end parallel do

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1

    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je

        call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%vel%x%old(i,:,k),&
                                            &p%of(id)%loc%vel_ten%xy(i,:,k),&
                                            &p%of(id)%loc%vel_ten%xyy(i,:,k),&
                                            &p%of(id)%loc%tdata%y%s1(i,:,k))

        call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%vel%y%old(i,:,k),&
                                            &p%of(id)%loc%vel_ten%yy(i,:,k),&
                                            &p%of(id)%loc%vel_ten%yyy(i,:,k),&
                                            &p%of(id)%loc%vel%y%old(i,:,k))

        call p%of(id)%loc%ccdsolvers%y%solve("uccd",p%of(id)%loc%vel%z%old(i,:,k),&
                                            &p%of(id)%loc%vel_ten%zy(i,:,k),&
                                            &p%of(id)%loc%vel_ten%zyy(i,:,k),&
                                            &p%of(id)%loc%tdata%y%s2(i,:,k))

    enddo
    enddo

enddo
!$omp end parallel do


!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1

    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie

        call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%vel%x%old(i,j,:),&
                                            &p%of(id)%loc%vel_ten%xz(i,j,:),&
                                            &p%of(id)%loc%vel_ten%xzz(i,j,:),&
                                            &p%of(id)%loc%tdata%z%s1(i,j,:))

        call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%vel%y%old(i,j,:),&
                                            &p%of(id)%loc%vel_ten%yz(i,j,:),&
                                            &p%of(id)%loc%vel_ten%yzz(i,j,:),&
                                            &p%of(id)%loc%tdata%z%s2(i,j,:))

        call p%of(id)%loc%ccdsolvers%z%solve("uccd",p%of(id)%loc%vel%z%old(i,j,:),&
                                            &p%of(id)%loc%vel_ten%zz(i,j,:),&
                                            &p%of(id)%loc%vel_ten%zzz(i,j,:),&
                                            &p%of(id)%loc%vel%z%old(i,j,:))
    enddo
    enddo

enddo
!$omp end parallel do


!$omp parallel do private(u,v,w,i,j,k)
do id = 0, p%glb%threads-1

    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie

        u = p%of(id)%loc%vel%x%old(i,j,k)
        v = p%of(id)%loc%tdata%y%s1(i,j,k)
        w = p%of(id)%loc%tdata%z%s1(i,j,k)

        p%of(id)%loc%velsrc%x%now(i,j,k) = - u * p%of(id)%loc%vel_ten%xx(i,j,k) &
                                        &  - v * p%of(id)%loc%vel_ten%xy(i,j,k) &
                                        &  - w * p%of(id)%loc%vel_ten%xz(i,j,k) 

        u = p%of(id)%loc%tdata%x%s1(i,j,k)
        v = p%of(id)%loc%vel%y%old(i,j,k)
        w = p%of(id)%loc%tdata%z%s2(i,j,k)

        p%of(id)%loc%velsrc%y%now(i,j,k) = - u * p%of(id)%loc%vel_ten%yx(i,j,k) &
                                        &  - v * p%of(id)%loc%vel_ten%yy(i,j,k) &
                                        &  - w * p%of(id)%loc%vel_ten%yz(i,j,k)

        u = p%of(id)%loc%tdata%x%s2(i,j,k)
        v = p%of(id)%loc%tdata%y%s2(i,j,k)
        w = p%of(id)%loc%vel%z%old(i,j,k)

        p%of(id)%loc%velsrc%z%now(i,j,k) = - u * p%of(id)%loc%vel_ten%zx(i,j,k) &
                                        &  - v * p%of(id)%loc%vel_ten%zy(i,j,k) &
                                        &  - w * p%of(id)%loc%vel_ten%zz(i,j,k)

    enddo
    enddo
    enddo

enddo
!$omp end parallel do

end subroutine
