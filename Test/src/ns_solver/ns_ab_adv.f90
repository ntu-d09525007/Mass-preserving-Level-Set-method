subroutine ns_ab_adv_source_sec
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: u,v,w,xp,xm,yp,ym,zp,zm
    
!$omp parallel do collapse(3), private(u,v,w,xp,xm,yp,ym,zp,zm)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    u = p%loc%vel%x%tmp(i,j,k)
    v = p%loc%tdata%y%s1(i,j,k)
    w = p%loc%tdata%z%s1(i,j,k)
        
    xp = 0.5d0*(-p%loc%vel%x%old(i+2,j,k)+4.0d0*p%loc%vel%x%old(i+1,j,k)-3.0d0*p%loc%vel%x%old(i,j,k))/p%glb%dx 
    xm = 0.5d0*( p%loc%vel%x%old(i-2,j,k)-4.0d0*p%loc%vel%x%old(i-1,j,k)+3.0d0*p%loc%vel%x%old(i,j,k))/p%glb%dx
    
    yp = 0.5d0*(-p%loc%vel%x%old(i,j+2,k)+4.0d0*p%loc%vel%x%old(i,j+1,k)-3.0d0*p%loc%vel%x%old(i,j,k))/p%glb%dy
    ym = 0.5d0*( p%loc%vel%x%old(i,j-2,k)-4.0d0*p%loc%vel%x%old(i,j-1,k)+3.0d0*p%loc%vel%x%old(i,j,k))/p%glb%dy
        
    zp = 0.5d0*(-p%loc%vel%x%old(i,j,k+2)+4.0d0*p%loc%vel%x%old(i,j,k+1)-3.0d0*p%loc%vel%x%old(i,j,k))/p%glb%dz
    zm = 0.5d0*( p%loc%vel%x%old(i,j,k-2)-4.0d0*p%loc%vel%x%old(i,j,k-1)+3.0d0*p%loc%vel%x%old(i,j,k))/p%glb%dz
    
    p%loc%velsrc%x%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                             &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                             &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0  
            
    !-----------------------------------------------------------
        
    u = p%loc%tdata%x%s1(i,j,k)
    v = p%loc%vel%y%tmp(i,j,k)
    w = p%loc%tdata%z%s2(i,j,k)
        
    xp = 0.5d0*(-p%loc%vel%y%old(i+2,j,k)+4.0d0*p%loc%vel%y%old(i+1,j,k)-3.0d0*p%loc%vel%y%old(i,j,k))/p%glb%dx 
    xm = 0.5d0*( p%loc%vel%y%old(i-2,j,k)-4.0d0*p%loc%vel%y%old(i-1,j,k)+3.0d0*p%loc%vel%y%old(i,j,k))/p%glb%dx
        
    yp = 0.5d0*(-p%loc%vel%y%old(i,j+2,k)+4.0d0*p%loc%vel%y%old(i,j+1,k)-3.0d0*p%loc%vel%y%old(i,j,k))/p%glb%dy
    ym = 0.5d0*( p%loc%vel%y%old(i,j-2,k)-4.0d0*p%loc%vel%y%old(i,j-1,k)+3.0d0*p%loc%vel%y%old(i,j,k))/p%glb%dy
        
    zp = 0.5d0*(-p%loc%vel%y%old(i,j,k+2)+4.0d0*p%loc%vel%y%old(i,j,k+1)-3.0d0*p%loc%vel%y%old(i,j,k))/p%glb%dz
    zm = 0.5d0*( p%loc%vel%y%old(i,j,k-2)-4.0d0*p%loc%vel%y%old(i,j,k-1)+3.0d0*p%loc%vel%y%old(i,j,k))/p%glb%dz
        
    p%loc%velsrc%y%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                             &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                             &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0
        
    !-----------------------------------------------------------
        
    u = p%loc%tdata%x%s2(i,j,k)
    v = p%loc%tdata%y%s2(i,j,k)
    w = p%loc%vel%z%tmp(i,j,k)
        
    xp = 0.5d0*(-p%loc%vel%z%old(i+2,j,k)+4.0d0*p%loc%vel%z%old(i+1,j,k)-3.0d0*p%loc%vel%z%old(i,j,k))/p%glb%dx 
    xm = 0.5d0*( p%loc%vel%z%old(i-2,j,k)-4.0d0*p%loc%vel%z%old(i-1,j,k)+3.0d0*p%loc%vel%z%old(i,j,k))/p%glb%dx
        
    yp = 0.5d0*(-p%loc%vel%z%old(i,j+2,k)+4.0d0*p%loc%vel%z%old(i,j+1,k)-3.0d0*p%loc%vel%z%old(i,j,k))/p%glb%dy
    ym = 0.5d0*( p%loc%vel%z%old(i,j-2,k)-4.0d0*p%loc%vel%z%old(i,j-1,k)+3.0d0*p%loc%vel%z%old(i,j,k))/p%glb%dy
        
    zp = 0.5d0*(-p%loc%vel%z%old(i,j,k+2)+4.0d0*p%loc%vel%z%old(i,j,k+1)-3.0d0*p%loc%vel%z%old(i,j,k))/p%glb%dz
    zm = 0.5d0*( p%loc%vel%z%old(i,j,k-2)-4.0d0*p%loc%vel%z%old(i,j,k-1)+3.0d0*p%loc%vel%z%old(i,j,k))/p%glb%dz
        
    p%loc%velsrc%z%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                             &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                             &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0
                                      
end do
end do 
end do
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

!$omp parallel do collapse(3), private(uh,vh,wh)        
do k = p%loc%ks-1, p%loc%ke
do j = p%loc%js-1, p%loc%je
do i = p%loc%is-1, p%loc%ie

    uh = 0.5d0*( p%loc%vel%x%tmp(i,j,k) + p%loc%vel%x%tmp(i+1,j,k) )
    vh = 0.5d0*( p%loc%vel%y%tmp(i,j,k) + p%loc%vel%y%tmp(i+1,j,k) )
    wh = 0.5d0*( p%loc%vel%z%tmp(i,j,k) + p%loc%vel%z%tmp(i+1,j,k) )
    
    if( uh>=0.0d0 )then
        p%loc%tdata%x%s1(i,j,k) = ( -p%loc%vel%x%old(i-1,j,k)+6.0d0*p%loc%vel%x%old(i,j,k)+3.0d0*p%loc%vel%x%old(i+1,j,k) )/8.0d0
    else
        p%loc%tdata%x%s1(i,j,k) = ( -p%loc%vel%x%old(i+2,j,k)+6.0d0*p%loc%vel%x%old(i+1,j,k)+3.0d0*p%loc%vel%x%old(i,j,k) )/8.0d0
    endif
    
    if( vh>=0.0d0 )then
        p%loc%tdata%x%s2(i,j,k) = ( -p%loc%vel%x%old(i,j-1,k)+6.0d0*p%loc%vel%x%old(i,j,k)+3.0d0*p%loc%vel%x%old(i,j+1,k) )/8.0d0
    else
        p%loc%tdata%x%s2(i,j,k) = ( -p%loc%vel%x%old(i,j+2,k)+6.0d0*p%loc%vel%x%old(i,j+1,k)+3.0d0*p%loc%vel%x%old(i,j,k) )/8.0d0
    endif
    
    if( wh>=0.0d0 )then
        p%loc%tdata%x%s3(i,j,k) = ( -p%loc%vel%x%old(i,j,k-1)+6.0d0*p%loc%vel%x%old(i,j,k)+3.0d0*p%loc%vel%x%old(i,j,k+1) )/8.0d0
    else
        p%loc%tdata%x%s3(i,j,k) = ( -p%loc%vel%x%old(i,j,k+2)+6.0d0*p%loc%vel%x%old(i,j,k+1)+3.0d0*p%loc%vel%x%old(i,j,k) )/8.0d0
    endif
    
    !-----------------------------------------------------------
    
    uh = 0.5d0*( p%loc%vel%x%tmp(i,j,k) + p%loc%vel%x%tmp(i,j+1,k) )
    vh = 0.5d0*( p%loc%vel%y%tmp(i,j,k) + p%loc%vel%y%tmp(i,j+1,k) )
    wh = 0.5d0*( p%loc%vel%z%tmp(i,j,k) + p%loc%vel%z%tmp(i,j+1,k) )
    
    if( uh>=0.0d0 )then
        p%loc%tdata%y%s1(i,j,k) = ( -p%loc%vel%y%old(i-1,j,k)+6.0d0*p%loc%vel%y%old(i,j,k)+3.0d0*p%loc%vel%y%old(i+1,j,k) )/8.0d0
    else
        p%loc%tdata%y%s1(i,j,k) = ( -p%loc%vel%y%old(i+2,j,k)+6.0d0*p%loc%vel%y%old(i+1,j,k)+3.0d0*p%loc%vel%y%old(i,j,k) )/8.0d0
    endif
    
    if( vh>=0.0d0 )then
        p%loc%tdata%y%s2(i,j,k) = ( -p%loc%vel%y%old(i,j-1,k)+6.0d0*p%loc%vel%y%old(i,j,k)+3.0d0*p%loc%vel%y%old(i,j+1,k) )/8.0d0
    else
        p%loc%tdata%y%s2(i,j,k) = ( -p%loc%vel%y%old(i,j+2,k)+6.0d0*p%loc%vel%y%old(i,j+1,k)+3.0d0*p%loc%vel%y%old(i,j,k) )/8.0d0
    endif
    
    if( wh>=0.0d0 )then
        p%loc%tdata%y%s3(i,j,k) = ( -p%loc%vel%y%old(i,j,k-1)+6.0d0*p%loc%vel%y%old(i,j,k)+3.0d0*p%loc%vel%y%old(i,j,k+1) )/8.0d0
    else
        p%loc%tdata%y%s3(i,j,k) = ( -p%loc%vel%y%old(i,j,k+2)+6.0d0*p%loc%vel%y%old(i,j,k+1)+3.0d0*p%loc%vel%y%old(i,j,k) )/8.0d0
    endif
    
    !-----------------------------------------------------------

    uh = 0.5d0*( p%loc%vel%x%tmp(i,j,k) + p%loc%vel%x%tmp(i,j,k+1) )
    vh = 0.5d0*( p%loc%vel%y%tmp(i,j,k) + p%loc%vel%y%tmp(i,j,k+1) )
    wh = 0.5d0*( p%loc%vel%z%tmp(i,j,k) + p%loc%vel%z%tmp(i,j,k+1) )
    
    if( uh>=0.0d0 )then
        p%loc%tdata%z%s1(i,j,k) = ( -p%loc%vel%z%old(i-1,j,k)+6.0d0*p%loc%vel%z%old(i,j,k)+3.0d0*p%loc%vel%z%old(i+1,j,k) )/8.0d0
    else
        p%loc%tdata%z%s1(i,j,k) = ( -p%loc%vel%z%old(i+2,j,k)+6.0d0*p%loc%vel%z%old(i+1,j,k)+3.0d0*p%loc%vel%z%old(i,j,k) )/8.0d0
    endif
    
    if( vh>=0.0d0 )then
        p%loc%tdata%z%s2(i,j,k) = ( -p%loc%vel%z%old(i,j-1,k)+6.0d0*p%loc%vel%z%old(i,j,k)+3.0d0*p%loc%vel%z%old(i,j+1,k) )/8.0d0
    else
        p%loc%tdata%z%s2(i,j,k) = ( -p%loc%vel%z%old(i,j+2,k)+6.0d0*p%loc%vel%z%old(i,j+1,k)+3.0d0*p%loc%vel%z%old(i,j,k) )/8.0d0
    endif
    
    if( wh>=0.0d0 )then
        p%loc%tdata%z%s3(i,j,k) = ( -p%loc%vel%z%old(i,j,k-1)+6.0d0*p%loc%vel%z%old(i,j,k)+3.0d0*p%loc%vel%z%old(i,j,k+1) )/8.0d0
    else
        p%loc%tdata%z%s3(i,j,k) = ( -p%loc%vel%z%old(i,j,k+2)+6.0d0*p%loc%vel%z%old(i,j,k+1)+3.0d0*p%loc%vel%z%old(i,j,k) )/8.0d0
    endif
    
end do
end do
end do
!$omp end parallel do

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    uh = 0.5d0*( p%loc%vel%x%tmp(i,j,k) + p%loc%vel%x%tmp(i+1,j,k) )
    vh = 0.5d0*( p%loc%vel%y%tmp(i,j,k) + p%loc%vel%y%tmp(i+1,j,k) )
    wh = 0.5d0*( p%loc%vel%z%tmp(i,j,k) + p%loc%vel%z%tmp(i+1,j,k) )

    p%loc%velsrc%x%now(i,j,k) = - uh*( p%loc%tdata%x%s1(i,j,k)-p%loc%tdata%x%s1(i-1,j,k) )/p%glb%dx &
                               &- vh*( p%loc%tdata%x%s2(i,j,k)-p%loc%tdata%x%s2(i,j-1,k) )/p%glb%dy &
                               &- wh*( p%loc%tdata%x%s3(i,j,k)-p%loc%tdata%x%s3(i,j,k-1) )/p%glb%dz
    
    uh = 0.5d0*( p%loc%vel%x%tmp(i,j,k) + p%loc%vel%x%tmp(i,j+1,k) )
    vh = 0.5d0*( p%loc%vel%y%tmp(i,j,k) + p%loc%vel%y%tmp(i,j+1,k) )
    wh = 0.5d0*( p%loc%vel%z%tmp(i,j,k) + p%loc%vel%z%tmp(i,j+1,k) )
    
    p%loc%velsrc%y%now(i,j,k) = - uh*( p%loc%tdata%y%s1(i,j,k)-p%loc%tdata%y%s1(i-1,j,k) )/p%glb%dx &
                               &- vh*( p%loc%tdata%y%s2(i,j,k)-p%loc%tdata%y%s2(i,j-1,k) )/p%glb%dy &
                               &- wh*( p%loc%tdata%y%s3(i,j,k)-p%loc%tdata%y%s3(i,j,k-1) )/p%glb%dz
    
    uh = 0.5d0*( p%loc%vel%x%tmp(i,j,k) + p%loc%vel%x%tmp(i,j,k+1) )
    vh = 0.5d0*( p%loc%vel%y%tmp(i,j,k) + p%loc%vel%y%tmp(i,j,k+1) )
    wh = 0.5d0*( p%loc%vel%z%tmp(i,j,k) + p%loc%vel%z%tmp(i,j,k+1) )
    
    p%loc%velsrc%z%now(i,j,k) = - uh*( p%loc%tdata%z%s1(i,j,k)-p%loc%tdata%z%s1(i-1,j,k) )/p%glb%dx &
                               &- vh*( p%loc%tdata%z%s2(i,j,k)-p%loc%tdata%z%s2(i,j-1,k) )/p%glb%dy &
                               &- wh*( p%loc%tdata%z%s3(i,j,k)-p%loc%tdata%z%s3(i,j,k-1) )/p%glb%dz
                                      
end do
end do
end do
!$omp end parallel do
    
end subroutine

subroutine ns_ab_adv_source_uccd
use all
implicit none
integer :: i,j,k
real(8) :: u,v,w

!$omp parallel do collapse(3), private(u,v,w)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    u = p%loc%vel%x%tmp(i,j,k)
    v = p%loc%tdata%y%s1(i,j,k)
    w = p%loc%tdata%z%s1(i,j,k)

    p%loc%velsrc%x%now(i,j,k) = -u*p%loc%vel_ten%xx(i,j,k)-v*p%loc%vel_ten%xy(i,j,k)-w*p%loc%vel_ten%xz(i,j,k)

    u = p%loc%tdata%x%s1(i,j,k)
    v = p%loc%vel%y%tmp(i,j,k)
    w = p%loc%tdata%z%s2(i,j,k)

    p%loc%velsrc%y%now(i,j,k) = -u*p%loc%vel_ten%yx(i,j,k)-v*p%loc%vel_ten%yy(i,j,k)-w*p%loc%vel_ten%yz(i,j,k)

    u = p%loc%tdata%x%s2(i,j,k)
    v = p%loc%tdata%y%s2(i,j,k)
    w = p%loc%vel%z%tmp(i,j,k)

    p%loc%velsrc%z%now(i,j,k) = -u*p%loc%vel_ten%zx(i,j,k)-v*p%loc%vel_ten%zy(i,j,k)-w*p%loc%vel_ten%zz(i,j,k)

enddo
enddo
enddo
!$omp end parallel do
    
end subroutine