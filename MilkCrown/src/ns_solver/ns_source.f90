subroutine second_order
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: u,v,w,xp,xm,yp,ym,zp,zm
real(8) :: ux,uy,uz,vx,vy,vz,wx,wy,wz
real(8) :: rho, mu, delta, curv
real(8) :: difx, dify, difz
real(8) :: xx, yy, zz, nx, ny, nz

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
        
    call p%of(id)%find_stag_vel( p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                                &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                                &p%of(id)%loc%vel%x%old, p%of(id)%loc%vel%y%old, p%of(id)%loc%vel%z%old )
enddo       
!$omp end parallel do
    
call pt%tdatax%sync
call pt%tdatay%sync
call pt%tdataz%sync
    
!$omp parallel do private(i,j,k,u,v,w,xp,xm,yp,ym,zp,zm,rho,mu,delta,curv,xx,yy,zz,nx,ny,nz,ux,uy,uz,vx,vy,vz,wx,wy,wz,difx,dify,difz)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,u,v,w,xp,xm,yp,ym,zp,zm,rho,mu,delta,curv,xx,yy,zz)&
    !$omp& private(ux,uy,uz,vx,vy,vz,wx,wy,wz,difx,dify,difz,nx,ny,nz)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        u = p%of(id)%loc%vel%x%old(i,j,k)
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

        rho = 0.5d0 * ( p%of(id)%loc%rho%old(i,j,k) + p%of(id)%loc%rho%old(i+1,j,k) )
        mu  = 0.5d0 * ( p%of(id)%loc%mu%old(i,j,k)  + p%of(id)%loc%mu%old(i+1,j,k) )
        curv = 0.5d0 * ( p%of(id)%loc%normals%curv%now(i,j,k) + p%of(id)%loc%normals%curv%now(i+1,j,k) )
        delta = 0.5d0 * ( p%of(id)%loc%delta%now(i,j,k) + p%of(id)%loc%delta%now(i+1,j,k) )

        nx = 0.5d0 * ( p%of(id)%loc%normals%x%now(i,j,k) + p%of(id)%loc%normals%x%now(i+1,j,k) )
        ny = 0.5d0 * ( p%of(id)%loc%normals%y%now(i,j,k) + p%of(id)%loc%normals%y%now(i+1,j,k) )
        nz = 0.5d0 * ( p%of(id)%loc%normals%z%now(i,j,k) + p%of(id)%loc%normals%z%now(i+1,j,k) )

        xx = (p%of(id)%loc%vel%x%old(i+1,j,k) - 2.0d0 * p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i-1,j,k))/p%glb%dx**2.0
        yy = (p%of(id)%loc%vel%x%old(i,j+1,k) - 2.0d0 * p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i,j-1,k))/p%glb%dy**2.0
        zz = (p%of(id)%loc%vel%x%old(i,j,k+1) - 2.0d0 * p%of(id)%loc%vel%x%old(i,j,k) + p%of(id)%loc%vel%x%old(i,j,k-1))/p%glb%dz**2.0

        ux = 0.5d0* ( p%of(id)%loc%vel%x%old(i+1,j,k) + p%of(id)%loc%vel%x%old(i-1,j,k)) / p%glb%dx
        uy = 0.5d0* ( p%of(id)%loc%vel%x%old(i,j+1,k) + p%of(id)%loc%vel%x%old(i,j-1,k)) / p%glb%dy
        uz = 0.5d0* ( p%of(id)%loc%vel%x%old(i,j,k+1) + p%of(id)%loc%vel%x%old(i,j,k-1)) / p%glb%dz

        vx = 0.25d0*( p%of(id)%loc%vel%y%old(i+1,j  ,k)-p%of(id)%loc%vel%y%old(i,j  ,k) &
                    &+p%of(id)%loc%vel%y%old(i+1,j-1,k)-p%of(id)%loc%vel%y%old(i,j-1,k) ) / p%glb%dx

        wx = 0.25d0*( p%of(id)%loc%vel%z%old(i+1,j,k  )-p%of(id)%loc%vel%z%old(i,j,k  ) &
                    &+p%of(id)%loc%vel%z%old(i+1,j,k-1)-p%of(id)%loc%vel%z%old(i,j,k-1) ) / p%glb%dx

        difx = mu * xx / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * nx * (ux+ux) / ( rho * p%glb%re )
        dify = mu * yy / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * ny * (vx+uy) / ( rho * p%glb%re )
        difz = mu * zz / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * nz * (wx+uz) / ( rho * p%glb%re )

        p%of(id)%loc%velsrc%x%now(i,j,k) = p%of(id)%loc%velsrc%x%now(i,j,k) &
                                       & + (difx + dify + difz) &
                                       & + p%glb%gx / p%glb%fr * p%glb%btn_g &
                                       & - delta * curv * nx / rho / p%glb%we * p%glb%btn_sf
        !-----------------------------------------------------------
            
        u = p%of(id)%loc%tdata%x%s1(i,j,k)
        v = p%of(id)%loc%vel%y%old(i,j,k)
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

        rho = 0.5d0 * ( p%of(id)%loc%rho%old(i,j,k) + p%of(id)%loc%rho%old(i,j+1,k) )
        mu  = 0.5d0 * ( p%of(id)%loc%mu%old(i,j,k)  + p%of(id)%loc%mu%old(i,j+1,k) )
        curv = 0.5d0 * ( p%of(id)%loc%normals%curv%now(i,j,k) + p%of(id)%loc%normals%curv%now(i,j+1,k) )
        delta = 0.5d0 * ( p%of(id)%loc%delta%now(i,j,k) + p%of(id)%loc%delta%now(i,j+1,k) )
            
        nx = 0.5d0 * ( p%of(id)%loc%normals%x%now(i,j,k) + p%of(id)%loc%normals%x%now(i,j+1,k) )
        ny = 0.5d0 * ( p%of(id)%loc%normals%y%now(i,j,k) + p%of(id)%loc%normals%y%now(i,j+1,k) )
        nz = 0.5d0 * ( p%of(id)%loc%normals%z%now(i,j,k) + p%of(id)%loc%normals%z%now(i,j+1,k) )

        xx = (p%of(id)%loc%vel%y%old(i+1,j,k) - 2.0d0 * p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i-1,j,k))/p%glb%dx**2.0
        yy = (p%of(id)%loc%vel%y%old(i,j+1,k) - 2.0d0 * p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i,j-1,k))/p%glb%dy**2.0
        zz = (p%of(id)%loc%vel%y%old(i,j,k+1) - 2.0d0 * p%of(id)%loc%vel%y%old(i,j,k) + p%of(id)%loc%vel%y%old(i,j,k-1))/p%glb%dz**2.0

        vx = 0.5d0* ( p%of(id)%loc%vel%y%old(i+1,j,k) + p%of(id)%loc%vel%y%old(i-1,j,k)) / p%glb%dx
        vy = 0.5d0* ( p%of(id)%loc%vel%y%old(i,j+1,k) + p%of(id)%loc%vel%y%old(i,j-1,k)) / p%glb%dy
        vz = 0.5d0* ( p%of(id)%loc%vel%y%old(i,j,k+1) + p%of(id)%loc%vel%y%old(i,j,k-1)) / p%glb%dz

        uy = 0.25d0*( p%of(id)%loc%vel%x%old(i  ,j+1,k)-p%of(id)%loc%vel%x%old(i  ,j,k) &
                    &+p%of(id)%loc%vel%x%old(i-1,j+1,k)-p%of(id)%loc%vel%x%old(i-1,j,k) ) / p%glb%dy

        wy = 0.25d0*( p%of(id)%loc%vel%z%old(i,j+1,k  )-p%of(id)%loc%vel%z%old(i,j,k  ) &
                    &+p%of(id)%loc%vel%z%old(i,j+1,k-1)-p%of(id)%loc%vel%z%old(i,j,k-1) ) / p%glb%dy

        difx = mu * xx / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * nx * (uy+vx) / ( rho * p%glb%re )
        dify = mu * yy / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * ny * (vy+vy) / ( rho * p%glb%re )
        difz = mu * zz / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * nz * (wy+vz) / ( rho * p%glb%re )

        p%of(id)%loc%velsrc%y%now(i,j,k) = p%of(id)%loc%velsrc%y%now(i,j,k) &
                                       & + (difx + dify + difz) &
                                       & + p%glb%gy / p%glb%fr * p%glb%btn_g &
                                       & - delta * curv * ny / rho / p%glb%we * p%glb%btn_sf

        !-----------------------------------------------------------
            
        u = p%of(id)%loc%tdata%x%s2(i,j,k)
        v = p%of(id)%loc%tdata%y%s2(i,j,k)
        w = p%of(id)%loc%vel%z%old(i,j,k)

        xp = 0.5d0*(-p%of(id)%loc%vel%z%old(i+2,j,k)+4.0d0*p%of(id)%loc%vel%z%old(i+1,j,k)-3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dx 
        xm = 0.5d0*( p%of(id)%loc%vel%z%old(i-2,j,k)-4.0d0*p%of(id)%loc%vel%z%old(i-1,j,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dx
            
        yp = 0.5d0*(-p%of(id)%loc%vel%z%old(i,j+2,k)+4.0d0*p%of(id)%loc%vel%z%old(i,j+1,k)-3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dy
        ym = 0.5d0*( p%of(id)%loc%vel%z%old(i,j-2,k)-4.0d0*p%of(id)%loc%vel%z%old(i,j-1,k)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dy
            
        zp = 0.5d0*(-p%of(id)%loc%vel%z%old(i,j,k+2)+4.0d0*p%of(id)%loc%vel%z%old(i,j,k+1)-3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dz
        zm = 0.5d0*( p%of(id)%loc%vel%z%old(i,j,k-2)-4.0d0*p%of(id)%loc%vel%z%old(i,j,k-1)+3.0d0*p%of(id)%loc%vel%z%old(i,j,k))/p%glb%dz
            
        p%of(id)%loc%velsrc%z%now(i,j,k) = - ((u+abs(u))*xm+(u-abs(u))*xp)/2.0d0 &
                                        &  - ((v+abs(v))*ym+(v-abs(v))*yp)/2.0d0 &
                                        &  - ((w+abs(w))*zm+(w-abs(w))*zp)/2.0d0

        rho = 0.5d0 * ( p%of(id)%loc%rho%old(i,j,k) + p%of(id)%loc%rho%old(i,j,k+1) )
        mu  = 0.5d0 * ( p%of(id)%loc%mu%old(i,j,k)  + p%of(id)%loc%mu%old(i,j,k+1) )
        curv = 0.5d0 * ( p%of(id)%loc%normals%curv%now(i,j,k) + p%of(id)%loc%normals%curv%now(i,j,k+1) )
        delta = 0.5d0 * ( p%of(id)%loc%delta%now(i,j,k) + p%of(id)%loc%delta%now(i,j,k+1) )

        nx = 0.5d0 * ( p%of(id)%loc%normals%x%now(i,j,k) + p%of(id)%loc%normals%x%now(i,j,k+1) )
        ny = 0.5d0 * ( p%of(id)%loc%normals%y%now(i,j,k) + p%of(id)%loc%normals%y%now(i,j,k+1) )
        nz = 0.5d0 * ( p%of(id)%loc%normals%z%now(i,j,k) + p%of(id)%loc%normals%z%now(i,j,k+1) )

        xx = (p%of(id)%loc%vel%z%old(i+1,j,k) - 2.0d0 * p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i-1,j,k))/p%glb%dx**2.0
        yy = (p%of(id)%loc%vel%z%old(i,j+1,k) - 2.0d0 * p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i,j-1,k))/p%glb%dy**2.0
        zz = (p%of(id)%loc%vel%z%old(i,j,k+1) - 2.0d0 * p%of(id)%loc%vel%z%old(i,j,k) + p%of(id)%loc%vel%z%old(i,j,k-1))/p%glb%dz**2.0

        wx = 0.5d0* ( p%of(id)%loc%vel%z%old(i+1,j,k) + p%of(id)%loc%vel%z%old(i-1,j,k)) / p%glb%dx
        wy = 0.5d0* ( p%of(id)%loc%vel%z%old(i,j+1,k) + p%of(id)%loc%vel%z%old(i,j-1,k)) / p%glb%dy
        wz = 0.5d0* ( p%of(id)%loc%vel%z%old(i,j,k+1) + p%of(id)%loc%vel%z%old(i,j,k-1)) / p%glb%dz

        uz = 0.25d0*( p%of(id)%loc%vel%x%old(i  ,j,k+1)-p%of(id)%loc%vel%x%old(i  ,j,k) &
                    &+p%of(id)%loc%vel%x%old(i-1,j,k+1)-p%of(id)%loc%vel%x%old(i-1,j,k) ) / p%glb%dz

        vz = 0.25d0*( p%of(id)%loc%vel%z%old(i,j  ,k+1)-p%of(id)%loc%vel%z%old(i,j  ,k) &
                    &+p%of(id)%loc%vel%z%old(i,j-1,k+1)-p%of(id)%loc%vel%z%old(i,j-1,k) ) / p%glb%dz

        difx = mu * xx / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * nx * (uz+wx) / ( rho * p%glb%re )
        dify = mu * yy / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * ny * (vz+wy) / ( rho * p%glb%re )
        difz = mu * zz / ( rho * p%glb%re ) !+ (1.0d0-p%glb%mu_12) * delta * nz * (wz+wz) / ( rho * p%glb%re )

        p%of(id)%loc%velsrc%z%now(i,j,k) = p%of(id)%loc%velsrc%z%now(i,j,k) &
                                       & + (difx + dify + difz) &
                                       & + p%glb%gz / p%glb%fr * p%glb%btn_g &
                                       & - delta * curv * nz / rho / p%glb%we * p%glb%btn_sf

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