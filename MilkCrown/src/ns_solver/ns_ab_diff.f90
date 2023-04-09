subroutine ns_ab_diff_sec
implicit none

    call sec_part1
    call sec_part2
    call sec_part3

end subroutine

subroutine sec_part1
use all 
implicit none
integer :: i,j,k
real(8) :: rho,mu,xx,yy,zz,dif_x,dif_y,dif_z

    !$omp parallel do collapse(3), private(rho,mu,xx,yy,zz,dif_x,dif_y,dif_z)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i+1,j,k))
         mu = 0.5d0*(p%loc%mu%old(i,j,k)+p%loc%mu%old(i+1,j,k))

        xx = (p%loc%vel%x%old(I+1,J,K)-2.0d0*p%loc%vel%x%old(I,J,K)+p%loc%vel%x%old(I-1,J,K))/p%glb%dx**2.0d0
        yy = (p%loc%vel%x%old(I,J+1,K)-2.0d0*p%loc%vel%x%old(I,J,K)+p%loc%vel%x%old(I,J-1,K))/p%glb%dy**2.0d0
        zz = (p%loc%vel%x%old(I,J,K+1)-2.0d0*p%loc%vel%x%old(I,J,K)+p%loc%vel%x%old(I,J,K-1))/p%glb%dz**2.0d0
        
        dif_x = mu/rho*xx/p%glb%re 
        dif_y = mu/rho*yy/p%glb%re
        dif_z = mu/rho*zz/p%glb%re 

        p%loc%velsrc%x%tmp(i,j,k) = dif_x + dif_y + dif_z 

        !----------------------------------------------------

        rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i,j+1,k))
         mu = 0.5d0*(p%loc%mu%old(i,j,k)+p%loc%mu%old(i,j+1,k))

        xx = (p%loc%vel%y%old(I+1,J,K)-2.0d0*p%loc%vel%y%old(I,J,K)+p%loc%vel%y%old(I-1,J,K))/p%glb%dx**2.0d0
        yy = (p%loc%vel%y%old(I,J+1,K)-2.0d0*p%loc%vel%y%old(I,J,K)+p%loc%vel%y%old(I,J-1,K))/p%glb%dy**2.0d0
        zz = (p%loc%vel%y%old(I,J,K+1)-2.0d0*p%loc%vel%y%old(I,J,K)+p%loc%vel%y%old(I,J,K-1))/p%glb%dz**2.0d0
        
        dif_x = mu/rho*xx/p%glb%re 
        dif_y = mu/rho*yy/p%glb%re
        dif_z = mu/rho*zz/p%glb%re 

        p%loc%velsrc%y%tmp(i,j,k) = dif_x + dif_y + dif_z 

        !----------------------------------------------------

        rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i,j,k+1))
         mu = 0.5d0*(p%loc%mu%old(i,j,k)+p%loc%mu%old(i,j,k+1))

        xx = (p%loc%vel%z%old(I+1,J,K)-2.0d0*p%loc%vel%z%old(I,J,K)+p%loc%vel%z%old(I-1,J,K))/p%glb%dx**2.0d0
        yy = (p%loc%vel%z%old(I,J+1,K)-2.0d0*p%loc%vel%z%old(I,J,K)+p%loc%vel%z%old(I,J-1,K))/p%glb%dy**2.0d0
        zz = (p%loc%vel%z%old(I,J,K+1)-2.0d0*p%loc%vel%z%old(I,J,K)+p%loc%vel%z%old(I,J,K-1))/p%glb%dz**2.0d0
        
        dif_x = mu/rho*xx/p%glb%re 
        dif_y = mu/rho*yy/p%glb%re
        dif_z = mu/rho*zz/p%glb%re 

        p%loc%velsrc%z%tmp(i,j,k) = dif_x + dif_y + dif_z 
           
    end do
    end do
    end do 
    !$omp end parallel do

end subroutine

subroutine sec_part2
use all 
implicit none
integer :: i,j,k
real(8) :: rho,ux,vx,wx,uy,vy,wy,uz,vz,wz
real(8) :: dif_x,dif_y,dif_z,delta
real(8) :: phix,phiy,phiz

    !$omp parallel do collapse(3), private(rho,ux,vx,wx,uy,vy,wy,uz,vz,wz,dif_x,dif_y,dif_z,phix,phiy,phiz,delta)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie

        rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i+1,j,k))
        delta = 0.5d0*(p%loc%delta%old(i,j,k)+p%loc%delta%old(i+1,j,k))

        ux = 0.5d0*( p%loc%vel%x%old(i+1,j,k)-p%loc%vel%x%old(i-1,j,k) )/p%glb%dx
        uy = 0.5d0*( p%loc%vel%x%old(i,j+1,k)-p%loc%vel%x%old(i,j-1,k) )/p%glb%dy
        uz = 0.5d0*( p%loc%vel%x%old(i,j,k+1)-p%loc%vel%x%old(i,j,k-1) )/p%glb%dz

        vx = 0.5d0*( p%loc%vel%y%old(i+1,j,k)-p%loc%vel%y%old(i,j,k)+p%loc%vel%y%old(i+1,j-1,k)-p%loc%vel%y%old(i,j-1,k) )/p%glb%dx
        wx = 0.5d0*( p%loc%vel%z%old(i+1,j,k)-p%loc%vel%z%old(i,j,k)+p%loc%vel%z%old(i+1,j,k-1)-p%loc%vel%z%old(i,j,k-1) )/p%glb%dx

        phix = (p%loc%phi%old(i+1,j,k)-p%loc%phi%old(i,j,k))/p%glb%dx
        phiy = 0.25d0*(p%loc%phi%old(i+1,j+1,k)-p%loc%phi%old(i+1,j-1,k)+p%loc%phi%old(i,j+1,k)-p%loc%phi%old(i,j-1,k))/p%glb%dy
        phiz = 0.25d0*(p%loc%phi%old(i+1,j,k+1)-p%loc%phi%old(i+1,j,k-1)+p%loc%phi%old(i,j,k+1)-p%loc%phi%old(i,j,k-1))/p%glb%dz
        
        dif_x = (1.0d0-p%glb%mu_12)*delta*phix*2.0d0*ux/(rho*p%glb%re)
        dif_y = (1.0d0-p%glb%mu_12)*delta*phiy*(uy+vx)/(rho*p%glb%re)
        dif_z = (1.0d0-p%glb%mu_12)*delta*phiz*(uz+wx)/(rho*p%glb%re)

        p%loc%velsrc%x%tmp(i,j,k) = p%loc%velsrc%x%tmp(i,j,k) + dif_x + dif_y + dif_z 

        !-------------------------------------------------------------

        rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i,j+1,k))
        delta = 0.5d0*(p%loc%delta%old(i,j,k)+p%loc%delta%old(i,j+1,k))

        vx = 0.5d0*( p%loc%vel%y%old(i+1,j,k)-p%loc%vel%y%old(i-1,j,k) )/p%glb%dx
        vy = 0.5d0*( p%loc%vel%y%old(i,j+1,k)-p%loc%vel%y%old(i,j-1,k) )/p%glb%dy
        vz = 0.5d0*( p%loc%vel%y%old(i,j,k+1)-p%loc%vel%y%old(i,j,k-1) )/p%glb%dz

        uy = 0.5d0*( p%loc%vel%x%old(i,j+1,k)-p%loc%vel%x%old(i,j,k)+p%loc%vel%x%old(i-1,j+1,k)-p%loc%vel%x%old(i-1,j,k) )/p%glb%dy
        wy = 0.5d0*( p%loc%vel%z%old(i,j+1,k)-p%loc%vel%z%old(i,j,k)+p%loc%vel%z%old(i,j+1,k-1)-p%loc%vel%z%old(i,j,k-1) )/p%glb%dy

        phix = 0.25d0*(p%loc%phi%old(i+1,j,k)-p%loc%phi%old(i-1,j,k)+p%loc%phi%old(i+1,j+1,k)-p%loc%phi%old(i-1,j+1,k))/p%glb%dx
        phiy = ( p%loc%phi%old(i,j+1,k)-p%loc%phi%old(i,j,k) )/p%glb%dy
        phiz = 0.25d0*(p%loc%phi%old(i,j+1,k+1)-p%loc%phi%old(i,j+1,k-1)+p%loc%phi%old(i,j,k+1)-p%loc%phi%old(i,j,k-1))/p%glb%dz

        dif_x = (1.0d0-p%glb%mu_12)*delta*phix*(uy+vx)/(rho*p%glb%re)
        dif_y = (1.0d0-p%glb%mu_12)*delta*phiy*2.0d0*vy/(rho*p%glb%re)
        dif_z = (1.0d0-p%glb%mu_12)*delta*phiz*(wy+vz)/(rho*p%glb%re)

        p%loc%velsrc%y%tmp(i,j,k) = p%loc%velsrc%y%tmp(i,j,k) + dif_x + dif_y + dif_z 

        !-------------------------------------------------------------

        rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i,j,k+1))
        delta = 0.5d0*(p%loc%delta%old(i,j,k)+p%loc%delta%old(i,j,k+1))
        
        wx = 0.5d0*( p%loc%vel%z%old(i+1,j,k)-p%loc%vel%z%old(i-1,j,k) )/p%glb%dx
        wy = 0.5d0*( p%loc%vel%z%old(i,j+1,k)-p%loc%vel%z%old(i,j-1,k) )/p%glb%dy
        wz = 0.5d0*( p%loc%vel%z%old(i,j,k+1)-p%loc%vel%z%old(i,j,k-1) )/p%glb%dz

        uz = 0.5d0*( p%loc%vel%x%old(i,j,k+1)-p%loc%vel%x%old(i,j,k)+p%loc%vel%x%old(i-1,j,k+1)-p%loc%vel%x%old(i-1,j,k) )/p%glb%dz
        vz = 0.5d0*( p%loc%vel%y%old(i,j,k+1)-p%loc%vel%y%old(i,j,k)+p%loc%vel%y%old(i,j-1,k+1)-p%loc%vel%y%old(i,j-1,k) )/p%glb%dz

        phix = 0.25d0*( p%loc%phi%old(i+1,j,k+1)-p%loc%phi%old(i-1,j,k+1) + p%loc%phi%old(i+1,j,k)-p%loc%phi%old(i-1,j,k) )/p%glb%dx
        phiy = 0.25d0*( p%loc%phi%old(i,j+1,k+1)-p%loc%phi%old(i,j-1,k+1) + p%loc%phi%old(i,j+1,k)-p%loc%phi%old(i,j-1,k) )/p%glb%dy
        phiz = ( p%loc%phi%old(i,j,k+1)-p%loc%phi%old(i,j,k) )/p%glb%dz

        dif_x = (1.0d0-p%glb%mu_12)*delta*phix*(uz+wx)/(rho*p%glb%re)
        dif_y = (1.0d0-p%glb%mu_12)*delta*phiy*(vz+wy)/(rho*p%glb%re)
        dif_z = (1.0d0-p%glb%mu_12)*delta*phiz*2.0d0*wz/(rho*p%glb%re)

        p%loc%velsrc%z%tmp(i,j,k) = p%loc%velsrc%z%tmp(i,j,k) + dif_x + dif_y + dif_z 

    end do
    end do
    end do 
    !$omp end parallel do

end subroutine

subroutine sec_part3
use all 
implicit none
integer :: i,j,k
real(8) :: rho,delta,curv,phix,phiy,phiz

!$omp parallel do collapse(3), private(rho,delta,curv,phix,phiy,phiz)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i+1,j,k))
    delta = 0.5d0*(p%loc%delta%old(i,j,k)+p%loc%delta%old(i+1,j,k))
    curv = (p%loc%normals%curv%old(i,j,k)+p%loc%normals%curv%old(i+1,j,k))/2.0d0

    phix = 0.5d0*( p%loc%normals%x%old(i,j,k)+p%loc%normals%x%old(i+1,j,k) )
    p%loc%velsrc%x%tmp(i,j,k) = p%loc%velsrc%x%tmp(i,j,k) &
        + p%glb%gx*p%glb%btn_g / p%glb%fr - p%glb%btn_sf*curv*delta*phix / (p%glb%we*rho) 

    !--------------------------------------------------

    rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i,j+1,k))
    delta = 0.5d0*(p%loc%delta%old(i,j,k)+p%loc%delta%old(i,j+1,k))
    curv = (p%loc%normals%curv%old(i,j,k)+p%loc%normals%curv%old(i,j+1,k))/2.0d0

    phiy = 0.5d0*( p%loc%normals%y%old(i,j,k)+p%loc%normals%y%old(i,j+1,k) )
    p%loc%velsrc%y%tmp(i,j,k) = p%loc%velsrc%y%tmp(i,j,k) &
        + p%glb%gy*p%glb%btn_g / p%glb%fr - p%glb%btn_sf*curv*delta*phiy / (p%glb%we*rho) 

    !--------------------------------------------------

    rho = 0.5d0*(p%loc%rho%old(i,j,k)+p%loc%rho%old(i,j,k+1))
    delta = 0.5d0*(p%loc%delta%old(i,j,k)+p%loc%delta%old(i,j,k+1))
    curv = (p%loc%normals%curv%old(i,j,k)+p%loc%normals%curv%old(i,j,k+1))/2.0d0

    phiz = 0.5d0*( p%loc%normals%z%old(i,j,k)+p%loc%normals%z%old(i,j,k+1) )
    p%loc%velsrc%z%tmp(i,j,k) = p%loc%velsrc%z%tmp(i,j,k) &
        + p%glb%gz*p%glb%btn_g / p%glb%fr - p%glb%btn_sf*curv*delta*phiz / (p%glb%we*rho) 

enddo
enddo
enddo
!$omp end parallel do

end subroutine