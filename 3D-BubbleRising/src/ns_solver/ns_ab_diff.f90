subroutine ns_ab_diff_source
implicit none

    call ns_ab_diff_source_sec
    ! call ns_ab_diff_source_uccd

end subroutine

subroutine ns_ab_diff_source_sec
use all
!$ use omp_lib
implicit none
integer :: id

!$omp parallel do 
do id = 0, p%glb%threads-1

    call ns_ab_diff_job_sec(p%of(id),p%of(id)%loc%velsrc%x%now,p%of(id)%loc%velsrc%y%now,p%of(id)%loc%velsrc%z%now,&
                                     p%of(id)%loc%vel%x%old,p%of(id)%loc%vel%y%old,p%of(id)%loc%vel%z%old, &
                                     p%of(id)%loc%phi%old,p%of(id)%loc%normals%curv%old,p%of(id)%loc%delta%old,&
                                     p%of(id)%loc%normals%x%old, p%of(id)%loc%normals%y%old, p%of(id)%loc%normals%z%old, &
                                     p%of(id)%loc%rho%old,p%of(id)%loc%mu%old,.false.,1.0d0)
    
enddo
!$omp end parallel do
    
end subroutine

subroutine ns_ab_diff_job_sec(q,sx,sy,sz,u,v,w,phi,curv,delta,nx,ny,nz,rho,mu,reset,alpha)
use all
implicit none
type(job) :: q
real(8), dimension(q%loc%is-q%glb%ghc:q%loc%ie+q%glb%ghc,&
                  &q%loc%js-q%glb%ghc:q%loc%je+q%glb%ghc,&
                  &q%loc%ks-q%glb%ghc:q%loc%ke+q%glb%ghc) :: sx,sy,sz,u,v,w,phi,curv,delta,rho,mu,nx,ny,nz
integer :: i,j,k
logical :: reset
real(8) :: alpha
real(8) :: rho_, mu_, delta_, curv_
real(8) :: xx,yy,zz
real(8) :: ux,uy,uz,vx,vy,vz,wx,wy,wz
real(8) :: phix,phiy,phiz
real(8) :: dif_x, dif_y, dif_z

if(reset)then

    !$omp parallel do num_threads(q%glb%nthreads) collapse(3) private(i,j,k)
    do k = q%loc%ks, q%loc%ke
    do j = q%loc%js, q%loc%je
    do i = q%loc%is, q%loc%ie
        sx(i,j,k) = 0.0d0
        sy(i,j,k) = 0.0d0
        sz(i,j,k) = 0.0d0
    enddo
    enddo
    enddo
    !$omp end parallel do

endif

!$omp parallel do num_threads(q%glb%nthreads) collapse(3) private(rho_,mu_,delta_,curv_) &
!$omp& private(xx,yy,zz,ux,uy,uz,vx,vy,vz,wx,wy,wz) &
!$omp& private(phix,phiy,phiz, dif_x, dif_y, dif_z)
do k = q%loc%ks, q%loc%ke
do j = q%loc%js, q%loc%je
do i = q%loc%is, q%loc%ie

    rho_   = 0.5d0*(  rho(i,j,k)+  rho(i+1,j,k))
    mu_    = 0.5d0*(   mu(i,j,k)+   mu(i+1,j,k))
    delta_ = 0.5d0*(delta(i,j,k)+delta(i+1,j,k))
    curv_  = 0.5d0*( curv(i,j,k)+ curv(i+1,j,k))

    xx = (u(i+1,j,k)-2.0d0*u(i,j,k)+u(i-1,j,k))/q%glb%dx**2.0d0
    yy = (u(i,j+1,k)-2.0d0*u(i,j,k)+u(i,j-1,k))/q%glb%dy**2.0d0
    zz = (u(i,j,k+1)-2.0d0*u(i,j,k)+u(i,j,k-1))/q%glb%dz**2.0d0

    ux = 0.5d0*(u(i+1,j,k)-u(i-1,j,k))/q%glb%dx
    uy = 0.5d0*(u(i,j+1,k)-u(i,j-1,k))/q%glb%dy
    uz = 0.5d0*(u(i,j,k+1)-u(i,j,k-1))/q%glb%dz

    vx = 0.5d0*( v(i+1,j,k)-v(i,j,k)+v(i+1,j-1,k)-v(i,j-1,k) )/q%glb%dx
    wx = 0.5d0*( w(i+1,j,k)-w(i,j,k)+w(i+1,j,k-1)-w(i,j,k-1) )/q%glb%dx

    ! phix = (phi(i+1,j,k)-phi(i,j,k))/q%glb%dx
    ! phiy = 0.25d0*(phi(i+1,j+1,k)-phi(i+1,j-1,k)+phi(i,j+1,k)-phi(i,j-1,k))/q%glb%dy
    ! phiz = 0.25d0*(phi(i+1,j,k+1)-phi(i+1,j,k-1)+phi(i,j,k+1)-phi(i,j,k-1))/q%glb%dz 

    phix = 0.5d0*( nx(i,j,k) + nx(i+1,j,k) )
    phiy = 0.5d0*( ny(i,j,k) + ny(i+1,j,k) )
    phiz = 0.5d0*( nz(i,j,k) + nz(i+1,j,k) )

    dif_x = mu_ / rho_ * xx / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phix * 2.0d0*ux / ( rho_ * q%glb%re)
    dif_y = mu_ / rho_ * yy / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phiy * (uy+vx)  / ( rho_ * q%glb%re)
    dif_z = mu_ / rho_ * zz / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phiz * (uz+wx)  / ( rho_ * q%glb%re)

    sx(i,j,k) = sx(i,j,k) + alpha * ( dif_x+dif_y+dif_z + q%glb%gx*q%glb%btn_g / q%glb%fr &
            & - q%glb%btn_sf*curv_*delta_*phix / (q%glb%we*rho_)  )

    ! ==========================================================================================

    rho_   = 0.5d0*(  rho(i,j,k)+  rho(i,j+1,k))
    mu_    = 0.5d0*(   mu(i,j,k)+   mu(i,j+1,k))
    delta_ = 0.5d0*(delta(i,j,k)+delta(i,j+1,k))
    curv_  = 0.5d0*( curv(i,j,k)+ curv(i,j+1,k))

    xx = (v(i+1,j,k)-2.0d0*v(i,j,k)+v(i-1,j,k))/q%glb%dx**2.0d0
    yy = (v(i,j+1,k)-2.0d0*v(i,j,k)+v(i,j-1,k))/q%glb%dy**2.0d0
    zz = (v(i,j,k+1)-2.0d0*v(i,j,k)+v(i,j,k-1))/q%glb%dz**2.0d0

    vx = 0.5d0*(v(i+1,j,k)-v(i-1,j,k))/q%glb%dx
    vy = 0.5d0*(v(i,j+1,k)-v(i,j-1,k))/q%glb%dy
    vz = 0.5d0*(v(i,j,k+1)-v(i,j,k-1))/q%glb%dz

    uy = 0.5d0*( u(i,j+1,k)-u(i,j,k)+u(i-1,j+1,k)-u(i-1,j,k) )/q%glb%dy
    wy = 0.5d0*( w(i,j+1,k)-w(i,j,k)+w(i,j+1,k-1)-w(i,j,k-1) )/q%glb%dy

    ! phix = 0.25d0*(phi(i+1,j,k)-phi(i-1,j,k)+phi(i+1,j+1,k)-phi(i-1,j+1,k))/q%glb%dx
    ! phiy = ( phi(i,j+1,k)-phi(i,j,k) )/q%glb%dy
    ! phiz = 0.25d0*(phi(i,j+1,k+1)-phi(i,j+1,k-1)+phi(i,j,k+1)-phi(i,j,k-1))/q%glb%dz

    phix = 0.5d0*( nx(i,j,k) + nx(i,j+1,k) )
    phiy = 0.5d0*( ny(i,j,k) + ny(i,j+1,k) )
    phiz = 0.5d0*( nz(i,j,k) + nz(i,j+1,k) )

    dif_x = mu_ / rho_ * xx / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phix*(uy+vx) / ( rho_ * q%glb%re)
    dif_y = mu_ / rho_ * yy / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phiy*2.0d0*vy/ ( rho_ * q%glb%re)
    dif_z = mu_ / rho_ * zz / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phiz*(wy+vz) / ( rho_ * q%glb%re)

    sy(i,j,k) = sy(i,j,k) + alpha * ( dif_x+dif_y+dif_z + q%glb%gy * q%glb%btn_g / q%glb%fr &
            & - q%glb%btn_sf * curv_*delta_ * phiy / (q%glb%we*rho_)  )

    ! ==========================================================================================

    rho_   = 0.5d0*(  rho(i,j,k)+  rho(i,j,k+1))
    mu_    = 0.5d0*(   mu(i,j,k)+   mu(i,j,k+1))
    delta_ = 0.5d0*(delta(i,j,k)+delta(i,j,k+1))
    curv_  = 0.5d0*( curv(i,j,k)+ curv(i,j,k+1))

    wx = 0.5d0*( w(i+1,j,k)-w(i-1,j,k) )/q%glb%dx
    wy = 0.5d0*( w(i,j+1,k)-w(i,j-1,k) )/q%glb%dy
    wz = 0.5d0*( w(i,j,k+1)-w(i,j,k-1) )/q%glb%dz

    uz = 0.5d0*( u(i,j,k+1)-u(i,j,k)+u(i-1,j,k+1)-u(i-1,j,k) )/q%glb%dz
    vz = 0.5d0*( v(i,j,k+1)-v(i,j,k)+v(i,j-1,k+1)-v(i,j-1,k) )/q%glb%dz

    ! phix = 0.25d0*( phi(i+1,j,k+1) - phi(i-1,j,k+1) + phi(i+1,j,k) - phi(i-1,j,k) )/q%glb%dx
    ! phiy = 0.25d0*( phi(i,j+1,k+1) - phi(i,j-1,k+1) + phi(i,j+1,k) - phi(i,j-1,k) )/q%glb%dy
    ! phiz = ( phi(i,j,k+1) - phi(i,j,k) ) / q%glb%dz

    phix = 0.5d0*( nx(i,j,k) + nx(i,j,k+1) )
    phiy = 0.5d0*( ny(i,j,k) + ny(i,j,k+1) )
    phiz = 0.5d0*( nz(i,j,k) + nz(i,j,k+1) )
    
    dif_x = mu_ / rho_ * xx / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phix * (uz+wx)  / ( rho_ * q%glb%re )
    dif_y = mu_ / rho_ * yy / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phiy * (vz+wy)  / ( rho_ * q%glb%re )
    dif_z = mu_ / rho_ * zz / q%glb%re !+ (1.0d0-q%glb%mu_12) * delta_ * phiz * 2.0d0*wz / ( rho_ * q%glb%re )

    sz(i,j,k) = sz(i,j,k) + alpha * ( dif_x+dif_y+dif_z + q%glb%gz * q%glb%btn_g / q%glb%fr &
            & - q%glb%btn_sf * curv_ * delta_ * phiz / (q%glb%we*rho_)  )
enddo
enddo
enddo
!$omp end parallel do

end subroutine

subroutine ns_ab_diff_source_uccd()
use all
!$ use omp_lib
implicit none
integer :: id

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

!$omp parallel do
do id = 0, p%glb%threads-1
    
    call ns_split_diff_source( p%of(id), &
                              &p%of(id)%loc%tdata%x%s1, p%of(id)%loc%tdata%y%s1, p%of(id)%loc%tdata%z%s1, &
                              &p%of(id)%loc%tdata%x%s2, p%of(id)%loc%tdata%y%s2, p%of(id)%loc%tdata%z%s2, &
                              &p%of(id)%loc%vel%x%old, p%of(id)%loc%vel%y%old, p%of(id)%loc%vel%z%old, &
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
                                &p%of(id)%loc%vel%x%tmp, p%of(id)%loc%vel%y%tmp, p%of(id)%loc%vel%z%tmp )
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
                              &p%of(id)%loc%vel%x%tmp, p%of(id)%loc%vel%y%tmp, p%of(id)%loc%vel%z%tmp, &
                              &p%of(id)%loc%velsrc%x%tmp, p%of(id)%loc%velsrc%y%tmp, p%of(id)%loc%velsrc%z%tmp, &
                              &p%of(id)%loc%rho%old,p%of(id)%loc%mu%old,p%of(id)%loc%delta%old,p%of(id)%loc%normals%curv%old, &
                              &p%of(id)%loc%normals%x%old, p%of(id)%loc%normals%y%old, p%of(id)%loc%normals%z%old, &
                              &.false., 0.5d0)
    
enddo    
!$omp end parallel do


end subroutine