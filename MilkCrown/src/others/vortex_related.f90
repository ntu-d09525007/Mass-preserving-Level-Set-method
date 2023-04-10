subroutine find_tensor(tens,u,v,w)
use all
implicit none
type(tensor) :: tens
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w
integer :: i, j, k

!$omp parallel do collapse(2)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
    call p%loc%ccdsolvers%x%solve("ccd",u(:,j,k),tens%xx(:,j,k),tens%xxx(:,j,k))
    call p%loc%ccdsolvers%x%solve("ccd",v(:,j,k),tens%yx(:,j,k),tens%yxx(:,j,k))
    call p%loc%ccdsolvers%x%solve("ccd",w(:,j,k),tens%zx(:,j,k),tens%zxx(:,j,k))
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%y%solve("ccd",u(i,:,k),tens%xy(i,:,k),tens%xyy(i,:,k))
    call p%loc%ccdsolvers%y%solve("ccd",v(i,:,k),tens%yy(i,:,k),tens%yyy(i,:,k))
    call p%loc%ccdsolvers%y%solve("ccd",w(i,:,k),tens%zy(i,:,k),tens%zyy(i,:,k))
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%z%solve("ccd",u(i,j,:),tens%xz(i,j,:),tens%xzz(i,j,:))
    call p%loc%ccdsolvers%z%solve("ccd",v(i,j,:),tens%yz(i,j,:),tens%yzz(i,j,:))
    call p%loc%ccdsolvers%z%solve("ccd",w(i,j,:),tens%zz(i,j,:),tens%zzz(i,j,:))
enddo
enddo
!$omp end parallel do

end subroutine

subroutine find_gradient(tens,phi)
use all
implicit none
type(tensor) :: tens
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: phi
integer :: i,j,k

!$omp parallel do collapse(2)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
    call p%loc%ccdsolvers%x%solve("ccd",phi(:,j,k),tens%x(:,j,k),tens%tmp(:,j,k))
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%y%solve("ccd",phi(i,:,k),tens%y(i,:,k),tens%tmp(i,:,k))
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%z%solve("ccd",phi(i,j,:),tens%z(i,j,:),tens%tmp(i,j,:))
enddo
enddo
!$omp end parallel do

end subroutine

subroutine vortex_dynamics
use all
implicit none
integer :: id,i,j,k
real(8) :: u,v,w
real(8) :: ux,uy,uz,vx,vy,vz,wx,wy,wz
real(8) :: uxx,uyy,uzz,vxx,vyy,vzz,wxx,wyy,wzz
real(8) :: wx_x,wx_y,wx_z,wy_x,wy_y,wy_z,wz_x,wz_y,wz_z
real(8) :: wx_xx,wx_yy,wx_zz,wy_xx,wy_yy,wy_zz,wz_xx,wz_yy,wz_zz
real(8) :: rho,rhox,rhoy,rhoz,px,py,pz
real(8) :: R
real(8) :: An2, Bn2, Q, Qmax

id=0

call rho_mu

Qmax=1.0d-3

call find_tensor(p%loc%vel_ten, p%loc%nvel%x%now, p%loc%nvel%y%now, p%loc%nvel%z%now)
 
!$omp parallel do collapse(3), private(u,v,w,ux,uy,uz,vx,vy,vz,wx,wy,wz,Q,An2,Bn2), reduction(max:Qmax), &
!$omp& private(uxx,uyy,uzz,vxx,vyy,vzz,wxx,wyy,wzz)   
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    u=p%loc%nvel%x%now(i,j,k);v=p%loc%nvel%y%now(i,j,k);w=p%loc%nvel%z%now(i,j,k)

    ux=p%loc%vel_ten%xx(i,j,k);uy=p%loc%vel_ten%xy(i,j,k);uz=p%loc%vel_ten%xz(i,j,k)
    vx=p%loc%vel_ten%yx(i,j,k);vy=p%loc%vel_ten%yy(i,j,k);vz=p%loc%vel_ten%yz(i,j,k)
    wx=p%loc%vel_ten%zx(i,j,k);wy=p%loc%vel_ten%zy(i,j,k);wz=p%loc%vel_ten%zz(i,j,k)

    uxx=p%loc%vel_ten%xxx(i,j,k);uyy=p%loc%vel_ten%xyy(i,j,k);uzz=p%loc%vel_ten%xzz(i,j,k)
    vxx=p%loc%vel_ten%yxx(i,j,k);vyy=p%loc%vel_ten%yyy(i,j,k);vzz=p%loc%vel_ten%yzz(i,j,k)
    wxx=p%loc%vel_ten%zxx(i,j,k);wyy=p%loc%vel_ten%zyy(i,j,k);wzz=p%loc%vel_ten%zzz(i,j,k)
    
    An2 = Ux**2.0d0 + Vy**2.0d0 + Wz**2.0d0 + 0.5d0*( (Uy+Vx)**2.0d0+(Uz+Wx)**2.0d0+(Vz+Wy)**2.0d0 )
    Bn2 = 0.5d0*( (Uy-Vx)**2.0d0 + (Uz-Wx)**2.0d0 + (Vz-Wy)**2.0d0 )
            
    Q = 0.5d0*( Bn2-An2 )
    Qmax = max(Q,Qmax)
    
    p%loc%q_cri%now(i,j,k) = Q

    p%loc%omega_cri%now(i,j,k) = Bn2
    p%loc%omega_cri%old(i,j,k) = Bn2+An2
    
    p%loc%vort%x%now(i,j,k) = Wy-Vz
    p%loc%vort%y%now(i,j,k) = Uz-Wx
    p%loc%vort%z%now(i,j,k) = Vx-Uy
    
    p%loc%lamb%x%now(i,j,k) = - p%loc%nvel%y%now(i,j,k)*p%loc%vort%z%now(i,j,k) &
                                  &  + p%loc%nvel%z%now(i,j,k)*p%loc%vort%y%now(i,j,k)
                                
    p%loc%lamb%y%now(i,j,k) = - p%loc%nvel%z%now(i,j,k)*p%loc%vort%x%now(i,j,k) &
                                  &  + p%loc%nvel%x%now(i,j,k)*p%loc%vort%z%now(i,j,k)
                                
    p%loc%lamb%z%now(i,j,k) = - p%loc%nvel%x%now(i,j,k)*p%loc%vort%y%now(i,j,k) &
                                  &  + p%loc%nvel%y%now(i,j,k)*p%loc%vort%x%now(i,j,k)

    p%loc%lamb_div%now(i,j,k) = -u*(uxx+uyy+uzz)-v*(vxx+vyy+vzz)-w*(wxx+wyy+wzz) &
    - p%loc%vort%x%now(i,j,k)**2.0d0 - p%loc%vort%y%now(i,j,k)**2.0d0 - p%loc%vort%z%now(i,j,k)**2.0d0
end do
end do
end do
!$omp end parallel do

call bc(p%loc%vort%x%now)
call bc(p%loc%vort%y%now)
call bc(p%loc%vort%z%now) 

call find_tensor(p%loc%vor_ten, p%loc%vort%x%now, p%loc%vort%y%now, p%loc%vort%z%now)
call find_gradient(p%loc%p_ten, p%loc%p%now)
call find_gradient(p%loc%rho_ten, p%loc%rho%now)

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%omega_cri%old(i,j,k) = p%loc%omega_cri%old(i,j,k) + 0.002d0*Qmax*2.0d0
    p%loc%omega_cri%now(i,j,k) = p%loc%omega_cri%now(i,j,k) + 0.002d0*Qmax
    
    p%loc%omega_cri%now(i,j,k) = p%loc%omega_cri%now(i,j,k) / p%loc%omega_cri%old(i,j,k)

end do
end do
end do
!$omp end parallel do

!$omp parallel do collapse(3), private(ux,uy,uz,vx,vy,vz,wx,wy,wz,uxx,uyy,uzz,vxx,vyy,vzz,wxx,wyy,wzz), &
!$omp& private(wx_x,wx_y,wx_z,wy_x,wy_y,wy_z,wz_x,wz_y,wz_z,rho,rhox,rhoy,rhoz,px,py,pz), &
!$omp& private(wx_xx,wx_yy,wx_zz,wy_xx,wy_yy,wy_zz,wz_xx,wz_yy,wz_zz)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    ux=p%loc%vel_ten%xx(i,j,k);uy=p%loc%vel_ten%xy(i,j,k);uz=p%loc%vel_ten%xz(i,j,k)
    vx=p%loc%vel_ten%yx(i,j,k);vy=p%loc%vel_ten%yy(i,j,k);vz=p%loc%vel_ten%yz(i,j,k)
    wx=p%loc%vel_ten%zx(i,j,k);wy=p%loc%vel_ten%zy(i,j,k);wz=p%loc%vel_ten%zz(i,j,k)

    uxx=p%loc%vel_ten%xxx(i,j,k);uyy=p%loc%vel_ten%xyy(i,j,k);uzz=p%loc%vel_ten%xzz(i,j,k)
    vxx=p%loc%vel_ten%yxx(i,j,k);vyy=p%loc%vel_ten%yyy(i,j,k);vzz=p%loc%vel_ten%yzz(i,j,k)
    wxx=p%loc%vel_ten%zxx(i,j,k);wyy=p%loc%vel_ten%zyy(i,j,k);wzz=p%loc%vel_ten%zzz(i,j,k)

    wx_x=p%loc%vor_ten%xx(i,j,k);wx_y=p%loc%vor_ten%xy(i,j,k);wx_z=p%loc%vor_ten%xz(i,j,k)
    wy_x=p%loc%vor_ten%yx(i,j,k);wy_y=p%loc%vor_ten%yy(i,j,k);wy_z=p%loc%vor_ten%yz(i,j,k)
    wz_x=p%loc%vor_ten%zx(i,j,k);wz_y=p%loc%vor_ten%zy(i,j,k);wz_z=p%loc%vor_ten%zz(i,j,k)

    wx_xx=p%loc%vor_ten%xxx(i,j,k);wx_yy=p%loc%vor_ten%xyy(i,j,k);wx_zz=p%loc%vor_ten%xzz(i,j,k)
    wy_xx=p%loc%vor_ten%yxx(i,j,k);wy_yy=p%loc%vor_ten%yyy(i,j,k);wy_zz=p%loc%vor_ten%yzz(i,j,k)
    wz_xx=p%loc%vor_ten%zxx(i,j,k);wz_yy=p%loc%vor_ten%zyy(i,j,k);wz_zz=p%loc%vor_ten%zzz(i,j,k)

    rho = p%loc%rho%now(i,j,k)

    rhox = p%loc%rho_ten%x(i,j,k)
    rhoy = p%loc%rho_ten%y(i,j,k)
    rhoz = p%loc%rho_ten%z(i,j,k)

    px =  p%loc%p_ten%x(i,j,k)
    py =  p%loc%p_ten%y(i,j,k)
    pz =  p%loc%p_ten%z(i,j,k)

    ! ============================================================================
    p%loc%vort_adv%x%now(i,j,k) = p%loc%nvel%x%now(i,j,k)*wx_x + &
                                         p%loc%nvel%y%now(i,j,k)*wx_y + &
                                         p%loc%nvel%z%now(i,j,k)*wx_z 

    p%loc%vort_adv%y%now(i,j,k) = p%loc%nvel%x%now(i,j,k)*wy_x + &
                                         p%loc%nvel%y%now(i,j,k)*wy_y + &
                                         p%loc%nvel%z%now(i,j,k)*wy_z 

    p%loc%vort_adv%z%now(i,j,k) = p%loc%nvel%x%now(i,j,k)*wz_x + &
                                         p%loc%nvel%y%now(i,j,k)*wz_y + &
                                         p%loc%nvel%z%now(i,j,k)*wz_z 
    ! ============================================================================
    p%loc%vort_tws%x%now(i,j,k) = p%loc%vort%x%now(i,j,k)*ux + &
                                         p%loc%vort%y%now(i,j,k)*uy + &
                                         p%loc%vort%z%now(i,j,k)*uz 

    p%loc%vort_tws%y%now(i,j,k) = p%loc%vort%x%now(i,j,k)*vx + &
                                         p%loc%vort%y%now(i,j,k)*vy + &
                                         p%loc%vort%z%now(i,j,k)*vz

    p%loc%vort_tws%z%now(i,j,k) = p%loc%vort%x%now(i,j,k)*wx + &
                                         p%loc%vort%y%now(i,j,k)*wy + &
                                         p%loc%vort%z%now(i,j,k)*wz
    ! ============================================================================
    p%loc%vort_baro%x%now(i,j,k) = (rhoy*pz-rhoz*py)/rho**2.0d0
    p%loc%vort_baro%y%now(i,j,k) = (rhoz*px-rhox*pz)/rho**2.0d0
    p%loc%vort_baro%z%now(i,j,k) = (rhox*py-rhoy*px)/rho**2.0d0
    ! ============================================================================
    p%loc%vort_visc%x%now(i,j,k) = ( (wx_xx+wx_yy+wx_zz)/rho-(rhoy*(wxx+wyy+wzz)-rhoz*(vxx+vyy+vzz))/rho**2.0d0 )/p%glb%re
    p%loc%vort_visc%y%now(i,j,k) = ( (wy_xx+wy_yy+wy_zz)/rho-(rhoz*(uxx+uyy+uzz)-rhox*(wxx+wyy+wzz))/rho**2.0d0 )/p%glb%re
    p%loc%vort_visc%z%now(i,j,k) = ( (wz_xx+wz_yy+wz_zz)/rho-(rhox*(vxx+vyy+vzz)-rhoy*(uxx+uyy+uzz))/rho**2.0d0 )/p%glb%re

enddo
enddo
enddo
!$omp end parallel do 

end subroutine