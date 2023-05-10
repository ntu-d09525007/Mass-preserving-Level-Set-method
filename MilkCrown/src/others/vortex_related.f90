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

    ! call p%rho_mu

    ! Qmax=1.0d-3
    ! !$omp parallel do private(i,j,k,u,v,w,ux,uy,uz,vx,vy,vz,wx,wy,wz,Q,An2,Bn2), reduction(max:Qmax), &
    ! !$omp& private(uxx,uyy,uzz,vxx,vyy,vzz,wxx,wyy,wzz)
    ! do id = 0, p%glb%threads-1
    
    !     call p%of(id)%find_tensor(p%of(id)%loc%vel_ten, p%of(id)%loc%nvel%x%now, p%of(id)%loc%nvel%y%now, p%of(id)%loc%nvel%z%now)
        
    !     do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    !     do j = p%of(id)%loc%js, p%of(id)%loc%je
    !     do i = p%of(id)%loc%is, p%of(id)%loc%ie

    !         u=p%of(id)%loc%nvel%x%now(i,j,k);v=p%of(id)%loc%nvel%y%now(i,j,k);w=p%of(id)%loc%nvel%z%now(i,j,k)

    !         ux=p%of(id)%loc%vel_ten%xx(i,j,k);uy=p%of(id)%loc%vel_ten%xy(i,j,k);uz=p%of(id)%loc%vel_ten%xz(i,j,k)
    !         vx=p%of(id)%loc%vel_ten%yx(i,j,k);vy=p%of(id)%loc%vel_ten%yy(i,j,k);vz=p%of(id)%loc%vel_ten%yz(i,j,k)
    !         wx=p%of(id)%loc%vel_ten%zx(i,j,k);wy=p%of(id)%loc%vel_ten%zy(i,j,k);wz=p%of(id)%loc%vel_ten%zz(i,j,k)

    !         uxx=p%of(id)%loc%vel_ten%xxx(i,j,k);uyy=p%of(id)%loc%vel_ten%xyy(i,j,k);uzz=p%of(id)%loc%vel_ten%xzz(i,j,k)
    !         vxx=p%of(id)%loc%vel_ten%yxx(i,j,k);vyy=p%of(id)%loc%vel_ten%yyy(i,j,k);vzz=p%of(id)%loc%vel_ten%yzz(i,j,k)
    !         wxx=p%of(id)%loc%vel_ten%zxx(i,j,k);wyy=p%of(id)%loc%vel_ten%zyy(i,j,k);wzz=p%of(id)%loc%vel_ten%zzz(i,j,k)
            
    !         An2 = Ux**2.0d0 + Vy**2.0d0 + Wz**2.0d0 + 0.5d0*( (Uy+Vx)**2.0d0+(Uz+Wx)**2.0d0+(Vz+Wy)**2.0d0 )
    !         Bn2 = 0.5d0*( (Uy-Vx)**2.0d0 + (Uz-Wx)**2.0d0 + (Vz-Wy)**2.0d0 )
                    
    !         Q = 0.5d0*( Bn2-An2 )
    !         Qmax = max(Q,Qmax)
            
    !         p%of(id)%loc%q_cri%now(i,j,k) = Q

    !         p%of(id)%loc%omega_cri%now(i,j,k) = Bn2
    !         p%of(id)%loc%omega_cri%old(i,j,k) = Bn2+An2
            
    !         p%of(id)%loc%vort%x%now(i,j,k) = Wy-Vz
    !         p%of(id)%loc%vort%y%now(i,j,k) = Uz-Wx
    !         p%of(id)%loc%vort%z%now(i,j,k) = Vx-Uy
            
    !         p%of(id)%loc%lamb%x%now(i,j,k) = - p%of(id)%loc%nvel%y%now(i,j,k)*p%of(id)%loc%vort%z%now(i,j,k) &
    !                                       &  + p%of(id)%loc%nvel%z%now(i,j,k)*p%of(id)%loc%vort%y%now(i,j,k)
                                        
    !         p%of(id)%loc%lamb%y%now(i,j,k) = - p%of(id)%loc%nvel%z%now(i,j,k)*p%of(id)%loc%vort%x%now(i,j,k) &
    !                                       &  + p%of(id)%loc%nvel%x%now(i,j,k)*p%of(id)%loc%vort%z%now(i,j,k)
                                        
    !         p%of(id)%loc%lamb%z%now(i,j,k) = - p%of(id)%loc%nvel%x%now(i,j,k)*p%of(id)%loc%vort%y%now(i,j,k) &
    !                                       &  + p%of(id)%loc%nvel%y%now(i,j,k)*p%of(id)%loc%vort%x%now(i,j,k)

    !         p%of(id)%loc%lamb_div%now(i,j,k) = -u*(uxx+uyy+uzz)-v*(vxx+vyy+vzz)-w*(wxx+wyy+wzz) &
    !         - p%of(id)%loc%vort%x%now(i,j,k)**2.0d0 - p%of(id)%loc%vort%y%now(i,j,k)**2.0d0 - p%of(id)%loc%vort%z%now(i,j,k)**2.0d0
    !     end do
    !     end do
    !     end do
        
    !     call p%of(id)%bc(0,p%of(id)%loc%vort%x%now)
    !     call p%of(id)%bc(0,p%of(id)%loc%vort%y%now)
    !     call p%of(id)%bc(0,p%of(id)%loc%vort%z%now) 
        
    ! enddo  
    ! !$omp end parallel do
    
    ! call pt%vort%sync
    
    ! !$omp parallel do private(i,j,k,ux,uy,uz,vx,vy,vz,wx,wy,wz,uxx,uyy,uzz,vxx,vyy,vzz,wxx,wyy,wzz), &
    ! !$omp& private(wx_x,wx_y,wx_z,wy_x,wy_y,wy_z,wz_x,wz_y,wz_z,rho,rhox,rhoy,rhoz,px,py,pz), &
    ! !$omp& private(wx_xx,wx_yy,wx_zz,wy_xx,wy_yy,wy_zz,wz_xx,wz_yy,wz_zz)
    ! do id = 0, p%glb%threads-1

    !     call p%of(id)%find_tensor(p%of(id)%loc%vor_ten, p%of(id)%loc%vort%x%now, p%of(id)%loc%vort%y%now, p%of(id)%loc%vort%z%now)
    !     call p%of(id)%find_gradient(p%of(id)%loc%p_ten, p%of(id)%loc%p%now)
    !     call p%of(id)%find_gradient(p%of(id)%loc%rho_ten, p%of(id)%loc%rho%now)
        
    !     do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    !     do j = p%of(id)%loc%js, p%of(id)%loc%je
    !     do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
    !         p%of(id)%loc%omega_cri%old(i,j,k) = p%of(id)%loc%omega_cri%old(i,j,k) + 0.002d0*Qmax*2.0d0
    !         p%of(id)%loc%omega_cri%now(i,j,k) = p%of(id)%loc%omega_cri%now(i,j,k) + 0.002d0*Qmax
            
    !         p%of(id)%loc%omega_cri%now(i,j,k) = p%of(id)%loc%omega_cri%now(i,j,k) / p%of(id)%loc%omega_cri%old(i,j,k)
        
    !     end do
    !     end do
    !     end do

    !     do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    !     do j = p%of(id)%loc%js, p%of(id)%loc%je
    !     do i = p%of(id)%loc%is, p%of(id)%loc%ie

    !         ux=p%of(id)%loc%vel_ten%xx(i,j,k);uy=p%of(id)%loc%vel_ten%xy(i,j,k);uz=p%of(id)%loc%vel_ten%xz(i,j,k)
    !         vx=p%of(id)%loc%vel_ten%yx(i,j,k);vy=p%of(id)%loc%vel_ten%yy(i,j,k);vz=p%of(id)%loc%vel_ten%yz(i,j,k)
    !         wx=p%of(id)%loc%vel_ten%zx(i,j,k);wy=p%of(id)%loc%vel_ten%zy(i,j,k);wz=p%of(id)%loc%vel_ten%zz(i,j,k)

    !         uxx=p%of(id)%loc%vel_ten%xxx(i,j,k);uyy=p%of(id)%loc%vel_ten%xyy(i,j,k);uzz=p%of(id)%loc%vel_ten%xzz(i,j,k)
    !         vxx=p%of(id)%loc%vel_ten%yxx(i,j,k);vyy=p%of(id)%loc%vel_ten%yyy(i,j,k);vzz=p%of(id)%loc%vel_ten%yzz(i,j,k)
    !         wxx=p%of(id)%loc%vel_ten%zxx(i,j,k);wyy=p%of(id)%loc%vel_ten%zyy(i,j,k);wzz=p%of(id)%loc%vel_ten%zzz(i,j,k)

    !         wx_x=p%of(id)%loc%vor_ten%xx(i,j,k);wx_y=p%of(id)%loc%vor_ten%xy(i,j,k);wx_z=p%of(id)%loc%vor_ten%xz(i,j,k)
    !         wy_x=p%of(id)%loc%vor_ten%yx(i,j,k);wy_y=p%of(id)%loc%vor_ten%yy(i,j,k);wy_z=p%of(id)%loc%vor_ten%yz(i,j,k)
    !         wz_x=p%of(id)%loc%vor_ten%zx(i,j,k);wz_y=p%of(id)%loc%vor_ten%zy(i,j,k);wz_z=p%of(id)%loc%vor_ten%zz(i,j,k)

    !         wx_xx=p%of(id)%loc%vor_ten%xxx(i,j,k);wx_yy=p%of(id)%loc%vor_ten%xyy(i,j,k);wx_zz=p%of(id)%loc%vor_ten%xzz(i,j,k)
    !         wy_xx=p%of(id)%loc%vor_ten%yxx(i,j,k);wy_yy=p%of(id)%loc%vor_ten%yyy(i,j,k);wy_zz=p%of(id)%loc%vor_ten%yzz(i,j,k)
    !         wz_xx=p%of(id)%loc%vor_ten%zxx(i,j,k);wz_yy=p%of(id)%loc%vor_ten%zyy(i,j,k);wz_zz=p%of(id)%loc%vor_ten%zzz(i,j,k)

    !         rho = p%of(id)%loc%rho%now(i,j,k)

    !         rhox = p%of(id)%loc%rho_ten%x(i,j,k)
    !         rhoy = p%of(id)%loc%rho_ten%y(i,j,k)
    !         rhoz = p%of(id)%loc%rho_ten%z(i,j,k)

    !         px =  p%of(id)%loc%p_ten%x(i,j,k)
    !         py =  p%of(id)%loc%p_ten%y(i,j,k)
    !         pz =  p%of(id)%loc%p_ten%z(i,j,k)

    !         ! ============================================================================
    !         p%of(id)%loc%vort_adv%x%now(i,j,k) = p%of(id)%loc%nvel%x%now(i,j,k)*wx_x + &
    !                                              p%of(id)%loc%nvel%y%now(i,j,k)*wx_y + &
    !                                              p%of(id)%loc%nvel%z%now(i,j,k)*wx_z 

    !         p%of(id)%loc%vort_adv%y%now(i,j,k) = p%of(id)%loc%nvel%x%now(i,j,k)*wy_x + &
    !                                              p%of(id)%loc%nvel%y%now(i,j,k)*wy_y + &
    !                                              p%of(id)%loc%nvel%z%now(i,j,k)*wy_z 

    !         p%of(id)%loc%vort_adv%z%now(i,j,k) = p%of(id)%loc%nvel%x%now(i,j,k)*wz_x + &
    !                                              p%of(id)%loc%nvel%y%now(i,j,k)*wz_y + &
    !                                              p%of(id)%loc%nvel%z%now(i,j,k)*wz_z 
    !         ! ============================================================================
    !         p%of(id)%loc%vort_tws%x%now(i,j,k) = p%of(id)%loc%vort%x%now(i,j,k)*ux + &
    !                                              p%of(id)%loc%vort%y%now(i,j,k)*uy + &
    !                                              p%of(id)%loc%vort%z%now(i,j,k)*uz 

    !         p%of(id)%loc%vort_tws%y%now(i,j,k) = p%of(id)%loc%vort%x%now(i,j,k)*vx + &
    !                                              p%of(id)%loc%vort%y%now(i,j,k)*vy + &
    !                                              p%of(id)%loc%vort%z%now(i,j,k)*vz

    !         p%of(id)%loc%vort_tws%z%now(i,j,k) = p%of(id)%loc%vort%x%now(i,j,k)*wx + &
    !                                              p%of(id)%loc%vort%y%now(i,j,k)*wy + &
    !                                              p%of(id)%loc%vort%z%now(i,j,k)*wz
    !         ! ============================================================================
    !         p%of(id)%loc%vort_baro%x%now(i,j,k) = (rhoy*pz-rhoz*py)/rho**2.0d0
    !         p%of(id)%loc%vort_baro%y%now(i,j,k) = (rhoz*px-rhox*pz)/rho**2.0d0
    !         p%of(id)%loc%vort_baro%z%now(i,j,k) = (rhox*py-rhoy*px)/rho**2.0d0
    !         ! ============================================================================
    !         p%of(id)%loc%vort_visc%x%now(i,j,k) = ( (wx_xx+wx_yy+wx_zz)/rho-(rhoy*(wxx+wyy+wzz)-rhoz*(vxx+vyy+vzz))/rho**2.0d0 )/p%glb%re
    !         p%of(id)%loc%vort_visc%y%now(i,j,k) = ( (wy_xx+wy_yy+wy_zz)/rho-(rhoz*(uxx+uyy+uzz)-rhox*(wxx+wyy+wzz))/rho**2.0d0 )/p%glb%re
    !         p%of(id)%loc%vort_visc%z%now(i,j,k) = ( (wz_xx+wz_yy+wz_zz)/rho-(rhox*(vxx+vyy+vzz)-rhoy*(uxx+uyy+uzz))/rho**2.0d0 )/p%glb%re

    !     enddo
    !     enddo
    !     enddo
        
    ! enddo
    ! !$omp end parallel do 


end subroutine