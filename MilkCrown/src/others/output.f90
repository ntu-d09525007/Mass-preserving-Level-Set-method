subroutine output()
use all
implicit none
integer :: i,j,k
real(8) :: e0, e

! level set method, loss of volume/mass in percentage
write(p%fil%ls_mv,*)p%glb%time,100.0d0*(p%glb%imass-p%glb%mass)/p%glb%imass,100.0d0*(p%glb%imassv-p%glb%massv)/p%glb%imassv

e0 = p%glb%Es0 + p%glb%Ek0 + p%glb%Ep0
e = p%glb%Es + p%glb%Ek + p%glb%Ep + p%glb%Ev

write(p%fil%energy, *)p%glb%time, e, e0, p%glb%es, p%glb%ek, p%glb%ep, p%glb%ev

end subroutine

subroutine print_NS_info()
use all 
implicit none

    write(*,'("Divergence :",2ES15.4)')p%glb%vel_div,p%glb%vel_sdiv
    write(*,'("L2 norm    :",ES15.4)')p%glb%ns_l2f
    write(*,'("Linf norm  :",ES15.4)')p%glb%ns_linf
    write(*,*)''
    write(*,'("PPE iters  :",I15)')p%glb%piter
    write(*,'("PPE error  :",ES15.4)')p%glb%ppe_linf
    write(*,*)''     

end subroutine

subroutine print_LS_info()
use all
implicit none

    write(*,'("LS,  Loss of mass  (%) :",ES15.4)')100.0d0*(p%glb%imass-p%glb%mass)/p%glb%imass
    write(*,'("LS,  Loss of volume(%) :",ES15.4)')100.0d0*(p%glb%ivol-p%glb%vol)/p%glb%ivol
    write(*,*)''
    if(p%glb%method==3)then
        write(*,'("VOF, Loss of mass  (%) :",ES15.4)')100.0d0*(p%glb%imassv-p%glb%massv)/p%glb%imassv
        write(*,'("VOF, Loss of volume(%) :",ES15.4)')100.0d0*(p%glb%ivolv-p%glb%volv)/p%glb%ivolv
        write(*,*)''
    endif

end subroutine

subroutine print_CPU_info()
use all 
implicit none
real(8) :: total, totald

    total = p%glb%ls_adv + p%glb%ls_red + p%glb%ns
    write(*,'("Total CPU time(s) :",F15.6)')total
    write(*,'(4A18)')"Inter. Adv.","Inter. Recon.","PPE","NS"
    write(*,'(F17.2,"%",F17.2,"%",F17.2,"%",F17.2,"%")')100.0d0*p%glb%ls_adv/total,100.0d0*p%glb%ls_red/total&
                                            &,100.0d0*p%glb%ppe/total,100.0d0*(p%glb%ns-p%glb%ppe)/total

end subroutine

subroutine print_energy_info()
use all
implicit none
real(8):: e0, e

    e0 = p%glb%Es0 + p%glb%Ek0 + p%glb%Ep0
    e = p%glb%Es + p%glb%Ek + p%glb%Ep + p%glb%Ev

    write(*,'(A20, Es15.4)')"Total Energy loss:",e0-e
    write(*,'(A20, ES15.4)')"Surface Energy:", p%glb%es-p%glb%es0
    write(*,'(A20, ES15.4)')"Kinectic Energy:", p%glb%ek-p%glb%ek0
    write(*,'(A20, ES15.4)')"Potential Energy:", p%glb%ep-p%glb%ep0
    write(*,'(A20, ES15.4)')"Dissipation:", p%glb%ev

end subroutine


subroutine calculate_energy(init)
use all
implicit none
integer :: i,j,k
real(8) :: A, Q, lQ, Ke, lKe, dv, Po
real(8) :: ux, uy, uz, vx, vy, vz, wx, wy, wz
logical :: init

call rho_mu
call find_tensor(p%loc%vel_ten, p%loc%nvel%x%now, p%loc%nvel%y%now, p%loc%nvel%z%now)

dv = p%glb%dx * p%glb%dy * p%glb%dz

A = 0.0
Ke = 0.0
Po = 0.0
Q = 0.0

!$omp parallel do reduction(+:A, Ke, Po, Q), private(lq, lke, ux, uy, uz, vx, vy, vz, wx, wy, wz)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    ! Surface area
    if( abs(p%loc%phi%now(i,j,k)) < p%glb%dx ) A = A + dv

    if( p%loc%phi%now(i,j,k) > 0.0)then

        ! Kinetic energy
        lke = p%loc%nvel%x%now(i,j,k)**2 + p%loc%nvel%y%now(i,j,k)**2 + p%loc%nvel%z%now(i,j,k)**2
        Ke = Ke + lke * p%loc%rho%now(i,j,k) * 0.5 * dv * p%loc%heavy%now(i,j,k)

        ! Potential energy
        Po = Po + p%glb%z(i,j,k) * p%loc%rho%now(i,j,k) * dv * p%loc%heavy%now(i,j,k)

        ! Viscous dissipation
        ux=p%loc%vel_ten%xx(i,j,k);uy=p%loc%vel_ten%xy(i,j,k);uz=p%loc%vel_ten%xz(i,j,k)
        vx=p%loc%vel_ten%yx(i,j,k);vy=p%loc%vel_ten%yy(i,j,k);vz=p%loc%vel_ten%yz(i,j,k)
        wx=p%loc%vel_ten%zx(i,j,k);wy=p%loc%vel_ten%zy(i,j,k);wz=p%loc%vel_ten%zz(i,j,k)

        lq = 0.5 + (uy+vx)**2 + (uz+wx)**2 + (wy+vz)**2 + ux**2 + vy**2 + wz**2
        Q = Q + lq * 2.0 * p%loc%mu%now(i,j,k) * dv * p%loc%heavy%now(i,j,k)
        
    endif

enddo
enddo
enddo
!$omp end parallel do

A = A / p%glb%dx / 2


if(init)then
    
    p%glb%Es0 = A * p%glb%Fr / p%glb%We * p%glb%energy_unit * p%glb%btn_sf
    p%glb%Ek0 = Ke * p%glb%Fr * p%glb%energy_unit
    p%glb%Ep0 = Po * p%glb%energy_unit
    p%glb%Ev = 0.0
    
else

    p%glb%Es = A * p%glb%Fr / p%glb%We * p%glb%energy_unit * p%glb%btn_sf
    p%glb%Ek = Ke * p%glb%Fr * p%glb%energy_unit
    p%glb%Ep = Po * p%glb%energy_unit
    p%glb%Ev = p%glb%Ev + Q * p%glb%dt * p%glb%Fr / p%glb%Re * p%glb%energy_unit

endif


end subroutine
