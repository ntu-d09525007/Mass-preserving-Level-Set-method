subroutine output()
use all
implicit none
integer :: i,j,k,id
real(8) :: damfront, damh, r

if( p%glb%iter == 1)then
    p%glb%ivol = p%glb%vol
    p%glb%ivolv = p%glb%volv
    call p%sync
endif

! level set method, loss of volume/mass in percentage
write(p%fil%mass,*)p%glb%time,100.0d0*(p%glb%imass-p%glb%mass)/p%glb%imass,100.0d0*(p%glb%imassv-p%glb%massv)/p%glb%imassv
write(p%fil%vol,*)p%glb%time,100.0d0*(p%glb%ivol-p%glb%vol)/p%glb%ivol,100.0d0*(p%glb%ivolv-p%glb%volv)/p%glb%ivolv

p%glb%loss_mass_avg = p%glb%loss_mass_avg + abs(p%glb%imass-p%glb%mass)/p%glb%imass
p%glb%loss_vol_avg = p%glb%loss_vol_avg + abs(p%glb%ivol-p%glb%vol)/p%glb%ivol

p%glb%loss_mass_max = max(p%glb%loss_mass_max, abs(p%glb%imass-p%glb%mass)/p%glb%imass)
p%glb%loss_vol_max = max(p%glb%loss_vol_max, abs(p%glb%ivol-p%glb%vol)/p%glb%ivol)

p%glb%loss_mass_avgv = p%glb%loss_mass_avgv + abs(p%glb%imassv-p%glb%massv)/p%glb%imassv
p%glb%loss_vol_avgv = p%glb%loss_vol_avgv + abs(p%glb%ivolv-p%glb%volv)/p%glb%ivolv

p%glb%loss_mass_maxv = max(p%glb%loss_mass_maxv, abs(p%glb%imassv-p%glb%massv)/p%glb%imassv)
p%glb%loss_vol_maxv = max(p%glb%loss_vol_maxv, abs(p%glb%ivolv-p%glb%volv)/p%glb%ivolv)

! !Drybed 
! damfront = 0.0d0; damh=0.0d0
! !$omp parallel do private(i,j,k), reduction(max:damfront, damh), private(r)
! do id = 0, p%glb%threads-1
    
!     do k = p%of(id)%loc%ks, p%of(id)%loc%ke
!     do j = p%of(id)%loc%js, p%of(id)%loc%je
!     do i = p%of(id)%loc%is, p%of(id)%loc%ie

!         if( p%of(id)%loc%phi%now(i,j,k) * p%of(id)%loc%phi%now(i+1,j,k) < 0.0d0 )then
!             if( p%of(id)%loc%phi%now(i-1,j,k) > 0.0d0 .and. p%of(id)%loc%phi%now(i-2,j,k) >0.0d0 )then
!                 r = abs(p%of(id)%loc%phi%now(i,j,k)) / (abs(p%of(id)%loc%phi%now(i,j,k))+abs(p%of(id)%loc%phi%now(i+1,j,k)))
!                 damfront = max(damfront, p%glb%x(i,j,k) + p%glb%dx) * r
!             endif
!         endif

!     enddo
!     enddo
!     enddo

!     if(p%of(id)%loc%idx == 0)then
        
!         i = p%of(id)%loc%is
!         do k = p%of(id)%loc%ks, p%of(id)%loc%ke
!         do j = p%of(id)%loc%js, p%of(id)%loc%je
!             if( p%of(id)%loc%phi%now(i,j,k) * p%of(id)%loc%phi%now(i,j,k+1) < 0.0d0 )then
!                 if( p%of(id)%loc%phi%now(i,j,k-1) > 0.0d0 .and. p%of(id)%loc%phi%now(i,j,k-2) >0.0d0 )then
!                     r = abs(p%of(id)%loc%phi%now(i,j,k)) / (abs(p%of(id)%loc%phi%now(i,j,k))+abs(p%of(id)%loc%phi%now(i,j,k+1)))
!                     damh = max( damh, p%glb%z(i,j,k) + p%glb%dz * r)
!                 endif
!             endif
!         enddo
!         enddo

!     endif

! enddo
! !$omp end parallel do

! write(p%fil%damdata, *)p%glb%time, damfront, damh

end subroutine

subroutine print_NS_info()
use all 
implicit none
        write(*,'("Divergence :",2ES15.4)')p%glb%vel_div,p%glb%vel_sdiv
        ! write(*,'("L2 norm    :",ES15.4)')p%glb%ns_l2f
        ! write(*,'("Linf norm  :",ES15.4)')p%glb%ns_linf
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
        write(*,'("LS,  redistance error  :",ES15.4)')p%glb%red_error
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

        call pt%cputime(totald)
        total = p%glb%ls_adv + p%glb%ls_red + p%glb%ns
        write(*,'("Total CPU time(s) :",F15.6)')total
        write(*,'(4A18)')"Inter. Adv.","Inter. Recon.","PPE","NS"
        write(*,'(F17.2,"%",F17.2,"%",F17.2,"%",F17.2,"%")')100.0d0*p%glb%ls_adv/total,100.0d0*p%glb%ls_red/total&
                                                &,100.0d0*p%glb%ppe/total,100.0d0*(p%glb%ns-p%glb%ppe)/total
        write(*,*)''
        write(*,'(A18,F17.2,"%")')"Data Sync:",totald/total*100.0d0
end subroutine

subroutine calculate_interface_loss()
use all
implicit none
integer :: id,i,j,k
real(8) :: tgt, src
real(8) :: error, errorv

call p%ls_mv

!$omp parallel do private(i,j,k,src,tgt) reduction(+:error, errorv)
do id = 0, p%glb%threads-1
    
    !$omp parallel do collapse(3) private(i,j,k,src,tgt) reduction(+:error, errorv)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie

        !level-set
        src = heavyside(p%of(id)%loc%phi%tmp2(i,j,k) / p%glb%ls_wid)
        tgt = heavyside(p%of(id)%loc%phi%now(i,j,k)  / p%glb%ls_wid)

        error = error + abs(src-tgt)

        !vof
        src = p%of(id)%loc%vof%tmp2(i,j,k)
        tgt = p%of(id)%loc%phi%now(i,j,k)

        errorv = errorv + abs(src-tgt)

    enddo
    enddo
    enddo
    !$omp end parallel do

enddo
!$omp end parallel do

if(p%glb%method == 3)then
    write(*,'(A10, 2ES15.4)')"VOF Mass:", p%glb%loss_mass_avg / p%glb%iter, p%glb%loss_mass_max
    write(*,'(A10, 2ES15.4)')"VOF Vol:", p%glb%loss_vol_avg / p%glb%iter, p%glb%loss_vol_max
    write(*,'(A10, 2ES15.4)')"VOF Int:", errorv / p%glb%node_x / p%glb%node_y / p%glb%node_z
else
    write(*,'(A10, 2ES15.4)')"LS Mass:", p%glb%loss_mass_avg / p%glb%iter, p%glb%loss_mass_max
    write(*,'(A10, 2ES15.4)')"LS Vol:", p%glb%loss_vol_avg / p%glb%iter, p%glb%loss_vol_max
    write(*,'(A10, 2ES15.4)')"LS Int:", error / p%glb%node_x / p%glb%node_y / p%glb%node_z
endif

end subroutine

function heavyside(x)
implicit none
real(8) :: x, heavyside, eps, pi

eps = 1.0d-12
pi = dacos(-1.0d0)

if( x > 1.0d0 - eps)then
    heavyside = 1.0
else if( x < -1.0 + eps)then
    heavyside = 0.0
else
    heavyside = 0.5d0 * (1.0d0 + x + dsin(pi*x) / pi )
endif

end function
