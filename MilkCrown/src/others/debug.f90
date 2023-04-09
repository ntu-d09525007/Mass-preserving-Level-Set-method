subroutine find_momentum()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: momx,momy,momz,dv
    
    momx=0.0;momy=0.0;momz=0.0
    dv = p%glb%dx*p%glb%dy*p%glb%dz
    
    !$omp parallel do collapse(3), reduction(+:momx,momy,momz)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        momx = momx + p%loc%heavy%now(i,j,k)*p%loc%vel%x%now(i,j,k)*dv
        momy = momy + p%loc%heavy%now(i,j,k)*p%loc%vel%y%now(i,j,k)*dv
        momz = momz + p%loc%heavy%now(i,j,k)*p%loc%vel%z%now(i,j,k)*dv
        
    end do
    end do
    end do
    !$omp end parallel do
    
    momx = momx / p%glb%vol
    momy = momy / p%glb%vol
    momz = momz / p%glb%vol
    
    write(*,*)''
    write(*,'("X momentum  :",F12.5)')momx
    write(*,'("Y momentum  :",F12.5)')momy
    write(*,'("Z momentum  :",F12.5)')momz

end subroutine