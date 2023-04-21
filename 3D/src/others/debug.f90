subroutine find_momentum()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: momx,momy,momz,dv,heavy
    
    momx=0.0;momy=0.0;momz=0.0
    dv = p%glb%dx*p%glb%dy*p%glb%dz
    
    !$omp parallel do private(i,j,k,heavy), reduction(+:momx,momy,momz)
    do id = 0, p%glb%threads-1
        
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            heavy = p%of(id)%loc%heavy%now(i,j,k)
        
            momx = momx + heavy*p%of(id)%loc%vel%x%now(i,j,k)*dv
            momy = momy + heavy*p%of(id)%loc%vel%y%now(i,j,k)*dv
            momz = momz + heavy*p%of(id)%loc%vel%z%now(i,j,k)*dv
            
        end do
        end do
        end do

    enddo
    !$omp end parallel do
    
    momx = momx / p%glb%vol
    momy = momy / p%glb%vol
    momz = momz / p%glb%vol
    
    write(*,*)''
    write(*,'("X momentum  :",F12.5)')momx
    write(*,'("Y momentum  :",F12.5)')momy
    write(*,'("Z momentum  :",F12.5)')momz

end subroutine