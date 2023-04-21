subroutine manager_plot(p)
implicit none
class(manager) :: p
integer :: ix,iy,iz,i,j,k,id
character(3) :: name
real(8) :: x,y,z

 write(name,'(I3.3)')p%glb%pid
 open(unit=777,file="./out/"//trim(p%glb%name)//"_mesh.vtk")  
 WRITE(777,'(A)')"# vtk DataFile Version 3.0"
 write(777,'(A)')"vtk TEST"
 WRITE(777,'(A)')"ASCII"
 WRITE(777,'(A)')"DATASET STRUCTURED_POINTS"
 WRITE(777,'(A,3I6)')"DIMENSIONS ",p%glb%node_x,p%glb%node_y,p%glb%node_z
 WRITE(777,'(A,3ES15.4)')"SPACING ",p%glb%dx, p%glb%dy, p%glb%dz  
 WRITE(777,'(A,3ES15.4)')"ORIGIN ",p%glb%x(1,1,1),p%glb%y(1,1,1),p%glb%z(1,1,1)
 WRITE(777,'(A,I12)')"POINT_DATA ",(p%glb%node_x)*(p%glb%node_y)*(p%glb%node_z)
 
 ! 100 format(F20.10)
 ! 101 format(3F20.10)
 
 ! write(777,'(A)')"SCALARS Phi FLOAT"
 ! write(777,'(A)')"LOOKUP_TABLE DEFAULT"
 ! do iz=0, p%glb%grid_z-1
 ! id = iz*p%glb%grid_x*p%glb%grid_y 
 ! do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    ! do iy=0, p%glb%grid_y-1
    ! do j = p%of(id+iy*p%glb%grid_x)%loc%js, p%of(id+iy*p%glb%grid_x)%loc%je
        ! do ix=0, p%glb%grid_x-1
        ! do i = p%of(id+iy*p%glb%grid_x+ix)%loc%is, p%of(id+iy*p%glb%grid_x+ix)%loc%ie
            ! write(777,100)p%of(id+iy*p%glb%grid_x+ix)%loc%phi%now(i,j,k)
        ! enddo
        ! enddo       
    ! enddo
    ! enddo   
 ! enddo
 ! enddo
 
 close(777)
 
end subroutine