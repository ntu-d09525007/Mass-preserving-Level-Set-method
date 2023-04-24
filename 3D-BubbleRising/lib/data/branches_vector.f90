subroutine job_find_tensor(p,tens,u,v,w)
implicit none
class(job) :: p
type(tensor) :: tens
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w
integer :: i, j, k

do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
    call p%loc%ccdsolvers%x%solve("ccd",u(:,j,k),tens%xx(:,j,k),tens%xxx(:,j,k))
    call p%loc%ccdsolvers%x%solve("ccd",v(:,j,k),tens%yx(:,j,k),tens%yxx(:,j,k))
    call p%loc%ccdsolvers%x%solve("ccd",w(:,j,k),tens%zx(:,j,k),tens%zxx(:,j,k))
enddo
enddo

do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%y%solve("ccd",u(i,:,k),tens%xy(i,:,k),tens%xyy(i,:,k))
    call p%loc%ccdsolvers%y%solve("ccd",v(i,:,k),tens%yy(i,:,k),tens%yyy(i,:,k))
    call p%loc%ccdsolvers%y%solve("ccd",w(i,:,k),tens%zy(i,:,k),tens%zyy(i,:,k))
enddo
enddo

do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%z%solve("ccd",u(i,j,:),tens%xz(i,j,:),tens%xzz(i,j,:))
    call p%loc%ccdsolvers%z%solve("ccd",v(i,j,:),tens%yz(i,j,:),tens%yzz(i,j,:))
    call p%loc%ccdsolvers%z%solve("ccd",w(i,j,:),tens%zz(i,j,:),tens%zzz(i,j,:))
enddo
enddo

end subroutine

subroutine job_find_gradient(p,tens,phi)
implicit none
class(job) :: p
type(tensor) :: tens
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: phi
integer :: i,j,k

do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
    call p%loc%ccdsolvers%x%solve("ccd",phi(:,j,k),tens%x(:,j,k),tens%tmp(:,j,k) )
enddo
enddo

do k = p%loc%ks, p%loc%ke
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%y%solve("ccd",phi(i,:,k),tens%y(i,:,k),tens%tmp(i,:,k) )
enddo
enddo

do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    call p%loc%ccdsolvers%z%solve("ccd",phi(i,j,:),tens%z(i,j,:),tens%tmp(i,j,:) )
enddo
enddo

end subroutine