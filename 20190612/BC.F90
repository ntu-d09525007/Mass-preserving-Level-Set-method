SUBROUTINE BC_LS()
USE PRECISION
USE ls_DATA
IMPLICIT NONE

 CALL BC3D(PHI)

END SUBROUTINE

subroutine bc3d(fi)   
USE PRECISION
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: FI

!$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y

   fi(0,J,K)=fi(1,J,K)
   fi(-1,J,K)=fi(1,J,K)
   fi(-2,J,K)=fi(1,J,K)
   
   fi(NODE_X+1,J,K)=fi(NODE_X,J,K)
   fi(NODE_X+2,J,K)=fi(NODE_X,J,K)
   fi(NODE_X+3,J,K)=fi(NODE_X,J,K)
ENDDO
ENDDO
!$omp end parallel do

!$omp parallel do
DO K = 1, NODE_Z
DO I =-2, NODE_X+3
	    
   fi(I,0,K)=fi(I,1,K)
   fi(I,-1,K)=fi(I,1,K)
   fi(I,-2,K)=fi(I,1,K)
   

   fi(I,NODE_Y+1,K)=fi(I,NODE_Y,K)
   fi(I,NODE_Y+2,K)=fi(I,NODE_Y,K)
   fi(I,NODE_Y+3,K)=fi(I,NODE_Y,K)   
     
ENDDO
ENDDO
!$omp end parallel do

!$omp parallel do
DO J=-2,NODE_Y+3
DO I=-2,NODE_X+3

  fi(I,J,0)=fi(I,J,1)
  fi(I,J,-1)=fi(I,J,1)
  fi(I,J,-2)=fi(I,J,1)
     
  fi(I,J,NODE_Z+1)=fi(I,J,NODE_Z)
  fi(I,J,NODE_Z+2)=fi(I,J,NODE_Z)
  fi(I,J,NODE_Z+3)=fi(I,J,NODE_Z)
 ENDDO
 ENDDO
!$omp end parallel do

end subroutine bc3d

subroutine bcu(fi)
USE PRECISION
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: FI

if(VEL_BC==0) then 

    !$omp parallel do
	  DO K=1,NODE_Z
    DO J=1,NODE_Y
       fi(0,J,K)=0.0_DP
       fi(-1,J,K)= -fi(1,J,K)
       fi(-2,J,K)= -fi(2,J,K)
       fi(NODE_X,J,K)=0.0_DP
       fi(NODE_X+1,J,K)=-fi(NODE_X-1,J,K)
       fi(NODE_X+2,J,K)=-fi(NODE_X-2,J,K)
       fi(NODE_X+3,J,K)=-fi(NODE_X-3,J,K)     
    ENDDO
    ENDDO
    !$omp end parallel do
    	  
    !$omp parallel do
    DO K=1,NODE_Z
    DO I=-2,NODE_X+3

       fi(I,0,K)=-fi(I,1,K)
       fi(I,-1,K)=-fi(I,2,K)
       fi(I,-2,K)=-fi(I,3,K)
       fi(I,NODE_Y+1,K)=-fi(I,NODE_Y,K)
       fi(I,NODE_Y+2,K)=-fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+3,K)=-fi(I,NODE_Y-2,K)     
    ENDDO
    ENDDO
    !$omp end parallel do
    
    !$omp parallel do
    DO J=-2,NODE_Y+3
    DO I=-2,NODE_X+3
        fi(I,J,0)=-fi(I,J,1)
        fi(I,J,-1)=-fi(I,J,2)
        fi(I,J,-2)=-fi(I,J,3)
        fi(I,J,NODE_Z+1)=-fi(I,J,NODE_Z)
        fi(I,J,NODE_Z+2)=-fi(I,J,NODE_Z-1)
        fi(I,J,NODE_Z+3)=-fi(I,J,NODE_Z-2)     
    ENDDO
    ENDDO
    !$omp end parallel do

else if(vel_bc==1)then
	  	
   !$omp parallel do
   DO K=1,NODE_Z
   DO J=1,NODE_Y
       fi(0,J,K)=0.0_DP
       fi(-1,J,K)=-fi(1,J,K)
       fi(-2,J,K)=-fi(2,J,K)
       fi(NODE_X,J,K)=0.0_DP
       fi(NODE_X+1,J,K)=-fi(NODE_X-1,J,K)
       fi(NODE_X+2,J,K)=-fi(NODE_X-2,J,K)
       fi(NODE_X+3,J,K)=-fi(NODE_X-3,J,K)     
    ENDDO
    ENDDO
	  !$omp end parallel do

    !$omp parallel do
    DO K=1,NODE_Z
    DO I=-2,NODE_X+3
       fi(I,0,K)=fi(I,1,K)
       fi(I,-1,K)=fi(I,2,K)
       fi(I,-2,K)=fi(I,3,K)
       fi(I,NODE_Y+1,K)=fi(I,NODE_Y,K)
       fi(I,NODE_Y+2,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+3,K)=fi(I,NODE_Y-2,K)     
    ENDDO
    ENDDO
	  !$omp end parallel do

    !$omp parallel do
    DO J=-2,NODE_Y+3
    DO I=-2,NODE_X+3
       fi(I,J,0)=fi(I,J,1)
       fi(I,J,-1)=fi(I,J,2)
       fi(I,J,-2)=fi(I,J,3)
       fi(I,J,NODE_Z+1)=fi(I,J,NODE_Z)
       fi(I,J,NODE_Z+2)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+3)=fi(I,J,NODE_Z-2)     
    ENDDO
    ENDDO
	  !$omp end parallel do
	  
else if(vel_bc==2)then

   !$omp parallel do
   DO K=1,NODE_Z
   DO J=1,NODE_Y
       fi(0,J,K)=fi(1,J,K)
       fi(-1,J,K)=fi(1,J,K)
       fi(-2,J,K)=fi(1,J,K)
       fi(NODE_X,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+1,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+2,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+3,J,K)=fi(NODE_X-1,J,K)
    ENDDO
    ENDDO
	!$omp end parallel do

    !$omp parallel do
    DO K=1,NODE_Z
    DO I=-2,NODE_X+3
	   fi(I,1,K)=fi(I,2,K)
       fi(I,0,K)=fi(I,2,K)
       fi(I,-1,K)=fi(I,2,K)
       fi(I,-2,K)=fi(I,2,K)
	   fi(I,NODE_Y,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+1,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+2,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+3,K)=fi(I,NODE_Y-1,K)     
    ENDDO
    ENDDO
	!$omp end parallel do

    !$omp parallel do
    DO J=-2,NODE_Y+3
    DO I=-2,NODE_X+3
	   fi(I,J,1)=fi(I,J,2)
       fi(I,J,0)=fi(I,J,2)
       fi(I,J,-1)=fi(I,J,2)
       fi(I,J,-2)=fi(I,J,2)
	   fi(I,J,NODE_Z)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+1)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+2)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+3)=fi(I,J,NODE_Z-1)     
    ENDDO
    ENDDO
	!$omp end parallel do
	
endif 
	  
end subroutine bcu
	  
subroutine bcv(fi)
USE PRECISION
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: FI


if(VEL_BC==0) then 

 !$omp parallel do
  DO K=1,NODE_Z
  DO I=1,NODE_X
     fi(I,0,K)=0.0_DP
     fi(I,-1,K)=-fi(I,1,K)
     fi(I,-2,K)=-fi(I,2,K)
     fi(i,NODE_Y,k)=0.0_DP
     fi(I,NODE_Y+1,K)=-fi(I,NODE_Y-1,K)
     fi(I,NODE_Y+2,K)=-fi(I,NODE_Y-2,K)
     fi(I,NODE_Y+3,K)=-fi(I,NODE_Y-3,K)     
  ENDDO
  ENDDO
	!$omp end parallel do

  !$omp parallel do
  DO K=1,NODE_Z
  DO J=-2,NODE_Y+3
     fi(0,J,K)=-fi(1,J,K)
     fi(-1,J,K)=-fi(2,J,K)
     fi(-2,J,K)=-fi(3,J,K)
     fi(NODE_X+1,J,K)=-fi(NODE_X,J,K)
     fi(NODE_X+2,J,K)=-fi(NODE_X-1,J,K)
     fi(NODE_X+3,J,K)=-fi(NODE_X-2,J,K)     
  ENDDO
  ENDDO
	!$omp end parallel do

  !$omp parallel do
   DO J=-2,NODE_Y+3
   DO I=-2,NODE_X+3
      fi(I,J,0)=-fi(I,J,1)
      fi(I,J,-1)=-fi(I,J,2)
      fi(I,J,-2)=-fi(I,J,3)
      fi(I,J,NODE_Z+1)=-fi(I,J,NODE_Z)
      fi(I,J,NODE_Z+2)=-fi(I,J,NODE_Z-1)
      fi(I,J,NODE_Z+3)=-fi(I,J,NODE_Z-2)     
   ENDDO
   ENDDO
	!$omp end parallel do

else if(VEL_BC==1)THEN

  !$omp parallel do
   DO K=1,NODE_Z
   DO I=1,NODE_X
      fi(I,0,K)=0.0_DP
      fi(I,-1,K)=-fi(I,1,K)
      fi(I,-2,K)=-fi(I,2,K)
	    fi(i,NODE_Y,k)=0.0_DP
      fi(I,NODE_Y+1,K)=-fi(I,NODE_Y-1,K)
      fi(I,NODE_Y+2,K)=-fi(I,NODE_Y-2,K)
      fi(I,NODE_Y+3,K)=-fi(I,NODE_Y-3,K)     
   ENDDO
   ENDDO
	!$omp end parallel do

  !$omp parallel do 
   DO K=1,NODE_Z
   DO J=-2,NODE_Y+3
      fi(0,J,K)=fi(1,J,K)
      fi(-1,J,K)=fi(2,J,K)
      fi(-2,J,K)=fi(3,J,K)
      fi(NODE_X+1,J,K)=fi(NODE_X,J,K)
      fi(NODE_X+2,J,K)=fi(NODE_X-1,J,K)
      fi(NODE_X+3,J,K)=fi(NODE_X-2,J,K)     
   ENDDO
   ENDDO
	!$omp end parallel do
 
  !$omp parallel do
   DO J=-2,NODE_y+3
   DO I=-2,NODE_X+3
      fi(I,J,0)=fi(I,J,1)
      fi(I,J,-1)=fi(I,J,2)
      fi(I,J,-2)=fi(I,J,3)
      fi(I,J,NODE_Z+1)=fi(I,J,NODE_Z)
      fi(I,J,NODE_Z+2)=fi(I,J,NODE_Z-1)
      fi(I,J,NODE_Z+3)=fi(I,J,NODE_Z-2)     
   ENDDO
   ENDDO
	!$omp end parallel do

else if(vel_bc==2)then

   !$omp parallel do
   DO K=1,NODE_Z
   DO J=1,NODE_Y
       fi(1,J,K)=fi(2,J,K)
       fi(0,J,K)=fi(2,J,K)
       fi(-1,J,K)=fi(2,J,K)
       fi(-2,J,K)=fi(2,J,K)
       fi(NODE_X,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+1,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+2,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+3,J,K)=fi(NODE_X-1,J,K)
    ENDDO
    ENDDO
	!$omp end parallel do

    !$omp parallel do
    DO K=1,NODE_Z
    DO I=-2,NODE_X+3
       fi(I,0,K)=fi(I,1,K)
       fi(I,-1,K)=fi(I,1,K)
       fi(I,-2,K)=fi(I,1,K)
	   fi(I,NODE_Y,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+1,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+2,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+3,K)=fi(I,NODE_Y-1,K)     
    ENDDO
    ENDDO
	!$omp end parallel do

    !$omp parallel do
    DO J=-2,NODE_Y+3
    DO I=-2,NODE_X+3
	   fi(I,J,1)=fi(I,J,2)
       fi(I,J,0)=fi(I,J,2)
       fi(I,J,-1)=fi(I,J,2)
       fi(I,J,-2)=fi(I,J,2)
	   fi(I,J,NODE_Z)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+1)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+2)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+3)=fi(I,J,NODE_Z-1)     
    ENDDO
    ENDDO
	!$omp end parallel do


endif

end subroutine bcv

subroutine bcw(FI)
USE PRECISION
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: FI
 
if(VEL_BC==0) then 

  !$omp parallel do
   DO J=1,NODE_Y
   DO I=1,NODE_X
      fi(I,J,0)=0.0_DP
      fi(I,J,-1)=-fi(I,J,1)
      fi(I,J,-2)=-fi(I,J,2)
      fi(I,J,NODE_Z)=0.0_DP
      fi(I,J,NODE_Z+1)=-fi(I,J,NODE_Z-1)
      fi(I,J,NODE_Z+2)=-fi(I,J,NODE_Z-2)
      fi(I,J,NODE_Z+3)=-fi(I,J,NODE_Z-3)     
   ENDDO
   ENDDO
	!$omp end parallel do
 
  !$omp parallel do
   DO K=-2,NODE_Z+3
   DO J=1,NODE_Y
      fi(0,J,K)=-fi(1,J,K)
      fi(-1,J,K)=-fi(2,J,K)
      fi(-2,J,K)=-fi(3,J,K)
      fi(NODE_X+1,J,K)=-fi(NODE_X,J,K)
      fi(NODE_X+2,J,K)=-fi(NODE_X-1,J,K)
      fi(NODE_X+3,J,K)=-fi(NODE_X-2,J,K)     
    ENDDO
    ENDDO
	!$omp end parallel do

  !$omp parallel do
   DO K=-2,NODE_Z+3
   DO I=-2,NODE_X+3
      fi(I,0,K)=-fi(I,1,K)
      fi(I,-1,K)=-fi(I,2,K)
      fi(I,-2,K)=-fi(I,3,K)
      fi(I,NODE_Y+1,K)=-fi(I,NODE_Y,K)
      fi(I,NODE_Y+2,K)=-fi(I,NODE_Y-1,K)
      fi(I,NODE_Y+3,K)=-fi(I,NODE_Y-2,K)     
   ENDDO
   ENDDO
	!$omp end parallel do

else if(vel_bc==1)then

  !$omp parallel do
  DO J=1,NODE_Y
  DO I=1,NODE_X
      fi(I,J,0)=0.0_DP
      fi(I,J,-1)=-fi(I,J,1)
      fi(I,J,-2)=-fi(I,J,2)
      fi(I,J,NODE_Z)=0.0_DP
      fi(I,J,NODE_Z+1)=-fi(I,J,NODE_Z-1)
      fi(I,J,NODE_Z+2)=-fi(I,J,NODE_Z-2)
      fi(I,J,NODE_Z+3)=-fi(I,J,NODE_Z-3)     
  ENDDO
  ENDDO
	!$omp end parallel do

  !$omp parallel do
  DO K=-2,NODE_Z+3
  DO J=1,NODE_Y
    fi(0,J,K)=fi(1,J,K)
    fi(-1,J,K)=fi(2,J,K)
    fi(-2,J,K)=fi(3,J,K)
    fi(NODE_X+1,J,K)=fi(NODE_X,J,K)
    fi(NODE_X+2,J,K)=fi(NODE_X-1,J,K)
    fi(NODE_X+3,J,K)=fi(NODE_X-2,J,K)     
  ENDDO
  ENDDO
	!$omp end parallel do

  !$omp parallel do
  DO K=-2,NODE_Z+3
  DO I=-2,NODE_X+3
    fi(I,0,K)=fi(I,1,K)
    fi(I,-1,K)=fi(I,2,K)
    fi(I,-2,K)=fi(I,3,K)
    fi(I,NODE_Y+1,K)=fi(I,NODE_Y,K)
    fi(I,NODE_Y+2,K)=fi(I,NODE_Y-1,K)
    fi(I,NODE_Y+3,K)=fi(I,NODE_Y-2,K)     
  ENDDO
  ENDDO
	!$omp end parallel do
	
else if(vel_bc==2)then

   !$omp parallel do
   DO K=1,NODE_Z
   DO J=1,NODE_Y
       fi(1,J,K)=fi(2,J,K)
       fi(0,J,K)=fi(2,J,K)
       fi(-1,J,K)=fi(2,J,K)
       fi(-2,J,K)=fi(2,J,K)
       fi(NODE_X,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+1,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+2,J,K)=fi(NODE_X-1,J,K)
       fi(NODE_X+3,J,K)=fi(NODE_X-1,J,K)
    ENDDO
    ENDDO
	!$omp end parallel do

    !$omp parallel do
    DO K=1,NODE_Z
    DO I=-2,NODE_X+3
	   fi(I,1,K)=fi(I,2,K)
       fi(I,0,K)=fi(I,2,K)
       fi(I,-1,K)=fi(I,2,K)
       fi(I,-2,K)=fi(I,2,K)
	   fi(I,NODE_Y,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+1,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+2,K)=fi(I,NODE_Y-1,K)
       fi(I,NODE_Y+3,K)=fi(I,NODE_Y-1,K)     
    ENDDO
    ENDDO
	!$omp end parallel do

    !$omp parallel do
    DO J=-2,NODE_Y+3
    DO I=-2,NODE_X+3
       fi(I,J,0)=fi(I,J,1)
       fi(I,J,-1)=fi(I,J,1)
       fi(I,J,-2)=fi(I,J,1)
	   fi(I,J,NODE_Z)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+1)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+2)=fi(I,J,NODE_Z-1)
       fi(I,J,NODE_Z+3)=fi(I,J,NODE_Z-1)     
    ENDDO
    ENDDO
	!$omp end parallel do
	
endif

end subroutine bcw
