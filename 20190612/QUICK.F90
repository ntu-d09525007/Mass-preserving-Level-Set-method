SUBROUTINE QUICK()
IMPLICIT NONE
 
  CALL VELA_Q
  CALL VELB_Q
  CALL VELC_Q

END SUBROUTINE

SUBROUTINE QUICK_CELL_FACE(FI)
USE PRECISION
USE PROBLEM_DEF
USE DUMMY_DATA, ONLY : FP,FM,GP,GM,HP,HM
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP), DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: FI

 !$OMP PARALLEL DO 
 DO K = 0, NODE_Z
 DO J = 0, NODE_Y
 DO I = 0, NODE_X
   	FP(I,J,K) = (-FI(I-1,J,K)+6.0*FI(I,J,K)+3.0*FI(I+1,J,K))/8.0
   	FM(I,J,K) = (-FI(I+2,J,K)+6.0*FI(I+1,J,K)+3.0*FI(I,J,K))/8.0
   	
   	GP(I,J,K) = (-FI(I,J-1,K)+6.0*FI(I,J,K)+3.0*FI(I,J+1,K))/8.0
   	GM(I,J,K) = (-FI(I,J+2,K)+6.0*FI(I,J+1,K)+3.0*FI(I,J,K))/8.0

   	HP(I,J,K) = (-FI(I,J,K-1)+6.0*FI(I,J,K)+3.0*FI(I,J,K+1))/8.0
   	HM(I,J,K) = (-FI(I,J,K+2)+6.0*FI(I,J,K+1)+3.0*FI(I,J,K))/8.0	
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO
 
END SUBROUTINE

SUBROUTINE QUICK_U()
USE PROBLEM_DEF
USE PRECISION
USE DUMMY_DATA, ONLY : F,G,H,FP,FM,GP,GM,HP,HM
USE FLUID_PROPERTIES, ONLY : U,V,W,U_OLD
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: UU,VV,WW

 CALL QUICK_CELL_FACE(U_OLD)
  
 !$OMP PARALLEL DO PRIVATE(UU,VV,WW)
 DO K = 0, NODE_Z
 DO J = 0, NODE_Y
 DO I = 0, NODE_X
 	 
 	 UU = 0.5*(U(I,J,K)+U(I+1,J,K))
 	 VV = 0.5*(V(I,J,K)+V(I+1,J,K))
 	 WW = 0.5*(W(I,J,K)+W(I+1,J,K))
 	 
 	 IF(UU>=0.0)THEN
 	 	  F(I,J,K) = UU*FP(I,J,K)
 	 ELSE
 	 	  F(I,J,K) = UU*FM(I,J,K)
 	 ENDIF

 	 IF(VV>=0.0)THEN
 	 	  G(I,J,K) = VV*GP(I,J,K)
 	 ELSE
 	 	  G(I,J,K) = VV*GM(I,J,K)
 	 ENDIF

 	 IF(WW>=0.0)THEN
 	 	  H(I,J,K) = WW*HP(I,J,K)
 	 ELSE
 	 	  H(I,J,K) = WW*HM(I,J,K)
 	 ENDIF
 	  	  	 
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO
  	
END SUBROUTINE

SUBROUTINE QUICK_V()
USE PROBLEM_DEF
USE PRECISION
USE DUMMY_DATA, ONLY : F,G,H,FP,FM,GP,GM,HP,HM
USE FLUID_PROPERTIES, ONLY : U,V,W,V_OLD
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: UU,VV,WW

 CALL QUICK_CELL_FACE(V_OLD)
  
 !$OMP PARALLEL DO PRIVATE(UU,VV,WW)
 DO K = 0, NODE_Z
 DO J = 0, NODE_Y
 DO I = 0, NODE_X
 	 
 	 UU = 0.5*(U(I,J,K)+U(I,J+1,K))
 	 VV = 0.5*(V(I,J,K)+V(I,J+1,K))
 	 WW = 0.5*(W(I,J,K)+W(I,J+1,K))
 	 
 	 IF(UU>=0.0)THEN
 	 	  F(I,J,K) = UU*FP(I,J,K)
 	 ELSE
 	 	  F(I,J,K) = UU*FM(I,J,K)
 	 ENDIF

 	 IF(VV>=0.0)THEN
 	 	  G(I,J,K) = VV*GP(I,J,K)
 	 ELSE
 	 	  G(I,J,K) = VV*GM(I,J,K)
 	 ENDIF

 	 IF(WW>=0.0)THEN
 	 	  H(I,J,K) = WW*HP(I,J,K)
 	 ELSE
 	 	  H(I,J,K) = WW*HM(I,J,K)
 	 ENDIF
 	  	  	 
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO
  	
END SUBROUTINE

SUBROUTINE QUICK_W()
USE PROBLEM_DEF
USE PRECISION
USE DUMMY_DATA, ONLY : F,G,H,FP,FM,GP,GM,HP,HM
USE FLUID_PROPERTIES, ONLY : U,V,W,W_OLD
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: UU,VV,WW

 CALL QUICK_CELL_FACE(W_OLD)
  
 !$OMP PARALLEL DO PRIVATE(UU,VV,WW)
 DO K = 0, NODE_Z
 DO J = 0, NODE_Y
 DO I = 0, NODE_X
 	 
 	 UU = 0.5*(U(I,J,K)+U(I,J,K+1))
 	 VV = 0.5*(V(I,J,K)+V(I,J,K+1))
 	 WW = 0.5*(W(I,J,K)+W(I,J,K+1))
 	 
 	 IF(UU>=0.0)THEN
 	 	  F(I,J,K) = UU*FP(I,J,K)
 	 ELSE
 	 	  F(I,J,K) = UU*FM(I,J,K)
 	 ENDIF

 	 IF(VV>=0.0)THEN
 	 	  G(I,J,K) = VV*GP(I,J,K)
 	 ELSE
 	 	  G(I,J,K) = VV*GM(I,J,K)
 	 ENDIF

 	 IF(WW>=0.0)THEN
 	 	  H(I,J,K) = WW*HP(I,J,K)
 	 ELSE
 	 	  H(I,J,K) = WW*HM(I,J,K)
 	 ENDIF
 	  	  	 
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO
  	
END SUBROUTINE

SUBROUTINE VELA_Q()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
use vof_data
USE DUMMY_DATA, ONLY : F,G,H
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: DX2,DY2,DZ2,RE2X,RE2Y,RE2Z
REAL(DP) :: CM,CR,CC,DETF,CURVK,FINX,A1,A2,A3,A4,A5,A6,A7,TMP
    
  DX2=DX*DX
  DY2=DY*DY
  DZ2=DZ*DZ
  RE2X=RE*DX2
  RE2Y=RE*DY2
  RE2Z=RE*DZ2
  
  CALL QUICK_U
  
 !$omp parallel do private(cm,cr,cc,detf,curvk,finx,a1,a2,a3,a4,a5,a6,a7,TMP)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X-1

    CM=(AMU_OLD(I,J,K)+AMU_OLD(I+1,J,K))
    CR=(RHO_OLD(I,J,K)+RHO_OLD(I+1,J,K))
    CC=CM/CR
    
    !detf = 0.5_DP*(DELTA(i,j,k)+DELTA(i+1,j,k))
    TMP = HEAV(0.5_DP*(PHI(I,J,K)+PHI(I+1,J,K)),DETF)
    curvk= 0.5_DP*(curv(i,j,k)+curv(i+1,j,k))
    finx = 0.5_DP*(NORMAL_X(i,j,k)+NORMAL_X(i+1,j,k))
    
    IF(INTERFACE_METHOD.eq.1)THEN
    	DETF=1.0
    	FINX=(VOF(I+1,J,K)-VOF(I,J,K))/DX
    ENDIF

    A1=1.0_DP/DX*(F(I,J,K)-F(I-1,J,K))
    
    A2=1.0_DP/DX*(G(I,J,K)-G(I,J-1,K))
    
    A3=1.0_DP/DX*(H(I,J,K)-H(I,J,K-1))
       
    A4=1.0_DP/RE2X*CC*(U_OLD(I+1,J,K)-2.0_DP*U_OLD(I,J,K)+U_OLD(I-1,J,K))
   
    A5=1.0_DP/RE2Y*CC*(U_OLD(I,J+1,K)-2.0_DP*U_OLD(I,J,K)+U_OLD(I,J-1,K))
   
    A6=1.0_DP/RE2Z*CC*(U_OLD(I,J,K+1)-2.0_DP*U_OLD(I,J,K)+U_OLD(I,J,K-1))
	
    if(ST_FORCE==0)THEN
    	A7=0.0_dp
    ELSE IF(ST_FORCE==1)THEN
    	A7=2.0_DP/(CR*WE)*curvk*detf*finx
    ELSE IF(ST_FORCE==2)THEN
    	A7=2.0_DP/(CR*WE)*curvk*detf*finx*2.0_DP*TMP
    ENDIF
    
    US(I,J,K)=A1+A2+A3-A4-A5-A6+A7

  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do

  END SUBROUTINE VELA_Q 
  
  
  
 SUBROUTINE VELB_Q()
 USE PRECISION
 USE PROBLEM_DEF
 USE FLUID_PROPERTIES
 USE LS_DATA
 use vof_data
 USE DUMMY_DATA, ONLY : F,G,H
 IMPLICIT NONE
 INTEGER :: I,J,K
 REAL(DP) :: DX2,DY2,DZ2,RE2X,RE2Y,RE2Z
 REAL(DP) :: CM,CC,CR,DETF,FINY,CURVK,B1,B2,B3,B4,B5,B6,B7,TMP
 
  DX2=DX*DX
  DY2=DY*DY
  DZ2=DZ*DZ
  RE2X=RE*DX2
  RE2Y=RE*DY2
  RE2Z=RE*DZ2
  
  CALL QUICK_V
 
 !$omp parallel do private(cm,cc,cr,detf,finy,curvk,b1,b2,b3,b4,b5,b6,b7,TMP)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y-1
 DO I = 1, NODE_X
    
    CM=(AMU_OLD(I,J,K)+AMU_OLD(I,J+1,K))
    CR=(RHO_OLD(I,J,K)+RHO_OLD(I,J+1,K))
    CC=CM/CR
    
    !detf = 0.5_DP*(DELTA(i,j,k)+DELTA(i,j+1,k))
    TMP = HEAV(0.5_DP*(PHI(I,J,K)+PHI(I,J+1,K)),DETF)
    curvk= 0.5_DP*(curv(i,j,k)+curv(i,j+1,k))
    finy = 0.5_DP*(NORMAL_Y(i,j,k)+NORMAL_Y(i,j+1,k))   

    IF(INTERFACE_METHOD.eq.1)THEN
    	DETF=1.0
    	FINY=(VOF(I,J+1,K)-VOF(I,J,K))/DX
    ENDIF
    
    B1=1.0_DP/DX*(F(I,J,K)-F(I-1,J,K))
    
    B2=1.0_DP/DY*(G(I,J,K)-G(I,J-1,K))
    
    B3=1.0_DP/DZ*(H(I,J,K)-H(I,J,K-1))
   
    B4=1.0_DP/RE2X*CC*(V_OLD(I+1,J,K)-2.0_DP*V_OLD(I,J,K)+V_OLD(I-1,J,K))
   
    B5=1.0_DP/RE2Y*CC*(V_OLD(I,J+1,K)-2.0_DP*V_OLD(I,J,K)+V_OLD(I,J-1,K))
   
    B6=1.0_DP/RE2Z*CC*(V_OLD(I,J,K+1)-2.0_DP*V_OLD(I,J,K)+V_OLD(I,J,K-1))
    
    if(ST_FORCE==0)THEN
    	B7=0.0_dp
    ELSE IF(ST_FORCE==1)THEN
    	B7=2.0/(CR*WE)*curvk*detf*finy
    ELSE IF(ST_FORCE==2)THEN
    	B7=2.0/(CR*WE)*curvk*detf*finy*2.0_DP*TMP
    ENDIF

    VS(I,J,K)=B1+B2+B3-B4-B5-B6+B7

  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do

  END SUBROUTINE VELB_Q 
                
SUBROUTINE VELC_Q()
 USE PRECISION
 USE PROBLEM_DEF
 USE FLUID_PROPERTIES
 USE LS_DATA
 use vof_data
 USE DUMMY_DATA, ONLY : F,G,H
 IMPLICIT NONE
 INTEGER :: I,J,K
 REAL(DP) :: DX2,DY2,DZ2,RE2X,RE2Y,RE2Z,FR2
 REAL(DP) :: UU,UP,UM,VV,VP,VM,WP,WM,CM,CC,CR,DETF,FINZ,CURVK,C1,C2,C3,C4,C5,C6,C7,TMP
 
  DX2=DX*DX
  DY2=DY*DY
  DZ2=DZ*DZ
  RE2X=RE*DX2
  RE2Y=RE*DY2
  RE2Z=RE*DZ2 

  FR2=FR**2
 
 CALL QUICK_W
 
 !$omp parallel do private(cm,cr,cc,detf,curvk,finz,c1,c2,c3,c4,c5,c6,c7,TMP)
 DO K = 1, NODE_Z-1
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
 	
    CM=(AMU_OLD(I,J,K)+AMU_OLD(I,J,K+1))
    CR=(RHO_OLD(I,J,K)+RHO_OLD(I,J,K+1))
    CC=CM/CR
    
    !detf=0.5_DP*(DELTA(i,j,k)+DELTA(i,j,k+1))
    TMP = HEAV(0.5_DP*(PHI(I,J,K)+PHI(I,J,K+1)),DETF)
    curvk=0.5_DP*(curv(i,j,k)+curv(i,j,k+1))
    finz=0.5_DP*(NORMAL_Z(i,j,k)+NORMAL_Z(i,j,k+1))
    
    IF(INTERFACE_METHOD.eq.1)THEN
    	DETF=1.0
    	FINZ=(VOF(I,J,K+1)-VOF(I,J,K))/DZ
    ENDIF
    
    C1=1.0_DP/DX*(F(I,J,K)-F(I-1,J,K))
    
    C2=1.0_DP/DY*(G(I,J,K)-G(I,J-1,K))
    
    C3=1.0_DP/DZ*(H(I,J,K)-H(I,J,K-1))
    
    C4=1.0_DP/RE2X*CC*(W_OLD(I+1,J,K)-2.0_DP*W_OLD(I,J,K)+W_OLD(I-1,J,K))
  
    C5=1.0_DP/RE2Y*CC*(W_OLD(I,J+1,K)-2.0_DP*W_OLD(I,J,K)+W_OLD(I,J-1,K))
  
    C6=1.0_DP/RE2Z*CC*(W_OLD(I,J,K+1)-2.0_DP*W_OLD(I,J,K)+W_OLD(I,J,K-1))

    if(ST_FORCE==0)THEN
    	C7=0.0_dp
    ELSE IF(ST_FORCE==1)THEN
    	C7=2.0_DP/(CR*WE)*curvk*detf*finz
    ELSE IF(ST_FORCE==2)THEN
    	C7=2.0_DP/(CR*WE)*curvk*detf*finz*2.0_DP*TMP
    ENDIF
    
    WS(I,J,K)=C1+C2+C3-C4-C5-C6+C7
    
    IF( G_FORCE==1 )WS(I,J,K)=WS(I,J,K)+1.0_DP/FR2
    
  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do

  END SUBROUTINE VELC_Q