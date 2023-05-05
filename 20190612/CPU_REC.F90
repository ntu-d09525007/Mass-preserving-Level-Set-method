SUBROUTINE CPU_START_REC()
USE PROBLEM_DEF
IMPLICIT NONE

 CPU_COST=0.0
 TS = WTIME()

END SUBROUTINE

SUBROUTINE CPU_REC()
USE PRECISION
USE PROBLEM_DEF
IMPLICIT NONE

  IF(MOD(ITER,5000).EQ.0)THEN
    TE = WTIME()
	  CPU_COST = CPU_COST + TE - TS
    TS = WTIME()
  ENDIF
  
END SUBROUTINE

function wtime ( )
  implicit none
  integer ( kind = 4 ) clock_max
  integer ( kind = 4 ) clock_rate
  integer ( kind = 4 ) clock_reading
  real ( kind = 8 ) wtime
  call system_clock ( clock_reading, clock_rate, clock_max )
  wtime = real ( clock_reading, kind = 8 ) &
        / real ( clock_rate, kind = 8 )
  return
end function