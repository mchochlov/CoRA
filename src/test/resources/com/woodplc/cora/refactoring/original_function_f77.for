C
C
      FUNCTION ANGLE(ANGCOS,ANGSIN)
C
      USE PARAMETERS, ONLY : PI                      
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL(8) :: ANGCOS , ANGSIN
      REAL(8) :: ANGLE
      INTENT (INOUT) ANGCOS , ANGSIN
C
C Local variables
C
      REAL(8) :: DASIN
C
      IF(ANGSIN.GT.1.D0)ANGSIN  =   1.D0
      IF(ANGSIN.LT.-1.D0)ANGSIN = - 1.D0
      IF(ANGCOS.GT.1.D0)ANGCOS  =   1.D0
      IF(ANGCOS.LT.-1.D0)ANGCOS = - 1.D0
C
      IF(ANGCOS.GE.0.D0)ANGLE = DASIN(ANGSIN)
      IF(ANGCOS.LT.0.D0.AND.ANGSIN.GE.0.D0)
     1ANGLE =   PI - DASIN(ANGSIN)
      IF(ANGCOS.LT.0.D0.AND.ANGSIN.LT.0.D0)
     1ANGLE = - PI - DASIN(ANGSIN)
C
      RETURN
C
      END