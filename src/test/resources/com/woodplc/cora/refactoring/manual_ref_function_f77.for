C
C
      FUNCTION ANGLE(ANGCOS,ANGSIN, PI)
C
                            
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL(8) :: ANGCOS
      REAL(8) :: ANGSIN
      real(8) :: PI
      
      !Local variables
      REAL(8) :: ANGLE
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