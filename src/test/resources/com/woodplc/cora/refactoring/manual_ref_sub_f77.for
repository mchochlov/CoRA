C

C
C
      PURE SUBROUTINE PARTS(COMP, XMOD, ANGL, SMALLNUM, PI)
                       
      IMPLICIT NONE
C
C Dummy arguments
C
      REAL(8) :: ANGL
      real(8), intent(in) :: PI
      real(8), intent(in) :: SMALLNUM
      REAL(8) :: XMOD
      REAL(8) , DIMENSION(2) :: COMP
      
      INTENT (IN) COMP
      INTENT (OUT) ANGL , XMOD
C Local variables
      REAL(8) :: DABS
      REAL(8) :: DATAN2
      REAL(8) :: DSQRT
      REAL(8) :: R1
      REAL(8) :: R2
      
C
      R1 = COMP(1)
      IF(DABS(R1) <= SMALLNUM)R1 = 0.D0
      R2 = COMP(2)
      IF(DABS(R2) <= SMALLNUM)R2 = 0.D0
C
      XMOD  = DSQRT(R1*R1 + R2*R2)
C
      IF( DABS(R1) > SMALLNUM .AND. DABS(R2) > SMALLNUM )THEN
        ANGL = DATAN2(R2,R1)
      ELSE IF( DABS(R1) > SMALLNUM .AND. DABS(R2) < SMALLNUM )THEN
        IF( R1 > 0 )THEN
          ANGL = 0.D0
        ELSE
          ANGL = PI
        END IF
      ELSE IF( DABS(R1) < SMALLNUM .AND. DABS(R2) > SMALLNUM )THEN
        IF( R2 > 0 )THEN
          ANGL = PI * 0.5
        ELSE
          ANGL = PI * 1.5
        END IF
      ELSE
        ANGL = 0.D0
      END IF
C
      RETURN
C
      END