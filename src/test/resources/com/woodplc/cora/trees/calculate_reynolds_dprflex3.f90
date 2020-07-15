!
! -------------------------------------------------------------------- 
!
      SUBROUTINE CALCOF(CAN, CMT, CANN, CMTT, RENO, NORE, VRELP, DRAGD, MAXRE,  &
	& VISKIN)

      IMPLICIT NONE
      !*--CALCOF3460
      !
      !*** START OF DECLARATIONS REWRITTEN BY SPAG
      !
      ! COMMON VARIABLES
      !
      REAL(8) :: CAN
      REAL(8) :: CMT
      REAL(8) :: DRAGD
      INTEGER, intent(in) :: MAXRE
      REAL(8), intent(in) :: VISKIN
      REAL(8) :: VRELP
      REAL(8), DIMENSION(MAXRE) :: CANN
      REAL(8), DIMENSION(MAXRE) :: CMTT
      INTEGER, DIMENSION(MAXRE) :: NORE
      REAL(8), DIMENSION(MAXRE) :: RENO
      
      INTENT (IN) CANN, CMTT, DRAGD, NORE, RENO, VRELP
      INTENT (OUT) CAN, CMT
      !Local variables
      INTEGER :: I
      REAL(8) :: REYFAC
      REAL(8) :: REYN
      
      !
      !*** END OF DECLARATIONS REWRITTEN BY SPAG
! IF Cm CONSTANT ....
!
      IF(NORE(1).EQ.1)THEN
       CAN = CANN(1)
       CMT = CMTT(1)
      ELSE
!
! ... ELSE IF Ca = f(Re) => CALCULATE Re = f(VRELP) ...
!
       REYN = VRELP * DRAGD / VISKIN
!
! ... & CALCULATE COF. = f(Re)
!
       IF(REYN.LE.RENO(1))THEN
        CAN = CANN(1)
        CMT = CMTT(1)
       ELSEIF(REYN.GE.RENO(NORE(1)))THEN
        CAN = CANN(NORE(1))
        CMT = CMTT(NORE(1))
       ELSE
        DO 50 I=2,NORE(1)
         IF(REYN.LT.RENO(I))THEN
          REYFAC = (REYN-RENO(I-1)) / (RENO(I)-RENO(I-1))
          CAN = CANN(I-1) + (CANN(I)-CANN(I-1)) * REYFAC
          CMT = CMTT(I-1) + (CMTT(I)-CMTT(I-1)) * REYFAC
          GOTO 55
         ENDIF
50      CONTINUE
55      CONTINUE
       ENDIF

      ENDIF

      RETURN

      END