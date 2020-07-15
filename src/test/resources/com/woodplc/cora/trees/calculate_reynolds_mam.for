C
C -------------------------------------------------------------------- C
C
      PURE SUBROUTINE CALCOF(CAN, CMT, CANN, CMTT, RENO, NORE, VRELP, 
     & DRAGD, MAXRE, VISKIN)
C                    
      IMPLICIT NONE
C
C COMMON variables
C
      REAL(8) :: CAN
      REAL(8) :: CMT
      REAL(8) :: DRAGD
      INTEGER, intent(in) :: MAXRE
      REAL(8), intent(in) :: VISKIN
      REAL(8) :: VRELP
      REAL(8) , DIMENSION(MAXRE) :: CANN
      REAL(8) , DIMENSION(MAXRE) :: CMTT
      INTEGER , DIMENSION(MAXRE) :: NORE
      REAL(8) , DIMENSION(MAXRE) :: RENO
      
      INTENT (IN) CANN , CMTT , DRAGD , NORE , RENO , VRELP
      INTENT (OUT) CAN , CMT
C Local variables
      INTEGER :: I
      REAL(8) :: REYFAC
      REAL(8) :: REYN
      
C
C IF Cm CONSTANT ....
C
      IF(NORE(1).EQ.1)THEN
       CAN = CANN(1)
       CMT = CMTT(1)
      ELSE
C
C ... ELSE IF Ca = f(Re) => CALCULATE Re = f(VRELP) ...
C
       REYN = VRELP * DRAGD / VISKIN
C
C ... & CALCULATE COF. = f(Re)
C
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
C
      ENDIF
C
      RETURN
C
      END