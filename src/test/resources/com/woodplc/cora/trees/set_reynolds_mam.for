C
C
C
C
C
      SUBROUTINE SET_REYNOLDS(OMEGA, VRELCON, VRELC, VRELP, NDIMWV, 
     & ACTIVE_ELEM, PI, TIME, TRAMP, TSTART, INT, ISTATC, NODYN, 
     & NPERIOD, NUMEL, NWVPASS)
C
C ----------------------------------------------------------------------
C Routine to set control variables for Cd = f(Re)
C ----------------------------------------------------------------------
C
      
      
                            
      IMPLICIT NONE
C
C COMMON variables
C
      INTEGER, intent(in) :: INT
      INTEGER, intent(in) :: ISTATC
      integer, intent(in) :: NDIMWV
      INTEGER, intent(in) :: NODYN
      INTEGER, intent(inout) :: NPERIOD
      INTEGER, intent(in) :: NUMEL
      INTEGER, intent(out) :: NWVPASS
      real(8), intent(in) :: PI
      REAL(8), intent(in) :: TIME
      REAL(8), intent(in) :: TRAMP
      REAL(8), intent(in) :: TSTART
      integer, intent(in), dimension(numel,2) :: ACTIVE_ELEM
      REAL(8) , DIMENSION(NDIMWV) :: OMEGA
      REAL(8) , DIMENSION(INT,NUMEL) :: VRELC
      REAL(8) , DIMENSION(5) :: VRELCON
      REAL(8) , DIMENSION(INT,NUMEL) :: VRELP
      
      INTENT (IN) OMEGA , VRELC , VRELP
      INTENT (INOUT) VRELCON
C Local variables
      REAL(8) :: DABS
      REAL :: FLOAT
      INTEGER :: I
      INTEGER :: J
      INTEGER :: K
      REAL(8) :: WAVPERD
      
C
      IF (NODYN.EQ.1) THEN                       ! Static analysis
        NWVPASS = 0
      ELSE IF (NODYN.EQ.0.AND.ISTATC.EQ.1) THEN  ! Quasi-static analysis
        NWVPASS = 0
      ELSE                                       ! Dynamic analysis
        WAVPERD = (2.0D0 * PI) / OMEGA(1)
C
C...... Check for new wave period
C
        IF (TIME > (TSTART + TRAMP + (NPERIOD * WAVPERD))) THEN
          NWVPASS = 0
          NPERIOD = NPERIOD + 1
          DO I = 1,4                               ! Swap convergence
            VRELCON(6-I) = VRELCON(5-I)            ! check from previous
          END DO                                   ! 4 wace periods
          VRELCON(1) = 0.0D0
          DO J = 1,NUMEL                           ! Calculate current
C                                                  ! convergence check
C.......... Check is this an active element
C
            IF(ACTIVE_ELEM(J,2) /= 1)CYCLE

            DO K=1,INT                              
              IF (DABS(VRELC(K,J)) > 1.D-20) THEN
                VRELCON(1) = VRELCON(1) +
     &                     DABS((VRELC(K,J) - VRELP(K,J)) / VRELC(K,J))
              END IF
            END DO
          END DO
          VRELCON(1) = VRELCON(1) / FLOAT(NUMEL * INT)
          VRELCON(1) = VRELCON(1) * 100.0           ! In percent
        END IF
      END IF
C
      RETURN
C
      END SUBROUTINE SET_REYNOLDS