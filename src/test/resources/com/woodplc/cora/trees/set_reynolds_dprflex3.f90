




      SUBROUTINE SET_REYNOLDS(OMEGA, VRELCON, VRELC, VRELP, NDIMWV, PI, numel,  &
	& TIME, TRAMP, TSTART, INT, ISTATC, NODYN, NPERIOD, NWVPASS)
!
! ----------------------------------------------------------------------
! Routine to set control variables for Cd = f(Re)
! ----------------------------------------------------------------------
      
      
      

      IMPLICIT NONE
      !*--SET_REYNOLDS1858
      !
      !*** START OF DECLARATIONS REWRITTEN BY SPAG
      !
      ! COMMON VARIABLES
      !
      INTEGER, intent(in) :: INT
      INTEGER, intent(in) :: ISTATC
      integer, intent(in) :: NDIMWV
      INTEGER, intent(in) :: NODYN
      INTEGER, intent(inout) :: NPERIOD
      INTEGER, intent(out) :: NWVPASS
      real(8), intent(in) :: PI
      REAL(8), intent(in) :: TIME
      REAL(8), intent(in) :: TRAMP
      REAL(8), intent(in) :: TSTART
      integer, intent(in) :: numel
      REAL(8), DIMENSION(NDIMWV) :: OMEGA
      REAL(8), DIMENSION(INT, NUMEL) :: VRELC
      REAL(8), DIMENSION(5) :: VRELCON
      REAL(8), DIMENSION(INT, NUMEL) :: VRELP
      
      INTENT (IN) OMEGA, VRELC, VRELP
      INTENT (INOUT) VRELCON
      !Local variables
      INTEGER :: I
      INTEGER :: J
      INTEGER :: K
      REAL(8) :: WAVPERD
      
      !
      !*** END OF DECLARATIONS REWRITTEN BY SPAG
      !

      IF (NODYN.EQ.1) THEN                       ! Static analysis
        NWVPASS = 0
      ELSE IF (NODYN.EQ.0.AND.ISTATC.EQ.1) THEN  ! Quasi-static analysis
        NWVPASS = 0
      ELSE
        ! Continue only if atleast one harmonic is present.
        IF(NDIMWV > 0) THEN
          ! Dynamic analysis
          WAVPERD = 0.0D0
          IF( OMEGA(1) > 1.0D-8) WAVPERD = (2.0D0 * PI) / OMEGA(1)

!...... Check for new wave period
!
          IF (TIME > (TSTART + TRAMP + (NPERIOD * WAVPERD))) THEN
            NWVPASS = 0
            NPERIOD = NPERIOD + 1
            DO I = 1,4                              ! Swap convergence
              VRELCON(6-I) = VRELCON(5-I)           ! check from previous
            END DO                                  ! 4 wace periods
            VRELCON(1) = 0.0D0
            DO J = 1,NUMEL                          ! Calculate current
              DO K=1,INT                            ! convergence check
                IF (DABS(VRELC(K,J)) > 1.D-20) THEN
                  VRELCON(1) = VRELCON(1) +                                   &
                             DABS((VRELC(K,J) - VRELP(K,J)) / VRELC(K,J))
                END IF
              END DO
            END DO
            VRELCON(1) = VRELCON(1) / FLOAT(NUMEL * INT)
            VRELCON(1) = VRELCON(1) * 100.0           ! In percent
          END IF
        ELSE
          NWVPASS = 0
        END IF
      END IF

      RETURN

      END SUBROUTINE SET_REYNOLDS