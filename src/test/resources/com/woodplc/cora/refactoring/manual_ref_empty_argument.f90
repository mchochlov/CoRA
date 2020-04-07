!
!
!
!
!
      SUBROUTINE OPEN_PARAMETER_LOG_FILE(PARA_ANALYSIS_NO, ILOG, PARA_REC_NO, &
	& FATIGUE_ANALYSIS, axial_capacity_analysis_flag, IDEPLOY_FLAG, iscrn, IIN, IOUT, &
	& ANALYSIS_DIR, STUMP)
      
      
      
      ! Error: Module not found iopa
      use iopa, only: ilogsize

      IMPLICIT NONE

      
      
      CHARACTER(256), intent(in) :: ANALYSIS_DIR
      logical, intent(inout) :: FATIGUE_ANALYSIS
      integer, intent(inout) :: IDEPLOY_FLAG
      INTEGER, intent(in) :: IIN
      integer, intent(in) :: ILOG
      INTEGER, intent(in) :: IOUT
      integer, intent(inout) :: PARA_ANALYSIS_NO
      integer, intent(out) :: PARA_REC_NO
      CHARACTER(256), intent(in) :: STUMP
      integer, intent(in) :: axial_capacity_analysis_flag
      integer, intent(inout) :: iscrn
      
      !Local variables
      INTEGER :: IDUMMY(7)
      INTEGER :: IADDR
      INTEGER :: I
      INTEGER :: J
      INTEGER :: JJ
      INTEGER :: LENGTH
      INTEGER :: OPEN_STATUS
      INTEGER :: FILE_RECL
      REAL(8), DIMENSION(1) :: G
      CHARACTER(256) :: LOG_FILE_NAME
      CHARACTER(256) :: NAMFIL
      CHARACTER(256) :: LOG_FILE_DIR
      CHARACTER(256) :: FATIGUE_STUMP
      character(5) :: dummy_run_time
      character(10) :: dummy_date
      CHARACTER(90) :: MESSAGE
      CHARACTER(90) :: ERROR_MESSAGE
      

      IADDR = 18
      READ(IIN,REC=IADDR) dummy_run_time, dummy_date, idummy(1), idummy(2), idummy(3), axial_capacity_analysis_flag

      IADDR = 19
      READ(IIN,REC=IADDR) (IDUMMY(I), I=1,7)
      PARA_ANALYSIS_NO = IDUMMY(5)
      IDEPLOY_FLAG = IDUMMY(2)
      FATIGUE_ANALYSIS = IDUMMY(7)

      IF(PARA_ANALYSIS_NO /= 0) THEN

        ! OPEN THE TEMPORARY LOG FILE.

        !If this is a fatigue analysis then skip this part as everything is located in
        ! the main analysis directory
        IF( .NOT.FATIGUE_ANALYSIS )THEN
          LENGTH=LEN(ADJUSTL(TRIM(ANALYSIS_DIR)))
          IF(LENGTH > 1) THEN
            DO J = LENGTH-1, 1, -1
              IF(ANALYSIS_DIR(J:J) == "\") THEN
                JJ = J
                LENGTH = J
                EXIT
              END IF
            END DO
            ! If this is a deployment analysis then we need to step out two folders instead of one
            ! (Stage folder and Loadcase folder) so the process is repeated again
            IF( IDEPLOY_FLAG == 1 .or. axial_capacity_analysis_flag == 1)THEN
              DO J = LENGTH-1, 1, -1
                IF(ANALYSIS_DIR(J:J) == "\") THEN
                  JJ = J
                  EXIT
                END IF
              END DO
            END IF
          ENDIF
          LOG_FILE_DIR = ANALYSIS_DIR(1:JJ)
          WRITE(LOG_FILE_NAME, FMT="(A,A,I0,A)") "FLEX_", TRIM(STUMP),   &
            PARA_ANALYSIS_NO,".LOG"
        ELSE
          LOG_FILE_DIR = ANALYSIS_DIR
          FATIGUE_STUMP = STUMP(1:11)
          WRITE(LOG_FILE_NAME, FMT="(A,A,I0,A)") "FLEX_", TRIM(FATIGUE_STUMP),   &
            PARA_ANALYSIS_NO, ".LOG"
        END IF

        NAMFIL = TRIM(LOG_FILE_DIR) // TRIM(LOG_FILE_NAME)
        OPEN(UNIT=ILOG,STATUS='OLD',FILE=NAMFIL, share='DENYNONE',       &
          ACCESS='DIRECT', SHARED, RECL=FILE_RECL(ilogsize), IOSTAT=OPEN_STATUS)
        IF(OPEN_STATUS == 0) THEN
!         read the start record from header
          READ(UNIT=ILOG, REC=1) MESSAGE
          CALL FIND_REALS_IN_LINE(" ",MESSAGE,G,1,0.D0)
          PARA_REC_NO = NINT(G(1))
        ELSE
          WRITE(ERROR_MESSAGE, FMT="(2A)")                        &
            "ERROR: UNABLE TO OPEN TEMPORARY LOG FILE " ,         &
            " FOR TIME-DOMAIN ANALYSIS."
          CALL ERROR_HANDLING(ISCRN, IOUT, 254, ERROR_MESSAGE, .FALSE.)
        END IF

        ISCRN = ILOG

      END IF

      RETURN

      END SUBROUTINE OPEN_PARAMETER_LOG_FILE