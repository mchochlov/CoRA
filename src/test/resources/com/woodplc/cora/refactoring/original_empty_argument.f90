!
!
!
!
!
      SUBROUTINE OPEN_PARAMETER_LOG_FILE
      USE ANALYSIS_DATA, ONLY : PARA_ANALYSIS_NO, ILOG, PARA_REC_NO, FATIGUE_ANALYSIS, &
                                axial_capacity_analysis_flag
      USE DEPLOYMENT_DATA, ONLY : IDEPLOY_FLAG
      use vdu, only: iscrn
      use iopa, only: ilogsize

      IMPLICIT NONE

      COMMON/INOUT/IIN,IOUT,IADM,IALL
      COMMON /DIRNAM/ ANALYSIS_DIR,STUMP
      INTEGER :: IDUMMY(7), IADDR, I, J, JJ,  &
        LENGTH, OPEN_STATUS, IIN,IOUT,IADM,IALL
      INTEGER :: FILE_RECL !FUNCTION
      REAL(8), DIMENSION(1) :: G
      CHARACTER(256) :: ANALYSIS_DIR, STUMP, LOG_FILE_NAME, NAMFIL,      &
        LOG_FILE_DIR, FATIGUE_STUMP
      character(5) :: dummy_run_time
      character(10) :: dummy_date
      CHARACTER(90) :: MESSAGE, ERROR_MESSAGE

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