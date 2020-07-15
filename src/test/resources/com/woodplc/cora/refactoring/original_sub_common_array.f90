!
!
!
!
      SUBROUTINE TRANSF(DISPT,DISP,TDICOS)

      use hddim, only : nnode, ndeg, n1

      IMPLICIT NONE
      !*--TRANSF5551
      !
      !*** START OF DECLARATIONS REWRITTEN BY SPAG
      !
      ! COMMON VARIABLES
      !
      REAL(8), DIMENSION(40) :: DUMP, FC, FW
      INTEGER :: ICROUT, INT, ITYPE
      INTEGER, DIMENSION(40) :: LM
      COMMON /MISC  / FC, FW, DUMP, LM, ITYPE, INT, ICROUT
      !
      ! DUMMY ARGUMENTS
      !
      REAL(8), DIMENSION(N1) :: DISP, DISPT
      REAL(8), DIMENSION(NDEG, NDEG) :: TDICOS
      INTENT (INOUT) DISPT
      !
      ! LOCAL VARIABLES
      !
      INTEGER :: II, J, K
      !
      !*** END OF DECLARATIONS REWRITTEN BY SPAG
! TRANSFORMS NODAL VECTOR FROM LOCAL TO GLOBAL COORDINATES
!
      DO 30 K = 1,NNODE
        DO 10 J = 1,NDEG
          II        = (K-1)*NDEG + J
          DISP(J)   = DISPT(II)
10      CONTINUE

        CALL GMPRD(TDICOS,DISP,DUMP,NDEG,NDEG,1)

        DO 20 J = 1,NDEG
          II        = (K-1)*NDEG + J
          DISPT(II) = DUMP(J)
20      CONTINUE

30    CONTINUE

      RETURN

      END