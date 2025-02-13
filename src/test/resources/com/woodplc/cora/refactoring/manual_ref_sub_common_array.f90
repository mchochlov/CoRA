!
!
!
!
      SUBROUTINE TRANSF(DISPT, DISP, TDICOS, nnode, ndeg, n1, DUMP)

      

      IMPLICIT NONE
      !*--TRANSF5551
      !
      !*** START OF DECLARATIONS REWRITTEN BY SPAG
      !
      ! COMMON VARIABLES
      !
      integer, intent(in) :: n1
      integer :: ndeg
      integer, intent(in) :: nnode
      REAL(8), DIMENSION(N1) :: DISP
      REAL(8), DIMENSION(N1) :: DISPT
      REAL(8), DIMENSION(40) :: DUMP
      REAL(8), DIMENSION(NDEG, NDEG) :: TDICOS
      
      INTENT (INOUT) DISPT
      !Local variables
      INTEGER :: II
      INTEGER :: J
      INTEGER :: K
      
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