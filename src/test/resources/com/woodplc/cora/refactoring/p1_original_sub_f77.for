C
C
C
C
C
      SUBROUTINE CALCREGWAV(NRANDSEA,AUXSIN,CORD,DD0,NPBC,RAO,
     1                      VECCG0,OFFSET,NFRQHD,NATCH_RWE,IDATN_RWE,
     2                      WVNUM,OMEGA,WVAMP,WVRN,DIR,VOFFINI,NBAUX,
     3                      ACORD,TDST,SINDATA,NPNUM,RTCT0,DEPTH,GR,
     4                      IRANDSEA,NUM_SEAS,NHARZ,NHRZND,NDIMAUXSIN,
     5                      NAUXTOT,NSINTOT,NODEA,ISCRN)
C
      USE PARAMETERS, ONLY : PI
      USE WAVE_DATA, ONLY : NDIMWV, DIRDOM, FPEAK, NDIR, NHH,                       
     &  ISEA, AT, NUM_DIV
      USE WAVE_SPECTRUM_DATA, ONLY : SPARR                       
      IMPLICIT NONE
C
C COMMON variables
C
      REAL(8) :: COEFF_HT , COEFF_PER
      INTEGER :: IADM , IALL , IIN , IOUT , IRAND , IRND2REG , IRSTRT , 
     &           MAXRE , MXADM , MXAELM , MXANOD , MXART , MXAUX , 
     &           MXBC , MXCAB , MXCUR , MXDMPER , MXDRG , MXEL , 
     &           MXFJPTS , MXFREQ , MXHEAD , MXINT , MXLOAD , MXLSUB , 
     &           MXNLFJT , MXNLT , MXNOD , MXNODC , MXPAIR , MXRAOS , 
     &           MXREL , MXSPR , MXSPTS , MXUDL , MX_PANELS , N1 , N2 , 
     &           NAELEM , NANODE , NATND , NAUX , NBCAUX , NBCCA , 
     &           NBSIN , NCORD , NDEG , NDEG1 , NFREQ , NGENS , NH , 
     &           NNODE , NNODE2 , NONP , NTBC , NUMEL , NVSET , NWVHD
      COMMON /AUXDIM/ MXAUX , MXANOD , MXAELM , NAUX , NANODE , NAELEM , 
     &                NBCAUX , NBCCA
      COMMON /HDDIM / NUMEL , NONP , NNODE , NDEG , NGENS , NCORD , N1 , 
     &                NNODE2 , NTBC , NH , N2
      COMMON /INOUT / IIN , IOUT , IADM , IALL
      COMMON /MAXES / MXINT , MXREL , MXNOD , MXEL , MXADM , MXART , 
     &                MXSPR , MXCUR , MXFREQ , MXHEAD , MXBC , MXLOAD , 
     &                MXUDL , MXLSUB , MXPAIR , MXCAB , MXNODC , 
     &                MXSPTS , MXNLT , MXDRG , MXRAOS , MAXRE , 
     &                MXDMPER , MXNLFJT , MXFJPTS , MX_PANELS
      COMMON /RND2REG/ IRND2REG
      COMMON /RND2REG2/ COEFF_PER , COEFF_HT
      COMMON /SINUSOID/ NBSIN
      COMMON /START / IRSTRT , IRAND , NFREQ , NDEG1 , NATND , NWVHD , 
     &                NVSET
C
C Dummy arguments
C
      REAL(8) :: DEPTH , GR
      INTEGER :: IRANDSEA , ISCRN , NAUXTOT , NDIMAUXSIN , NRANDSEA , 
     &           NSINTOT , NUM_SEAS
      REAL(8) , DIMENSION(NANODE,3) :: ACORD
      REAL(8) , DIMENSION(NANODE*NDEG*NVSET*NRANDSEA,5) :: AUXSIN
      REAL(8) , DIMENSION(NCORD,NONP) :: CORD
      REAL(8) , DIMENSION(NDEG,NONP) :: DD0 , TDST
      REAL(8) , DIMENSION(NDIMWV) :: DIR , OMEGA , WVAMP , WVNUM , 
     &            WVRN
      INTEGER , DIMENSION(NATND,NVSET) :: IDATN_RWE
      INTEGER , DIMENSION(NVSET) :: NATCH_RWE
      INTEGER , DIMENSION(NANODE) :: NBAUX , NODEA
      INTEGER , DIMENSION(2,NVSET) :: NFRQHD
      INTEGER , DIMENSION(N2) :: NHARZ
      INTEGER , DIMENSION(NONP) :: NHRZND , NPNUM
      INTEGER , DIMENSION(NDEG,NONP) :: NPBC
      REAL(8) , DIMENSION(NDEG,MXRAOS) :: OFFSET
      REAL(8) , DIMENSION(NFREQ,NDEG1,NWVHD,MXRAOS) :: RAO
      REAL(8) , DIMENSION(3,NONP) :: RTCT0
      REAL(8) , DIMENSION(NBSIN,5) :: SINDATA
      REAL(8) , DIMENSION(NCORD,NVSET) :: VECCG0
      REAL(8) , DIMENSION(MXRAOS,NDEG) :: VOFFINI
      INTENT (IN) CORD , ISCRN , NODEA , NPNUM , NRANDSEA , NUM_SEAS , 
     &            RTCT0
      INTENT (OUT) AUXSIN , NDIMAUXSIN , NHARZ , NHRZND , SINDATA , TDST
      INTENT (INOUT) NAUXTOT , NPBC , NSINTOT
C
C Local variables
C
      INTEGER :: ALLOCATE_ERROR , I , IANODE , IDIR , IDOF , IHH , 
     &           INONP , ISIN , IVSET , LOCS , NACT , NAUXSIN ,  
     &           NEAR_IHH , NODE , NTOT , NUM
      REAL(8) :: ANGL , DIFF , DIFF0 , DIR1 , DTH , EQ_AMP , 
     &                EQ_DIR , EQ_PHA , FACANG , FMAX , HEIGHT_FACTOR , 
     &                OMEGA1 , PEAK_PER , SMAX , THET , THETD , 
     &                WVAMP1 , WVNUM1 , WVRN1 , WVRN_NR , XMOD
      REAL(8) , DIMENSION(2) :: COMP
      REAL(8) :: DATAN , DSQRT
      REAL(8) , DIMENSION(2,NONP,NDEG) :: DISPL
      REAL(8) , DIMENSION(2,NANODE,NDEG) :: DISPLA
      CHARACTER(400) :: ERROR_MESSAGE
      REAL(8) , DIMENSION(NCORD) :: R
      REAL(8) , DIMENSION(NONP,NDEG) :: RESPANG , RESPMAG , SPECMOM
      REAL(8) , DIMENSION(NANODE,NDEG) :: RESPANGA , RESPMAGA , 
     &            SPECMOMA
      REAL(8) , DIMENSION(2,NDEG) :: RSPONG , RSPONS
      REAL(8) , ALLOCATABLE , DIMENSION(:,:,:) :: SPECANG , SPECANGA , 
     &       SPECMAG , SPECMAGA
C
C Factor to change angles in radians to degrees
C
      FACANG = 45.D0 / DATAN(1.D0)
C
C ECHO EQUIVALENT REGULAR WAVE PARAMETERS TO THE OUTPUT FILE
C
      WRITE(IOUT,260)
C
      NACT = 0

        NTOT = 0
C
C INITIALISE NUMBER OF SINUSOIDAL BCs
C
      ISIN = NSINTOT
      NAUXSIN = NAUXTOT
C
      IF( ISEA(IRANDSEA) .EQ. 1 .OR. ISEA(IRANDSEA) .EQ. 5 .OR.
     &  ISEA(IRANDSEA) .EQ. 7 )RETURN

      DO I = 1, 1 ! LOOP FOR CATCHING ERRORS. 

      ALLOCATE( SPECMAG(NHH(IRANDSEA),NONP,NDEG), STAT=ALLOCATE_ERROR )
      IF( ALLOCATE_ERROR /= 0 )EXIT

      ALLOCATE( SPECMAGA(NHH(IRANDSEA),NANODE,NDEG),
     & STAT=ALLOCATE_ERROR )
      IF( ALLOCATE_ERROR /= 0 )EXIT

      ALLOCATE( SPECANG(NHH(IRANDSEA),NONP,NDEG), STAT=ALLOCATE_ERROR)
      IF( ALLOCATE_ERROR /= 0 )EXIT

      ALLOCATE( SPECANGA(NHH(IRANDSEA),NANODE,NDEG), 
     & STAT=ALLOCATE_ERROR)
      IF( ALLOCATE_ERROR /= 0 )EXIT

      END DO

      IF( ALLOCATE_ERROR /= 0 )THEN
        WRITE(ERROR_MESSAGE, FMT="(2A)") 
     &   "Error: Unable to allocate sufficient memory for " ,  
     &   " data arrays. Analysis terminating."
        CALL ERROR_HANDLING(ISCRN, IOUT, 253, ERROR_MESSAGE)
      END IF
C
      IF(NRANDSEA.GT.1)WRITE(IOUT,280)IRANDSEA
C
      DTH    = PI  / DFLOAT(NDIR(IRANDSEA))
C
      IF( ISEA(IRANDSEA) == 2 .OR. ISEA(IRANDSEA) == 3
     1   .OR. ISEA(IRANDSEA) == 6)THEN
        DO I = 1, NUM_DIV+1
          IF(I.EQ.1)THEN
            SMAX = SPARR(I,2,IRANDSEA)
            FMAX = SPARR(I,1,IRANDSEA)
          ELSE
            IF(SPARR(I,2,IRANDSEA).GT.SMAX)THEN
              SMAX = SPARR(I,2,IRANDSEA)
              FMAX = SPARR(I,1,IRANDSEA)
            END IF
          END IF
        END DO
        PEAK_PER = 1.D0 / FMAX
      ELSE IF( ISEA(IRANDSEA) == 4)THEN
        PEAK_PER = 1.D0 / FPEAK(IRANDSEA)
      END IF
C
C The equivalent regular wave period = coefficient (default 0.95) times the peak period
C
      PEAK_PER = PEAK_PER * COEFF_PER
C
C...... Store the equivalent regular wave direction as the dominant direction
C...... Equivalent phase is zero
C
      EQ_DIR = DIRDOM(IRANDSEA)
      EQ_PHA = 0.0

      DO IDIR = 1, NDIR(IRANDSEA)
        THET  = -(PI / 2.D0) + (DTH / 2.D0) + (DFLOAT(IDIR - 1) * DTH)
        THETD = THET * FACANG
        EQ_DIR = EQ_DIR + THETD
      END DO
C
C...... Store the harmonic number with the nearest period
C...... to the equivalent regular wave period
C
      DIFF0 = 1.0D25
      IF(IRANDSEA/=1)THEN
        NTOT= NTOT + NDIR(IRANDSEA-1)*NHH(IRANDSEA-1)
      ELSE
        NTOT = 0
      END IF
      DO IHH = 1, NHH(IRANDSEA)
        LOCS = NTOT + IHH+NDIR(IRANDSEA)-1
        DIFF = ABS(PEAK_PER-(2.0D0 * PI)/OMEGA(LOCS))

        IF(DIFF.LT.DIFF0)THEN
          DIFF0 = DIFF
          NEAR_IHH = IHH
        END IF
      END DO
C
C.... Equivalent Amplitude
C
      HEIGHT_FACTOR = 2.D0 * COEFF_HT

      EQ_AMP = HEIGHT_FACTOR * DSQRT(AT(IRANDSEA))
C
      WRITE(IOUT,240)
      WRITE(IOUT,250)EQ_AMP,PEAK_PER,EQ_DIR,EQ_PHA,COEFF_PER,COEFF_HT
C
      DO IHH = 1,NHH(IRANDSEA)               ! Loop over the harmonics
        NACT = NTOT + IHH+NDIR(IRANDSEA)-1
C
        WVNUM1 = WVNUM(NACT)
        OMEGA1 = OMEGA(NACT)
        WVAMP1 = WVAMP(NACT)
        WVRN1  = WVRN(NACT)
        DIR1 = DIR(NACT)
C
C CREATE A SPECTRUM OF RESPONSE FOR ATTACHED NODES IN ALL DOFS
C
        CALL SPECRESP(DD0,DISPL,NPBC,RAO,VECCG0,OFFSET,NFRQHD,
     1                RSPONS,RSPONG,R,NATCH_RWE,IDATN_RWE,WVNUM1,OMEGA1,
     2                WVAMP1,WVRN1,DIR1,VOFFINI)
C
C CREATE A SPECTRUM OF RESPONSE FOR AUXILIARY NODES IN TRANSLATIONAL DOFS
C
        CALL SPECRESPA(DISPLA,RAO,VECCG0,OFFSET,NFRQHD,RSPONS,
     1                 RSPONG,R,WVNUM1,OMEGA1,WVAMP1,WVRN1,DIR1,
     2                 VOFFINI,NBAUX,ACORD)
C
        DO IVSET = 1,NVSET                   ! Loop over the vessel sets
C
C ATTACHED NODES
C
          DO INONP = 1,NATCH_RWE(IVSET)      ! Loop over the attached nodes
            NODE = IDATN_RWE(INONP,IVSET)    ! Get the node number
            DO IDOF = 1,NDEG                 ! Loop over the DOFs
              IF(NPBC(IDOF,NODE).EQ.3)THEN   ! Check for vessel BC
C
C Convert the real and imaginary parts to magnitude and phase
C
                COMP(1) = DISPL(1,NODE,IDOF)
                COMP(2) = DISPL(2,NODE,IDOF)
                CALL PARTS(COMP,XMOD,ANGL)
C
C Transfer the values to the harmonic inclusive arrays
C
                SPECMAG(IHH,NODE,IDOF) = XMOD
                SPECANG(IHH,NODE,IDOF) = ANGL
C
              END IF
            END DO
          END DO
C
C AUXILIARY NODES
C
          DO IANODE = 1,NANODE               ! Loop over the auxiliary nodes
            IF(IVSET.EQ.NBAUX(IANODE))THEN   ! Check for vessel
              DO IDOF = 1,NDEG               ! Loop over the DOFs
C
C Convert the real and imaginary parts to magnitude and phase
C
                COMP(1) = DISPLA(1,IANODE,IDOF)
                COMP(2) = DISPLA(2,IANODE,IDOF)
                CALL PARTS(COMP,XMOD,ANGL)
C
C Transfer the values to the harmonic inclusive arrays
C
                SPECMAGA(IHH,IANODE,IDOF) = XMOD
                SPECANGA(IHH,IANODE,IDOF) = ANGL
C
              END DO
            END IF
          END DO
        END DO
      END DO
C
C ..... NOW FIND THE MOMENTS OF RESPONSE SPECTRUM
C
      DO IVSET = 1,NVSET                     ! Loop over the vessel sets
C
C ATTACHED NODES
C
        DO INONP = 1,NATCH_RWE(IVSET)     ! Loop over the attached nodes
          NODE = IDATN_RWE(INONP,IVSET)      ! Get the node number
          DO IDOF = 1,NDEG                   ! Loop over the DOFs
            IF(NPBC(IDOF,NODE).EQ.3)THEN     ! Check for vessel BC
              SPECMOM(NODE,IDOF) = 0.D0
              DO IHH = 1,NHH(IRANDSEA)       ! Loop over the harmonics
C
C Transfer the values to the harmonic inclusive arrays
C
                SPECMOM(NODE,IDOF) = SPECMOM(NODE,IDOF) +
     1                                 SPECMAG(IHH,NODE,IDOF)**2
              END DO
              SPECMOM(NODE,IDOF) = SPECMOM(NODE,IDOF)/2.0D0
            END IF
          END DO
        END DO
C
C AUXILIARY NODES
C
        DO IANODE = 1,NANODE             ! Loop over the auxiliary nodes
          IF(IVSET.EQ.NBAUX(IANODE))THEN     ! Check for vessel
            DO IDOF = 1,NDEG                 ! Loop over the DOFs
              SPECMOMA(IANODE,IDOF) = 0.D0
              DO IHH = 1,NHH(IRANDSEA)       ! Loop over the harmonics
C
C Transfer the values to the harmonic inclusive arrays
C
                SPECMOMA(IANODE,IDOF) = SPECMOMA(IANODE,IDOF) +
     1                                  SPECMAGA(IHH,IANODE,IDOF)**2
              END DO
              SPECMOMA(IANODE,IDOF) = SPECMOMA(IANODE,IDOF)/2.0D0
            END DO
          END IF
        END DO
      END DO
C
C ..... NOW FIND THE RESPONSE OF EACH ATTACHED NODE
C
C
      WVRN_NR = WVRN(NEAR_IHH+NTOT)
C
      DO IVSET = 1,NVSET                     ! Loop over the vessel sets
C
C ATTACHED NODES
C
        DO INONP = 1,NATCH_RWE(IVSET)     ! Loop over the attached nodes
          NODE = IDATN_RWE(INONP,IVSET)      ! Get the node number
          DO IDOF = 1,NDEG                   ! Loop over the DOFs
            IF(NPBC(IDOF,NODE).EQ.3)THEN     ! Check for vessel BC
              RESPMAG(NODE,IDOF) = HEIGHT_FACTOR *
     1                             DSQRT(SPECMOM(NODE,IDOF))
              RESPANG(NODE,IDOF) = SPECANG(NEAR_IHH,NODE,IDOF)
     1                             - WVRN_NR
C
C Convert angle to degrees temporarily
C
              RESPANG(NODE,IDOF) = RESPANG(NODE,IDOF) * FACANG
C
C Make sure angle is between 180 and -180
C
              CALL MTOP180(RESPANG(NODE,IDOF))
C
C Convert angle back to radians
C
              RESPANG(NODE,IDOF) = RESPANG(NODE,IDOF) / FACANG
            END IF
          END DO
        END DO
C
C AUXILIARY NODES
C
        DO IANODE = 1,NANODE             ! Loop over the auxiliary nodes
          IF(IVSET.EQ.NBAUX(IANODE))THEN     ! Check for vessel
            DO IDOF = 1,NDEG                 ! Loop over the DOFs
              RESPMAGA(IANODE,IDOF) = HEIGHT_FACTOR *
     1                                DSQRT(SPECMOMA(IANODE,IDOF))
              RESPANGA(IANODE,IDOF) = SPECANGA(NEAR_IHH,IANODE,IDOF)
     1                               - WVRN_NR
C
C Convert angle to degrees temporarily
C
              RESPANGA(IANODE,IDOF) = RESPANGA(IANODE,IDOF)*FACANG
C
C Make sure angle is between 180 and -180
C
              CALL MTOP180(RESPANGA(IANODE,IDOF))
C
C Convert angle back to radians
C
              RESPANGA(IANODE,IDOF) = RESPANGA(IANODE,IDOF)/FACANG
            END DO
          END IF
        END DO
      END DO
C
C ..... NOW SET THE SINUSOIDAL BOUNDARY CONDITIONS
C
      IF(NBSIN.GT.1)WRITE(IOUT,220)
      DO IVSET = 1,NVSET                     ! Loop over the vessel sets
C
C ATTACHED NODES
C
        DO INONP = 1,NATCH_RWE(IVSET)        ! Loop over the attached nodes
          NODE = IDATN_RWE(INONP,IVSET)      ! Get the node number
          DO IDOF = 1,NDEG                   ! Loop over the DOFs
            IF(NPBC(IDOF,NODE).EQ.3)THEN     ! Check for vessel BC
              IF(IDOF.LE.NCORD)TDST(IDOF,NODE) = DD0(IDOF,NODE) -
     1                                             CORD(IDOF,NODE)
              IF(IDOF.GT.NCORD)TDST(IDOF,NODE) =
     1                           RTCT0(IDOF-3,NODE) * 180.D0 / PI
              ISIN = ISIN + 1
              SINDATA(ISIN,1) = NODE
              SINDATA(ISIN,2) = IDOF
              SINDATA(ISIN,3) = RESPMAG(NODE,IDOF)
              SINDATA(ISIN,4) = PEAK_PER
              SINDATA(ISIN,5) = RESPANG(NODE,IDOF)
C
C Echo to the output file
C
              NUM = NPNUM(NODE)
              IF(IDOF.GT.3)THEN
                WRITE(IOUT,230)NUM,IDOF,RESPMAG(NODE,IDOF)*FACANG,
     1                         PEAK_PER,
     2                         RESPANG(NODE,IDOF)*FACANG
              ELSE
                WRITE(IOUT,230)NUM,IDOF,RESPMAG(NODE,IDOF),
     1                         PEAK_PER,
     2                         RESPANG(NODE,IDOF)*FACANG
              END IF
            END IF
          END DO
        END DO
C
C AUXILIARY NODES
C
        DO IANODE = 1,NANODE             ! Loop over the auxiliary nodes
          IF(IVSET.EQ.NBAUX(IANODE))THEN     ! Check for vessel
            DO IDOF = 1,NDEG                 ! Loop over the DOFs
C
              NAUXSIN = NAUXSIN + 1
              AUXSIN(NAUXSIN,1) = IANODE
              AUXSIN(NAUXSIN,2) = IDOF
              AUXSIN(NAUXSIN,3) = RESPMAGA(IANODE,IDOF)
              AUXSIN(NAUXSIN,4) = PEAK_PER
              AUXSIN(NAUXSIN,5) = RESPANGA(IANODE,IDOF)
C
C Echo to the output file
C
              NUM = NODEA(IANODE)
              IF(IDOF.GT.3)THEN
                WRITE(IOUT,230)NUM,IDOF,
     1                         RESPMAGA(IANODE,IDOF)*FACANG,
     2                         PEAK_PER,
     3                         RESPANGA(IANODE,IDOF)*FACANG
              ELSE
                WRITE(IOUT,230)NUM,IDOF,RESPMAGA(IANODE,IDOF),
     1                         PEAK_PER,
     2                         RESPANGA(IANODE,IDOF)*FACANG
              END IF
            END DO
          END IF
        END DO
      END DO
C
      NSINTOT = ISIN
      NAUXTOT = NAUXSIN
C
      DEALLOCATE(SPECMAG)
      DEALLOCATE(SPECMAGA)
      DEALLOCATE(SPECANG)
      DEALLOCATE(SPECANGA)
C
C ..... SET THE PARAMETERS FOR THE REGULAR AIRY WAVE ANALYSIS TO FOLLOW
C
      CALL SETREGWAV(WVNUM,OMEGA,WVAMP,WVRN,DIR,DEPTH,GR,IRANDSEA,
     1  EQ_AMP,EQ_DIR,PEAK_PER,EQ_PHA)
      IF( IRANDSEA == NUM_SEAS)THEN
C
C ..... NOW SET THE BOUNDARY CONDITION TYPE FROM VESSEL TO SINUSOIDAL
C
        DO IVSET = 1,NVSET                  ! Loop over the vessel sets
          DO INONP = 1,NATCH_RWE(IVSET)   ! Loop over the attached nodes
            NODE = IDATN_RWE(INONP,IVSET)      ! Get the node number
            DO IDOF = 1,NDEG                   ! Loop over the DOFs
              IF(NPBC(IDOF,NODE).EQ.3)THEN     ! Check for vessel BC
                NPBC(IDOF,NODE) = 11
              END IF
            END DO
          END DO
        END DO
C
C ..... SET THE FLAGS FOR THE REGULAR AIRY WAVE ANALYSIS TO FOLLOW
C
        IRAND      = 0        ! Reset the RAO flag so we don't calculate vessel
                              ! motions from RAOs like we'd normally do
        ISEA       = 1        ! Reset ISEA flag to a Regular Airy Wave analysis
        NDIR(1)    = NUM_SEAS ! Regular wave directions equals number of seas
        NHH(1)     = NUM_SEAS ! Regular wave harmonics equals number of seas
        NDIMAUXSIN = NANODE*NDEG*NVSET*NUM_SEAS ! Maximum no. of auxiliary BCs
        NHARZ(:)   = NUM_SEAS
        NHRZND(:)  = NUM_SEAS
        WRITE(IOUT,270)
      END IF
C
      RETURN
C
220   FORMAT(///,23X,'  SPECIFIED SINUSOIDAL DISPLACEMENTS ',//,
     18X,'Node           DOF          Amplitude          Period      ',
     2'      Phase',/)
230   FORMAT(5X,I7,7X,I6,3(3X,F15.3))
240   FORMAT(///,22X,'  EQUIVALENT REGULAR WAVE PROPERTIES ',//,
     14X,'Amplitude    Period    Direction',
     2'      Phase    Period Factor  Height Factor
     3',/)
250   FORMAT(1X,F10.3,4(3X,F9.3),7X,F9.3)
260   FORMAT(///,5X,'*REGULAR WAVE EQUIVALENT KEYWORD PRESENT',//,
     15X,'TREATING RANDOM SEA AS EQUIVALENT REGULAR WAVE')
270   FORMAT(///1X,78('='))
280   FORMAT(///,5X,'WAVE NUMBER :',I3,/,5X,16('-'))
290   FORMAT(///,5X,'DIRECTION NUMBER :',I3,/,5X,21('-'))
C
      END