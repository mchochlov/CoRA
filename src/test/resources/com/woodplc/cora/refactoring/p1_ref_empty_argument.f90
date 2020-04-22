!***********************************************************
!*             Product       : DeepRiser: DPRSTD           *
!*             Version       : 3.1.1                       *
!*             Release Date  : April, 2012                 *
!*             Job No.       : PR-11-1420                  *
!*             Module        : flx3d05.f90                 *
!*             Platform      : PC                          *
!*             Status        : Development                 *
!***********************************************************
!
!******************************************************************************************
!******************************************************************************************
!******************************************************************************************
!

      SUBROUTINE MAINA
!
! ----------------------------------------------------------------------
! Subroutine to perform initialisation and preliminary data input and
! determine pool storage required for profile solution scheme.
! ----------------------------------------------------------------------
!
      use active_element_data, only : initialise_active_element_data, deallocate_active_element_data
      use clashing_data, only : initialise_clashing_data, deallocate_clashing_data
      use floating_body_data, only : allocate_fb_data, deallocate_fb_data
      use guide_node_data, only : gnd_allocate_data, gnd_deallocate_data
      USE VESSEL_SETDOWN, ONLY : INITIALISE_VESSEL_SETDOWN_DATA
      USE ZG_GUIDE_DATA, ONLY : NZGG,INITIALISE_ZG_GUIDE_DATA, &
        DEALLOCATE_ZG_GUIDE_DATA
      USE WIND_DATA, ONLY : INITIALISE_WIND_DATA, DEALLOCATE_WIND_DATA
      USE AIR_CAN_DATA, ONLY : INITIALISE_AIR_CAN_DATA, DEALLOCATE_AIR_CAN_DATA
      USE CORIO_INERTIA, ONLY: INITIALISE_CORIO_INERTIA_DATA,  &
        DEALLOCATE_CORIO_INERTIA_DATA
      USE SOIL_MODEL_DATA, ONLY : INITIALISE_SOIL_MODEL_DATA,  &
        DEALLOCATE_SOIL_MODEL_DATA
      USE MODEL_DATA, ONLY : ALLOCATE_TENSIONER_DATA,  &
        DEALLOCATE_MODEL_DATA
      USE WEAKPOINT_DATA, ONLY : IWEAK_FLAG, NWEAKPTS,  &
                  INITIALISE_WEAKPTS_DATA, DEALLOCATE_WEAKPTS_DATA
      USE DEPLOYMENT_DATA, ONLY : IDEPLOY_FLAG, IDEPLOY_DRIFT_FLAG,   &
        INITIALISE_DRIFT_DEPLOYMENT_DATA,DEALLOCATE_DRIFT_DEPLOYMENT_DATA
      USE GUIDE_SURFACE_DATA, ONLY:  &
        INITIALISE_ELAS_GUIDE_SURF_DATA, DEALLOCATE_ELAS_GUIDE_SURF_DATA &
        , ALLOCATE_GUIDE_SURFACE_DATA, DEALLOCATE_GUIDE_SURFACE_DATA
      USE PIPE_IN_PIPE_DATA, ONLY :NPIPNOD,INITIALISE_PIPE_IN_PIPE_DATA,  &
        DEALLOCATE_PIPE_IN_PIPE_DATA, pip_set_dimensions
      USE NONLINEAR_CURVE, ONLY: INIT_NONLINEAR_CURVE
      USE NODE_SET_DATA, ONLY : ALLOCATE_NODE_SET_DATA, &
        DEALLOCATE_NODE_SET_DATA
      USE ELEMENT_SET_DATA, ONLY:ALLOCATE_ELEMENT_SET_DATA,  &
         DEALLOCATE_ELEMENT_SET_DATA
      USE BUOYANCY_DATA, ONLY:INITIALISE_BUOYANCY_DATA,  &
        DEALLOCATE_BUOYANCY_DATA
      USE WAVE_DATA, ONLY:ALLOCATE_WAVE_DATA, DEALLOCATE_WAVE_DATA,  &
        NUM_SEAS, NRANDSEA
      USE WAVE_SPECTRUM_DATA, ONLY:ALLOCATE_WAVE_SPECTRUM_DATA,  &
        DEALLOCATE_WAVE_SPECTRUM_DATA
      USE DAMPING_DATA, ONLY:INITIALISE_DAMPING_DATA,  &
        DEALLOCATE_DAMPING_DATA
      USE OCHI_HUBBLE_DATA, ONLY:ALLOCATE_OCHI_HUBBLE_DATA,  &
        DEALLOCATE_OCHI_HUBBLE_DATA
      USE DB_OUTPUT, ONLY: DEALLOCATE_DB_OUTPUT
      USE HDDIM2, ONLY : SIZE_IDACT, SIZE_AXPEN
      use drift_off_data, only: initialise_drift_off_data, deallocate_drift_off_data
      use hddim, only : numel, nonp, nnode, ndeg, ngens, ncord, n1, nnode2, n2
      use hp_tensioner_data, only : deallocate_hp_tensioner_data
      use vdu, only: iscrn
      use ANALYSIS_DATA, only : axial_capacity_analysis_flag, FATIGUE_ANALYSIS,  &
	& ILOG, PARA_ANALYSIS_NO, PARA_REC_NO
      IMPLICIT NONE
      !*--MAINA299
      !
      !*** START OF DECLARATIONS REWRITTEN BY SPAG
      !
      ! COMMON VARIABLES
      !
      
      common /DIRNAM/ ANALYSIS_DIR, STUMP
      CHARACTER(256) :: ANALYSIS_DIR
      CHARACTER(256) :: STUMP
      REAL(8) :: RHOA, STATIM, TIMLAST, WHEAD, WVEL
      REAL(8), DIMENSION(40) :: DUMP, FC, FW
      INTEGER :: IADM, IALL, ICALST,                                          &
        ICROUT, IIN, INT, IOUT,  IRAND, IRSTRT,                  &
        ISTADM, ISTAEL, ISTAND, ISTART, ISTAUX, ISTBNA, ISTBND,        &
        ISTBOY, ISTBYE, ISTCAB, ISTCUC, ISTCUR, ISTCVS, ISTDGC, ISTDTB,       &
        ISTELC, ISTELM, ISTFIL, ISTFLD, ISTINP, ISTMOMC, ISTMPL, ISTMTB,      &
        ISTNLBS, ISTNLM, ISTNOD, ISTNSP, ISTPBY, ISTPLD, ISTPLU, ISTAPN, ISTQTF, &
        ISTRAO, ISTREL, ISTSBD, ISTSEL, ISTSET, ISTSLG, ISTSOL, ISTSPR,       &
        ISTSRF, ISTSTF, ISTTRS, ISTTTR, ISTUWV, ISTVES, ISTWIC, ISTZGG,       &
        ITYPE, MAXRE,                     &
        MAX_AIRCAN_SETS, MXADM, MXANOD, MXART,MXAUX, MXBC, MXCAB, ISTARV
      INTEGER, DIMENSION(40) :: LM
      INTEGER :: MXCUR, MXDMPER, MXDRG, MXEL, MXFREQ, MXHEAD,        &
        MXINT, MXLOAD, MXNLT, MXNOD, MXNODC, MXPAIR, MXRAOS,         &
        MXREL, MXSPR, MXSPTS, MXSPTV, MXUDL, MXWIND, N2D, NAD,        &
        NADMAS, NAELEM, NAPANELS, NANODE, NART, NATND, NAUX, NAXPD, NBCAUX,   &
        NBNSTF, NBNSTFCK, NBSIN, NCHEAD, NDEG1, NDFREQ,          &
        NDHEAD, NDRAGC, NEQACT,              &
        NFREQ, NFSPEC, NLSUB, NMGSE, NMGSE0, NMGSN,                &
        NMGSN0, NMPOOL, NMSLUG,NN2, NN3, NNDSPR,       &
        NPRESS, NREACT, NRSDOF,                          &
        NSPRNG, NSPRTB, NSRFSET, NSURF, ISTFJL, ISTFJN, ISTDMP, ISTACN,       &
        IDRIFT, ITENS
      INTEGER :: NTHRST, NTHTM, NTORQD, NTTR, NVAR1,             &
        NVSET, NWHEAD, NWVHD
      COMMON /AM    / NADMAS, NART
      COMMON /AUXDIM/ MXAUX, MXANOD, NAUX, NANODE, NAELEM, NAPANELS, NBCAUX
      COMMON /BEND  / NBNSTF
      COMMON /BENDCK/ NBNSTFCK
      COMMON /CONTACT/ NSURF, NSRFSET, NMGSN, NMGSN0, NMGSE, NMGSE0
      COMMON /CURRENT/ NCHEAD
      COMMON /DIMCUR/ N2D
      COMMON /DISPBC/ NRSDOF, NN2, NN3, NAD, NEQACT
      COMMON /DRAGCH/ NDRAGC
      COMMON /IADRES/ ISTINP, ISTREL, ISTNOD, ISTELM, ISTADM, ISTART,     &
        ISTCUR, ISTNLM, ISTRAO, ISTBND, ISTPLD, ISTPLU, ISTELC, ISTCVS,   &
        ISTUWV, ISTMTB, ISTSPR, ISTCAB, ISTDGC, ISTTTR, ISTNSP, ISTFLD,   &
        ISTSTF, ISTBYE, ISTPBY, ISTFIL, ISTSBD, ISTDTB, ISTAUX, ISTAND,   &
        ISTAEL, ISTBNA, ISTSRF, ISTSEL, ISTMPL, ISTVES, ISTCUC, ISTWIC,   &
        ISTAPN, ISTQTF, ISTTRS, ISTMOMC, ISTNLBS, ISTSLG, ISTSET, ISTBOY, &
        ISTSOL, ISTZGG, ISTFJL, ISTFJN, ISTDMP, ISTACN, IDRIFT, ITENS,    &
        ISTARV
      COMMON /INOUT / IIN, IOUT, IADM, IALL
      COMMON /LOAD  / NFSPEC, NPRESS, NLSUB
      COMMON /MAXES / MXINT, MXREL, MXNOD, MXEL, MXADM, MXART, MXSPR,    &
        MXCUR, MXFREQ, MXHEAD, MXBC, MXLOAD, MXUDL, MXPAIR, MXCAB,       &
        MXNODC, MXSPTS, MXNLT, MXDRG, MXRAOS, MAXRE,    &
        MXDMPER, MXWIND, MAX_AIRCAN_SETS
      COMMON /MISC  / FC, FW, DUMP, LM, ITYPE, INT, ICROUT
      COMMON /MOONPOOL/ NMPOOL
      COMMON /NDSPR / NNDSPR
      COMMON /PENDAT/ NTORQD, NAXPD
      COMMON /REACTS/ NREACT
      COMMON /RTSTAT/ TIMLAST, STATIM, ICALST
      COMMON /SINUSOID/ NBSIN
      COMMON /SLUGS / NMSLUG
      COMMON /SPRING/ NSPRNG, NSPRTB, MXSPTV
      COMMON /START / IRSTRT, IRAND, NFREQ, NDEG1, NATND, NWVHD, NVSET
      COMMON /THETM / NTHTM
      COMMON /THRSTR/ NTHRST
      COMMON /TIMET / NTTR
      COMMON /VARS  / NVAR1
      COMMON /VDRIFT/ NDHEAD, NDFREQ
      COMMON /WIND  / WVEL, WHEAD, RHOA, NWHEAD
      !
      ! LOCAL VARIABLES
      !
      INTEGER :: IADDR, IDUMMY, MEND, MIATN, MIDACT,                         &
        MIDNLM, MIDNTP, MIDNTPBS, MIORDR, MISTOR, MJDIAG, MJEL, MLABEQ,      &
        MNEXT, MNORDR, MNPI, NRAO, NVCORD, NVDEG
      !
      !*** END OF DECLARATIONS REWRITTEN BY SPAG

      ! Initialization
      IDUMMY = 0

!
! OPEN THE TEMPORARY LOG FILE FOR PARAMETER-STUDY ANALYSIS
!
      ! CALL OPEN_PARAMETER_LOG_FILE
      CALL OPEN_PARAMETER_LOG_FILE(PARA_ANALYSIS_NO, ILOG, PARA_REC_NO,  &
	& FATIGUE_ANALYSIS, axial_capacity_analysis_flag, IDEPLOY_FLAG, iscrn, IIN,  &
	& IOUT, ANALYSIS_DIR, STUMP)
!
! FIND THE NUMBER OF RANDOM SEAS, AND TOTAL SEAS FOR DIMENSIONING ARRAYS
!
      CALL FIND_NUM_SEAS(IIN)

      CALL ALLOCATE_WAVE_DATA(ISCRN, IOUT, 0)
      call initialise_clashing_data(iscrn, iout)
      call allocate_fb_data(iscrn, iout)
      call gnd_allocate_data(iscrn, iout)
!
! CALL PRELIMINARY DATA INPUT ROUTINE
!
      CALL PRPOOL
      
      ! Set other dimensions and parameters.
      call pip_set_dimensions()
!
! INITIALISATION
!
      ICROUT = 2
      ITYPE  = 1
      ICALST = 0    ! Flag to indicate calculation of runtime statistics

      NADMAS  = NADMAS  + 1
      NART    = NART    + 1
      NSPRNG  = NSPRNG  + 1
      NSPRTB  = NSPRTB  + 1
      MXSPTV  = MXSPTV  + 1
      NFREQ   = NFREQ   + 1
      NRSDOF  = NRSDOF  + 1
      NATND   = NATND   + 1
      NDRAGC  = NDRAGC  + 1
      NNDSPR  = NNDSPR  + 1
      NLSUB   = NLSUB   + 1
      NTTR    = NTTR    + 1
      NBSIN   = NBSIN   + 1
      NSURF   = NSURF   + 1
      NZGG    = NZGG    + 1
      NSRFSET = NSRFSET + 1
      NMPOOL  = NMPOOL  + 1
      NBNSTFCK = 1
      IF(NBNSTF.EQ.0)THEN
         NBNSTF = NBNSTF + 1
         NBNSTFCK = 0
      ENDIF
      NMSLUG   = NMSLUG + 1
      IF(NWVHD.EQ.0)NWVHD = 1
      NCHEAD = NCHEAD + 1
      NDFREQ = NDFREQ + 1
      NDHEAD = NDHEAD + 1
      NTHRST = NTHRST + 1

      NWHEAD = NWHEAD + 1
      MXDMPER = MXDMPER + 1
      IF(IWEAK_FLAG == 1) NWEAKPTS = NWEAKPTS + 1
!
! 3-D HYBRID BEAMCOLUMN ELEMENT WITH TORSION PENALTY
!
      NCORD  = 3
      NDEG   = 6
      NDEG1  = NDEG + 7
      NGENS  = 4
      NTHTM  = 2
      NNODE  = 2

      NN3    = NONP   *  NDEG   + 2*SIZE_IDACT
      N2     = INT    *  NUMEL
      N2D    = 3      *  NUMEL
      NN2    = N2     *  NGENS
      N1     = NDEG   *  NNODE  + 2
      NVAR1  = NDEG   * (NNODE  + 1)
      NNODE2 = NNODE  *  NNODE
      NRAO   = NFREQ  *  NDEG1  * NWVHD * MXRAOS
      NREACT = NRSDOF *  NDEG
      NTORQD = 7      *  NUMEL
      NAXPD  = 7      *  SIZE_AXPEN
      NVCORD = NCORD  *  MXRAOS
      NVDEG  = NDEG   *  MXRAOS
!
! FIND THE NUMBER OF RANDOM SEAS, AND TOTAL SEAS FOR DIMENSIONING ARRAYS
!
      IADDR = ISTINP + 5
      READ(IIN,REC=IADDR)IDUMMY,IDUMMY,NRANDSEA,NUM_SEAS,IDUMMY,IDUMMY,  &
        IDUMMY
      IF(NRANDSEA.EQ.0)NRANDSEA = 1

      CALL FIND_NUMBER_DIVISIONS(IIN,ISTUWV,ISTINP)
!
! SET UP SPACE FOR PROFILE SOLUTION SCHEME
!
      MNPI      = 1
      MIDNTP    = MNPI   + NNODE*NUMEL
      MIDNTPBS  = MIDNTP + NUMEL
      MJEL      = MIDNTPBS + NUMEL
      MIATN     = MJEL   + NART
      MJDIAG    = MIATN  + NNODE*NART
      MIDACT    = MJDIAG + NN3
      MLABEQ    = MIDACT + NUMEL*N1
      MIDNLM    = MLABEQ + NN3
      MNORDR    = MIDNLM + NUMEL
      MIORDR    = MNORDR + (NUMEL+NPIPNOD)*3
      MISTOR    = MIORDR + NUMEL+NPIPNOD
      MNEXT     = MISTOR + NUMEL+NPIPNOD
      MEND      = MNEXT  - 1

      CALL ALLOCATE_ELEMENT_SET_DATA(NUMEL,ISCRN,IOUT)
      CALL ALLOCATE_TENSIONER_DATA(ISCRN,IOUT)
      CALL INIT_NONLINEAR_CURVE
      CALL INITIALISE_VESSEL_SETDOWN_DATA(MXRAOS)
      IF (NZGG>1 .OR. NSURF>1) THEN
        CALL INITIALISE_ZG_GUIDE_DATA(NONP,NUMEL)
      ELSE
        CALL INITIALISE_ZG_GUIDE_DATA(1,1)
      END IF
      CALL ALLOCATE_OCHI_HUBBLE_DATA(ISCRN, IOUT)
      CALL INITIALISE_WIND_DATA(MXWIND)
      CALL INITIALISE_AIR_CAN_DATA(MAX_AIRCAN_SETS)
      CALL INITIALISE_CORIO_INERTIA_DATA(NUMEL)
      CALL INITIALISE_SOIL_MODEL_DATA(ISCRN,IOUT)
      call initialise_buoyancy_data(iscrn,iout)
      if(iweak_flag == 1)then
        call initialise_weakpts_data(nweakpts, iscrn, iout)
      end if
      IF(IDEPLOY_FLAG == 1 .AND. IDEPLOY_DRIFT_FLAG == 1) THEN
        CALL INITIALISE_DRIFT_DEPLOYMENT_DATA(NVSET)
      END IF
      CALL INITIALISE_ELAS_GUIDE_SURF_DATA(NSURF, NUMEL, NONP)
      IF (NSURF>1) THEN
        CALL ALLOCATE_GUIDE_SURFACE_DATA(NSURF,NONP,ISCRN,IOUT)
      ELSE
        CALL ALLOCATE_GUIDE_SURFACE_DATA(1,1,ISCRN,IOUT)
      END IF
      CALL INITIALISE_PIPE_IN_PIPE_DATA(NUMEL, ISCRN, IOUT, NONP)
      CALL ALLOCATE_NODE_SET_DATA(NONP, ISCRN, IOUT)
      CALL INITIALISE_DAMPING_DATA(MXEL,N1,NUMEL,NPIPNOD,ISCRN, IOUT)
      call initialise_drift_off_data
      call initialise_active_element_data(iscrn,iout,numel,nonp)

!
! WAVE_SPECTRUN_DATA MODULE
!
      CALL ALLOCATE_WAVE_SPECTRUM_DATA(ISCRN,IOUT)

      CALL MAINB(MEND,NRAO,NVCORD,NVDEG,NRANDSEA,NUM_SEAS)


      CALL DEALLOCATE_ZG_GUIDE_DATA
      CALL DEALLOCATE_ELEMENT_SET_DATA
      CALL DEALLOCATE_WIND_DATA
      CALL DEALLOCATE_AIR_CAN_DATA
      CALL DEALLOCATE_CORIO_INERTIA_DATA
      CALL DEALLOCATE_SOIL_MODEL_DATA
      CALL DEALLOCATE_MODEL_DATA
      CALL DEALLOCATE_WEAKPTS_DATA
      CALL DEALLOCATE_DRIFT_DEPLOYMENT_DATA
      CALL DEALLOCATE_DAMPING_DATA
      CALL DEALLOCATE_WAVE_SPECTRUM_DATA
      CALL DEALLOCATE_ELAS_GUIDE_SURF_DATA
      CALL DEALLOCATE_GUIDE_SURFACE_DATA
      CALL DEALLOCATE_PIPE_IN_PIPE_DATA
      CALL DEALLOCATE_BUOYANCY_DATA
      CALL DEALLOCATE_WAVE_DATA
      CALL DEALLOCATE_OCHI_HUBBLE_DATA
      CALL DEALLOCATE_DB_OUTPUT
      CALL DEALLOCATE_NODE_SET_DATA
      call deallocate_drift_off_data
      call deallocate_hp_tensioner_data
      call deallocate_clashing_data
      call deallocate_fb_data
      call gnd_deallocate_data
      call deallocate_active_element_data

      END