
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************


!>
!! Subroutine that reads the restart data and the remaining input data, initialises
!! analysis data, calls the cable solver, iterates the time loop and outputs results.
!<
subroutine mainb

  ! arrays from maina
  use maina_and_mainb_arrays, only :cord,b,data1,e,ss,npi,disp,dispt,d,sk,sxx,nume,npnum,data2,td,&
    tf,npbc,tx,data3,th,tq,ires,dd,tv,ta,tvs,ex,sigxx,dicos,tdicos,press,disp1,disp2,    &
    nharz,dragd,dint,vcur,cdnn,cdtt,cmnn,cann,cmtt,reno,nore,dd0,dd1,dd2,dd3,dd31,re,dd4,dd5,&
    ts0,ts,locm,admas,jel,outp,rao,offset,react,hawst,numspp,idnlmt,stiff,vmass,tdst,dsflcl,      &
    elmrst,spress,data4,data5,gs,plndr,idatn,iartnd,nhrznd,axpdat,dreact,offbcn,icab,xlen,idntp,  &
    sprfrc,reacsb,sbdres,idsbe,idsbn,reacs0,idsbe0,idsbn0,jdiag,idact,labeq,vlhsu,vlhsl,vrhs,     &
    rfaca,irdof,bound,w0,trqdat,defloc,rigvec,ieldfx,rotact,tglu,trgb,rtct0,rtctg,imvrg,imvrg0,   &
    athet,gdef,gdefs,locdg,dgch,idttr,iloc,forout,locs,xndspr,ptld,alllod,fluid,lsub,       &
    alphat,temp,jjel,tqold,tqnew,veccg0,dspcg_lf,dspcg,dspcgd,dspcgdd,trcgd,trcgdd,voffset,       &
    voffini,natch,nfrqhd,bnprop,bndstf,kstiff,icolor,rhs,rhs0,sprop,iaxfl,isbelm,vrelp,vrelc,     &
    boyste,xlenboy,ptboye,iptby,norder,ibcfil,ibcref,nbcfil,nbcref,refip,statof,tglsb,trgsf,      &
    seabed_axis,sxxt,rtstatm,rtstatf,sindata,acord,iaelem,ielaux,  &
    tdaux,nodea,npia,nbaux,dout,idisby,srestf,erestf,cordo,deffec,ivdrift,ves_drift,&
    ivesttr,implhyd,imnpl_ves,cord_mnpl,ax_mnpl,dim_mnpl,td_mnpl,tdax_mnpl,dspcg0,idnlmtbs,bsdiam,&
    vadmas,vedam,vescog,wcoef,cuc,qtf,thrust,auxsin,pslug,dist,islug,islug_fill,indic,calmmass,calmdamp,    &
    irbcfl,irbcrf,winch,numwin,winch_set,xleno,xlenp,renotab,iei_flag,cdamp,rotk,damper,   &
    iorder,istore,rtstatv,rtstata,wki_lift_nom, winch_timetrace_flag, epi,centrifugal_nodal,ilen_curv_tab, &
    numsynrp, elmrst_prev
  
  !arrays from mainb
  use maina_and_mainb_arrays, only :mainb_allocate_1, mainb_deallocate,npmomcur, kstiffnl,imomcur,&
    vessel_axis, ieldtb, inodtb, vessel_velocity, curvyt,     &
    curvzt,dcntct, bsmomcur, elmrst0, raova, phasa, stiff_modes,vmass_modes, bcnffl,  &
    bcnfrf, tmpffl, tmpfrf,drift_file, vesttr_file, tmpves, deallocate_profile_arrays, &
    reallocate_profile_arrays

  use warnings, only: warning
  use a02, only : iasmbl, itim, itim_synrp
  use airy, only : eomega, ifairy
  use am, only : nart
  use auxdim, only : nanode
  use bcdt, only : tstart, tfinsh, tstep, ptime, time, epsi, seabed_tol
  use bend, only : nbnstf
  use buoy, only : nptboy
  use cable, only : icaban
  use coffile, only : filcof, icof
  use curdat, only : icur, icursu
  use datbsi, only : idatb, ialdb, iendb, itimst, idbstat, writedbm, writedbf, ndtbel
  use datbsr, only : stdb, timdb, statstm
  use filebc, only : ifbc2, ndfile, ndrefc
  use flexib, only : rhop, rhopo, rhptq, rhptqo
  use hddim, only : numel, nonp, nnode, ndeg, ngens, ncord, n1, size_axpen, size_epi
  use inout, only : iout
  use maxes, only : mxraos
  use misc, only : num_int
  use moonpool, only : nmpool
  use moorfil, only : imr, mooring_file
  use mooring, only : moor, imstc
  use morold, only : icrold, iwvold
  use newtim, only : tstprt
  use flexjoints, only: fj_status, allocate_flexjoints, deallocate_flexjoints
  use nondyn, only : nodyn
  use prevus, only : tfprev
  use print_data_output, only : print_data,nprint,Reynolds_COF,print_end_time
  use ramp_mod, only : tramp, stan_ramp
  use reynold, only : ireyn, nwvpass, nperiod, inst_reyn
  use seabed_data, only : irigsf
  use rnd2reg, only : irnd2reg
  use rtstat, only : statim, icalst
  use slugs, only : nmslug, num_columns
  use spring, only : nsprtb, mxsptv
  use start, only : irstrt, irand, nfreq, ndeg1, natnd, nwvhd,nvset
  use static_mod, only : istatc
  use theory, only : ismall
  use timedata, only : time_trace_data
  use timet, only : nttr
  use timvar, only : iincr
  use torqe, only : itorq
  use type_mod, only : noutyp
  use vdu, only : iscrn, run_from_gui
  use vessel, only : icog
  use vforces, only : fves
  use zg_guide_data, only : nzgg
  use wave_data, only : ndimwv, nhh, isea, nrandsea, num_seas, omega, wvamp, wvrn, dir, wvnum, &
    History, wave_kinematics
  use floating_body_data, only : fb_anal, fb_refnode, num_fbset, nrecs_vel, iaddr_vel, fb_set,     &
    nraddampfq, ireta, iretb, ivel, ireta_cpl, iretb_cpl, ncouple, ncouplingfq, coupling_bodies,   &
    fb_cognode, fbpos, fbpos_prev
  use hysteresis_data, only : is_hysteresis_in_this_model, pre_hysteresis_step,                    &
    initialise_hysteresis_data, reset_hysteresis_data,update_eihexpect,                            &
    pre_hysteresis_step_convergence, hysteresis_step_convergence, &
    is_hysteresis_in_this_analysis, was_hysteresis_in_previous_analysis
  use hysteresis_output_data, only : initialise_hysteresis_output_data,reset_hysteresis_output_data
  use refnod_data, only : deallocate_refnod_data
  use vessel_data, only : deallocate_vessel_data, option_global
  use rstfil, only: final_static_required
  use analysis_data, only : analysis_stage, criteria_static, sheave_static
  use criteria_data, only : crit_not_conv, crit_inc_adjust, crit_abs_adjust, crit_adj_x2,          &
    crit_terminate, crit_iter, crit_sat
  use guide_vessel_data, only : gvd_num_surf, gvd_init_node_map, gvd_init_surf_cont, gvd_guide_response
  use guide_node_data, only : gnd_init_node_map, gnd_init_surf_cont  
  use active_element_data, only : active_bcs_node
  use clashing_data, only : update_clash_connectivity, num_clash_regs 
  use pipe_in_pipe_data, only : npipnod, update_pip_connectivity, pip_bandwidth, pip_diameter, &
    & fill_pip_diameter, npipsect
  use viv_drag_data, only: viv_drag_nelem, viv_drag_npoints, calc_drag_amplific_factors,           &
    apply_viv_drag, viv_drag_set_name
  use wake_interference, only : wki_model, wki_off, wki_user, get_upstream_data,wki_skip,          &
    allocate_wake_output_array,  get_downstream_data, wki_set_name, wki_nset
  use version_info, only : iversn
  use XML_Outputs, only : write_XML_tag
  use gui_dll_interface , only : flexcom_set_analysis_state, flexcom_get_analysis_cmd, dll_handle, &
    cmd_stop, state_stopped, cmd_pause, suspend_resume
  use extra_operators
  use ifport, only : timef
  use damping_data, only: alpha_f, alpha_f0
  use k_linear_data, only: k_linear
  use shape_function_data, only: maxint, fc, fw, fb, t, t2
  use dispbc, only : nad
  use bndwth, only : noband
  use math, only : smallnum, pi
  use database_multi_threading, only : wait_for_and_close_thread, threaded_db_flag
  use wave_time_history_data, only : process_timehistory_data, convrt_waveperiod_cirfreq
  use db_output, only: loadcase_GUID
  use others, only : iodbm, iodbf
  use matrix_vector_op, only: matrix_transpose
  use GUID_data, only: getGUID
  use synrp, only: nsynrpset, synrp_maxitr, synthetic_rope_analysis_requested
  use units, only : base_units, restart_base_units, file_extensions
  use bendck, only : nl_mat_nl_bnstf_ck
  use mod_plasticity_data, only: allocate_plas_data_main, &
    & plas_curve_const, plas_curve_vary, &
    & plas_model_no, plas_model, youngs_mod, gj_mod, &
    & deallocate_plas_store_data, deallocate_plas_output_data, &
    & allocate_plas_output_data
  use mod_plasticity_general, only: fill_in_constant_data_gen
  use mod_plastic_hardening, only: nplas_model, plas_calc_ss_curve_d
  use memory_data, only: total_mem_aloc
  use poisson_data, only: poisson
  use mod_common_database, only: db_plas_strain, db_eqv_plas_strain, idbcon
  use mod_cloud_licensing, only : set_token_threshold_time, successful_token_analysis
  use node_set_data, only : ndset_defn, ndset_name, ndset_nod, nndset
  use pipe_in_pipe_data, only : nested_section_limit, pip_inner_node,  &
	& pip_nodes, pip_section
  implicit none
  
  ! Declare local variables

  real(8), dimension(6) :: stinger_plane

  integer :: nwinch
  integer :: nsynrp
  integer :: iaddr
  integer :: iti
  integer :: icgrf
  integer :: istrs
  integer :: irndchk
  integer :: idspo
  integer :: itimo
  integer :: itimon
  integer :: idspon
  integer :: iclear
  integer :: ndrift_files
  integer :: nvesttr
  integer :: istat_step
  integer :: i
  integer :: iap1
  integer :: iap2
  integer :: iap3
  integer :: iap4
  integer :: iap5
  integer :: iap6
  integer :: iap7
  integer :: itq1
  integer :: itq2
  integer :: itq3
  integer :: itq4
  integer :: itq5
  integer :: itq6
  integer :: itq7
  integer :: ngset
  integer :: nmomcur
  integer :: nnlbs
  integer :: nnlbsel
  integer :: ndimauxsin
  integer :: nauxsin
  integer :: iunit
  integer :: ifloat
  integer :: ifloat1
  integer :: ifloat2
  integer :: icouple
  integer :: idir
  integer :: ifbset
  integer :: icurve
  integer :: j
  integer :: ielem
  integer :: k
  integer :: l
  integer :: jj
  integer :: ndvar
  integer :: isec
  integer :: iusers
  integer :: idummy
  integer :: ivset
  integer :: iro
  integer :: ii
  integer :: itiold
  integer :: iclold
  integer :: file_recl ! function
  integer :: n
  integer :: nad_temp
  integer :: gui_cmd
  integer :: iresult
  integer :: max_tstep
  integer :: max_column
  integer :: nn
  integer :: iel
  integer(1), dimension(16) :: GUIDdata
  integer :: ierr
  integer(8) :: isize
  !integer(IntKi) :: errStat              ! Status of error message, used in AeroDyn
  !integer :: ntim_AeroDyn
  
  real rnum   ! leave this as it is

  real(8) :: start_ttr
  real(8) :: end_ttr
  real(8) :: interval_ttr
  real(8) :: time_elapsed
  real(8) :: stfmor
  real(8) :: gr
  real(8) :: depth
  real(8) :: rhow
  real(8) :: do1
  real(8) :: do2
  real(8) :: di
  real(8) :: deq
  real(8) :: rout
  real(8) :: rin
  real(8) :: x
  real(8) :: xk
  real(8) :: xmoment
  real(8) :: arm
  real(8) :: strain
  real(8) :: stress
  real(8) :: get_stress_from_strain ! function
  real(8) :: sectarea
  real(8) :: timed
  real(8) :: timep
  real(8) :: tact
  real(8) :: wh
  real(8) :: eof
  real(8) :: pipe_ramp
  real(8) :: rmsprf
  real(8) :: elapsed
  real(8) :: checktime
  real(8) :: tstep_user
  real(8) :: threshold_period
  
  logical :: imod_rst
  logical :: analysis_stopped 
  logical :: drag_apply
  logical :: last_tstep
  logical :: is_nl_flexjoint
  logical :: stopped_after_pause
  logical :: less_than_end
  logical :: do_synt_rope_loop
  logical :: perform_last_step
  logical :: finished_time_loop
  logical :: static_anal
  logical :: qstatic_anal
  logical :: not_mooring_anal
  logical :: length_adjustment_permitted
  logical :: offset_conv_satisfied
  logical :: synrp_maxitr_reached
  logical :: section_limit_exceeded(3)
  logical :: warn_section_limit_exceeded

  real(8), dimension(5) :: vrelcon
  real(8), dimension(6) :: velglo
  real(8), dimension(6) :: velloc
  real(8), dimension(2) :: fvespr
  real(8), dimension(2) :: tdcogp
  real(8), dimension(3) :: cmomnt
  real(8), dimension(6,6) :: xltg
  real(8), dimension(6,6) :: xltgt
  real(8), dimension(8,4) :: calmdata

  character(len=2)   :: ext1
  character(len=2)   :: ext2
  character(len=50)  :: retard_filename
  character(len=200) :: message
  !character(ErrMsgLen) :: errMsg   ! Error message if ErrStat /= ErrID_None used in AeroDyn

  ! Wave time history local variables
  real(8) :: wth_elapstime   ! Wave time history elapsed time
  logical :: is_wave_timehist ! Flag for time history wave
  logical :: first_wth_print  ! Flag to indicate first print of wave time history data

  ! Initialise local variables
  isize = 0
  nwinch = 0
  nsynrp = 0
  iaddr = 0
  iti = 0
  icgrf = 0
  istrs = 0
  irndchk = 0
  idspo = 0
  itimo = 0
  itimon = 0
  idspon = 0
  iclear = 0
  ndrift_files = 0
  nvesttr = 0
  istat_step = 0
  i = 0
  iap1 = 0
  iap2 = 0
  iap3 = 0
  iap4 = 0
  iap5 = 0
  iap6 = 0
  iap7 = 0
  itq1 = 0
  itq2 = 0
  itq3 = 0
  itq4 = 0
  itq5 = 0
  itq6 = 0
  itq7 = 0
  ngset = 0
  nmomcur = 0
  nnlbs = 0
  nnlbsel = 0
  ndimauxsin = 0
  nauxsin = 0
  iunit = 0
  ifloat = 0
  ifloat1 = 0
  ifloat2 = 0
  icouple = 0
  idir = 0
  ifbset = 0
  icurve = 0
  j = 0
  ielem = 0
  k = 0
  l = 0
  jj = 0
  ndvar = 0
  isec = 0
  iusers = 0
  idummy = 0
  ivset = 0
  iro = 0
  ii = 0
  itiold = 0
  iclold = 0
  max_tstep = 0
  max_column = 0
  nn = 0
  iel = 0


  rnum = 0.0 ! this is a real(4). 
 
  start_ttr = 0.d0
  end_ttr = 0.d0
  interval_ttr = 0.d0
  time_elapsed = 0.d0
  stfmor = 0.d0
  gr = 0.d0
  depth = 0.d0
  rhow = 0.d0
  calmdata = 0.d0
  do1 = 0.d0
  do2 = 0.d0
  di = 0.d0
  deq = 0.d0
  rout = 0.d0
  rin = 0.d0
  x = 0.d0
  xk = 0.d0
  xmoment = 0.d0
  arm = 0.d0
  strain = 0.d0
  stress = 0.d0
  sectarea = 0.d0
  timed = 0.d0
  timep = 0.d0
  tact = 0.d0
  wh = 0.d0
  eof = 0.d0
  checktime = 0.d0
  elapsed = 0.d0

  vrelcon = 0.d0
  velglo = 0.d0
  velloc = 0.d0
  fvespr = 0.d0
  tdcogp = 0.d0
  cmomnt = 0.d0
  xltg = 0.d0
  xltgt = 0.d0
  wth_elapstime = 0.d0
  fbpos = 0.d0
  fbpos_prev = 0.d0
  tstep_user = 0.d0
  ext1 = " "
  ext2 = " "
  retard_filename = " "
  
  !errStat     = ErrID_None
  !errMsg      = ''

  imod_rst = .false.
  analysis_stopped = .false.
  drag_apply = .false.
  last_tstep = .false.
  stopped_after_pause = .false.
  is_wave_timehist = any(isea(:) == History)
  first_wth_print = .true.
  offset_conv_satisfied = .false.
  synrp_maxitr_reached = .false.
  section_limit_exceeded = .false.
  warn_section_limit_exceeded = .false.

  ! Call the TIMEF function early on the in program to initialise it
  elapsed = dble(timef())


  
  ! Allocate mainb arrays
  call mainb_allocate_1

  
  ! at the time of release of version 6.1.1, we are only interested in
  ! in the low frequency motions of the vessel cog. these are written
  ! out to an ascii file which may be input as a drift timetrace file
  ! in subsequent analyses. the second loop of dynamic mooring
  ! analysis to calculate the combined low and high frequency motions
  ! is not used in this release.

  iaddr = 1

  ! itim = 1 indicates initial pass of solution

  itim         = 1
  itim_synrp   = 0
  iincr        = 0
  icrold       = 0
  iwvold       = 0
  iasmbl       = 0
  iti          = 0
  icgrf        = 0
  istrs        = 0      ! counter for dongle-checking
  irndchk      = 1      ! initial check frequency
  idspo        = 0
  itimo        = 0
  itimon       = 0
  idspon       = 0
  iclear       = 0
  ndrift_files = 0      ! no. of vessel drift timetrace files
  nvesttr      = 0      ! no. of vessel timetrace files
  istat_step   = 0      ! static analysis step counter

  itimst       = 0

  call random_seed()    ! seed with time-dependent value

  ! set up initial values for control variables for cd = f(re)

  nperiod = 0
  nwvpass = 0
  do i = 1,5
    vrelcon(i) = 0.0
  end do

  ! set up axial and torque penalty data

  iap1 = 1
  iap2 = iap1 + size_axpen
  iap3 = iap2 + size_axpen
  iap4 = iap3 + size_axpen
  iap5 = iap4 + size_axpen
  iap6 = iap5 + size_axpen
  iap7 = iap6 + size_axpen

  itq1 = 1
  itq2 = itq1 + numel
  itq3 = itq2 + numel
  itq4 = itq3 + numel
  itq5 = itq4 + numel
  itq6 = itq5 + numel
  itq7 = itq6 + numel

  ! Read restart file
  if(irstrt /= 0)then
    itorq = 1
    call restart_file_read_record(iap3,iap6,iap7,itq3,itq6,itq7,stfmor,acord)
    xlen = xlenp
    xleno = xlenp
    
    ! Check if the file extension matches between this keyword file and its restart keyword file
    ! For instance, a keyxm cannot restart from a keyx or keyxi
    if( base_units /= restart_base_units )then
      write(message,fmt="(5a)")"The extension of this restart keyword file (", &
                              trim(file_extensions(base_units)), &
                              ") must match that of the last analysis (", &
                              trim(file_extensions(restart_base_units)), &
                              ")."
      call error_handling(iscrn, iout, 507,trim(message))
    end if
    
    ! if present run is static, reset velocities and accelerations from previous
    ! analysis to zero - it might have been quasi-static (fault report 97 in
    ! version 6.1.5)
    if(nodyn == 1)then
      dd1 = 0.d0
      dd2 = 0.d0
    end if
  else
    ! Allocate the flex joint data from the input file.
    call allocate_flexjoints(iscrn,iout,numel)

    ! Allocate plasticity data.
    if( nplas_model > 0 )then
      isize = 0
      call allocate_plas_data_main(ngens, num_int, numel, ierr, isize)
      if( ierr /= 0 )then
        call error_handling(iscrn, iout, 253, &
          & "Error: Unable to allocate sufficient memory for main plasticity data.")
      end if
      total_mem_aloc = total_mem_aloc + isize
    end if

  end if

  ! diameters of bend stiffener elements
  bsdiam = 0.d0
  
  ! Output unit system info.
  call output_unit_system_info

  ! Set curvature slippage origins
  call initialise_hawst_origin()

  ! Read input

  call read_input_file_main(nume,npi,data1,data2,npnum,cord,data3,gr,nharz,depth,rhow,dragd,dint, &
    vcur,cdnn,cdtt,cmnn,cann,cmtt,reno,nore,wki_lift_nom,locm,admas,jel,rao,hawst,numspp,    &
    idnlmt,data4,data5,gs,plndr,tglu,dd0,nhrznd,offset,icab,xleno,idntp,locdg,dgch,idttr,   &
    iloc,locs,xndspr,fluid,alphat,temp,jjel,veccg0,dspcg_lf,voffset,nfrqhd,bnprop,bndstf,kstiff,  &
    icolor,sprop,iaxfl,boyste,xlenboy,ptboye,iptby,ieldtb,acord,iaelem,ielaux,nodea,npia,nbaux,   &
    ngset,dout,idisby,cordo,deffec,ivdrift,ndrift_files,drift_file,ves_drift,ivesttr,nvesttr,vesttr_file,implhyd,  &
    imnpl_ves,cord_mnpl,ax_mnpl,dim_mnpl,vadmas,vedam,vescog,wcoef,cuc,qtf,thrust,cmomnt,stfmor,  &
    nmomcur,bsmomcur,npmomcur,nnlbs,kstiffnl,imomcur,nnlbsel,bsdiam,pslug,nrandsea,num_seas,      &
    dcntct,calmdata,ss,sk,sxx,calmmass,calmdamp,ismall,auxsin,sindata,npbc,rtct0,voffini,tdst,    &
    ndimauxsin,nauxsin,winch,nwinch,numwin,winch_timetrace_flag,winch_set, vessel_velocity,       &
    vessel_axis,damper,rotk,stinger_plane,nsynrp,numsynrp)

  
  ! Check if the selected nonlinear method is used correctly.
  call check_nonlinear_method
  
  ! Check if the slippage is used correctly.
  call check_hysteresis_slippage  

  if( nplas_model > 0 )then
    isize = 0
    call plas_calc_ss_curve_d(numel, isize, ierr, &
      & youngs_mod, plas_model_no, &
      & nplas_model, plas_model)
    if( ierr /= 0 )then
      call error_handling(iscrn, iout, 253, &
        & "Error: Unable to allocate sufficient memory for plasticity stress-strain data.")
    end if
    total_mem_aloc = total_mem_aloc + isize

    if( irstrt == 0 )then
      isize = 0
      call fill_in_constant_data_gen(numel, nplas_model, &
        & plas_model_no, plas_model, youngs_mod, gj_mod, poisson, &
        & dint, dout, plas_curve_const, &
        & plas_curve_vary, isize, ierr)
      if( ierr /= 0 )then
        call error_handling(iscrn, iout, 253, &
          & "Error: Unable to allocate sufficient memory for plasticity element section data.")
      end if
      total_mem_aloc = total_mem_aloc + isize
    end if

    ! Allocate output arrays.
    call allocate_plas_output_data(ndtbel, idbcon(db_plas_strain), idbcon(db_eqv_plas_strain), ierr, isize)
    if( ierr /= 0 )then
      call error_handling(iscrn, iout, 253, &
        & "Error: Unable to allocate sufficient memory for plasticity output data.")
    end if
    total_mem_aloc = total_mem_aloc + isize
  end if

  ! Output memory usage.
  call output_memory_data


  ! Fill in nodal diameter array.
  ! This is mainly used with PIP, but could be used in general, so it is decided to make this available.
  call fill_pip_diameter(numel, dint, dcntct, size_epi, nonp, epi, pip_diameter)
  
  !  update the wake drag coefficients, if this is a repeat analysis
  if( wki_skip )then
    call get_downstream_data
  end if

  if( irstrt == 0 )then
    xlen(:)  = xleno(:)
    xlenp(:) = xleno(:)
  end if

  ! Initialise shape functions N(x), plus integration point locations & weights 
  call initialise_shape_function_data(idntp, xlen, fc, fw, fb, t, t2)

  do n = 1, numel
    is_nl_flexjoint = fj_status(n) > 0
    
    if( idntp(n) == 1 .and. idnlmt(n) == 0 .and. &
      (kstiff(n) == 0 .or. kstiffnl(n) == 0) .and. &
      .not.(is_nl_flexjoint) ) then
      call assemble_k_linear(k_linear(1:n1,1:n1,n), kstiff(n), data1(n), data2(n), &
        data4(n), data5(n), xlen(n), imod_rst, bndstf)
    end if
  end do
  
  ! floating body analysis - open additional files for storage
  ! of retardation functions and body velocities
  if( fb_anal == 1 )then

    ! allocate unit numbers for individual floating bodies
    ! and floating body couples.
    iunit = 100
    do ifloat = 1, num_fbset
      if( nraddampfq(ifloat) > 1 )then
        ireta(ifloat) = iunit
        iretb(ifloat) = iunit + 1
        ivel(ifloat)  = iunit + 2
        iunit = iunit + 3
      end if
    end do
    do icouple = 1, ncouple
      if( ncouplingfq(icouple,2) > 1 )then
        do idir = 1,2
          ireta_cpl(icouple,idir) = iunit
          iretb_cpl(icouple,idir) = iunit + 1
          iunit = iunit + 2
          ifloat = coupling_bodies(icouple,idir)
          if( ivel(ifloat) == 0 )then
            ivel(ifloat) = iunit
            iunit = iunit + 1
          end if
        end do
      end if
    end do

    ! dynamic analysis
    if( nodyn /= 1 )then

      ! individual floating body retardation functions
      do ifbset = 1, num_fbset

        ! ireta ("ascii") file stores retardation functions for user scrutiny
        if( ireta(ifbset) /= 0 )then
          call number_to_character(ifbset,ext1)
          retard_filename = "ret_fn_" // trim(ext1) // ".dat"
          open(unit=ireta(ifbset), file=trim(retard_filename),status="unknown")
        end if

        ! iretb ("binary") file stores retardation functions at each time step
        if( iretb(ifbset) /= 0 )then
          open(unit=iretb(ifbset), status="scratch",access="direct", &
            form="unformatted", recl=file_recl(296))
        end if

        ! vel file stores cog velocity at each time step
        if( ivel(ifbset) /= 0 )then
          open(unit=ivel(ifbset), status="scratch", access="direct", &
            form="unformatted", recl=file_recl(56))
        end if
      end do

      ! coupled floating body retardation functions
      do icouple = 1, ncouple
        if( ncouplingfq(icouple,2) > 1 )then

          ! retrieve floating body number
          ifloat1 = coupling_bodies(icouple,1)
          ifloat2 = coupling_bodies(icouple,2)

          ! loop over directions
          do idir = 1,2

            ! create filename
            if( idir == 1 )then
              call number_to_character(ifloat1,ext1)
              call number_to_character(ifloat2,ext2)
            else
              call number_to_character(ifloat2,ext1)
              call number_to_character(ifloat1,ext2)
            end if
            retard_filename = "ret_fn_" // trim(ext1) //"_" // trim(ext2) // ".dat"

            ! ireta ("ascii") file stores retardation functions for user scrutiny
            open(unit=ireta_cpl(icouple,idir),file=trim(retard_filename), status="unknown")

            ! iretb ("binary") file stores retardation functions at each time step
            open(unit=iretb_cpl(icouple,idir), status="scratch",access="direct",  &
              form="unformatted",recl=file_recl(296))

          end do
        end if
      end do
    end if
  end if

  ! convert the stress-strain curves for the non-linear bend stiffeners
  ! to moment-curvatures for each element with non-linear stiffener
  icurve = nsprtb - nnlbsel
  if(nnlbs > 0)then
    do i = 1,numel                ! loop over elements
      idnlmtbs(i) = 0
      if(kstiffnl(i) == 1)then    ! check for nl bend stiffener
        
        ! Set flag if a non-linear bend stiffener element is also 
        ! made of nonlinear material
        if(kstiff(i) /= 0 .and. idnlmt(i) /= 0 .and. fj_status(i) == 0)then
          nl_mat_nl_bnstf_ck = .true.
        end if
        
        do j = 1,nbnstf           ! loop over bend stiffeners
          ielem = nint(bndstf(j,4))
          if(ielem == i)then      ! found the element

            do1  = bsdiam(ielem,1)    ! start diameter
            do2  = bsdiam(ielem,2)    ! end diameter
            di   = bsdiam(ielem,3)    ! internal diameter
            deq  = (do1 + do2) / 2.d0
            rout = deq / 2.0
            rin  = di / 2.0
            x    = rout / 100.0    ! split 1/4 circle into 100 sections

            icurve = icurve + 1
            idnlmtbs(ielem) = icurve - 1
            numspp(icurve,1) = 41
            hawst(icurve,2,1,1) = 0.d0 ! put an m-k point at the origin
            hawst(icurve,2,1,2) = 0.d0

            do k = 1,40
              xk = k * 0.05
              xmoment = 0.0
              do l = 1,100         ! loop over 100 sections
                arm      = (l-0.5) * x
                strain   = xk * arm
                stress   = get_stress_from_strain(ielem,strain,bsmomcur,npmomcur,imomcur)
                sectarea = x * (rout**2.0 - arm**2.0)**0.5
                if(arm < rin)then
                  sectarea = sectarea - x*(rin**2.0 - arm**2.0)**0.5
                end if
                xmoment = xmoment + sectarea * arm * stress
              end do
              xmoment = 4.0 * xmoment   ! full circle
              hawst(icurve,k+2,1,1) = xk
              hawst(icurve,k+2,1,2) = xmoment
            end do
          end if
        end do
      end if
    end do
  end if

  ! determine offset to be applied to vessels in this analysis, if
  ! appropriate (revised v51 rao format)
  if (irand > 0 .or. irnd2reg == 1) then
    call vessel_offset(mxraos,ndeg,nvset,dspcg_lf,offset,voffset,voffini)   
  end if

  ! determine start time for calculation of runtime statistics
  ! (dynamic analyses only)
  if (nodyn /= 1.and.istatc == 0) then
    if (statstm <= 1.d-10) then
      if (irstrt < 2) then
        statstm = tramp
      else if (irstrt == 2) then
        statstm = tramp + tfprev
      end if
    end if
    if (statstm < tstart) statstm = tstart
  end if

  ! find the global to local undeformed transformation matrix for each
  ! element
  call assemble_tglu_matrix(plndr,tglu,idntp)

  ! set up initial rotations for non-restart runs
  if(irstrt == 0) then
    itorq  = 0
    call rigid_body_euler_angles(trgb,dd0,rtct0,cord,npi,idntp,tglu,itorq)      
    do i = 1,nonp

    ! check is this an active node
      !if(active_node(i,2) /= 1)cycle
              
      do j = 1,3
        jj = j + 3
        dd0(jj,i) = rtct0(j,i)
      end do
    end do
  end if
  
  ! initialize time variables
  if(ialdb == 0)timed = stdb
  time = tstart + tstep
  timep = tstprt

  ! read boundary conditions
  call read_all_bcs(npi,npbc,tq,tdst,tf,press,ndvar,idatn,spress,offbcn,nume,npnum,lsub,natch, &
    ptboye,sindata,ibcfil,ibcref,nbcfil,nbcref,bcnffl,bcnfrf,statof,num_seas,dd0)


  call write_XML_tag(iout, 3, "Open", "End of Input Data Echo")
  write(iout,500)
  call write_XML_tag(iout, 3, "close") 
  call write_XML_tag(iout, 2, "Close")


  ! convert user boundary condition files and vessel timetrace files
  ! from ascii format to direct access format, if appropriate
  if (ndfile > 0 .or. ndrefc > 0 .or. ndrift_files > 0 .or. nvesttr > 0) then
    call convert_file(bcnffl,bcnfrf,nbcfil,tmpffl,tmpfrf,ivdrift,drift_file,ivesttr, &
      vesttr_file,tmpves)
  end if

  ! find initial position of reference point(s) in user boundary
  ! condition files, if appropriate
  if (ndrefc > 0) then
    call find_init_ref_pos(tmpfrf,refip)
  end if

  ! initialise hysteresis data
  if(is_hysteresis_in_this_model )then
    call initialise_hysteresis_data(num_int,ngens,nodyn,irstrt,numel,nsprtb,mxsptv,idnlmt, &
      iei_flag,numspp,hawst)
    call initialise_hysteresis_output_data(irstrt)

    ! Alpha_f is zero if the previous analysis did NOT have hysteresis and
    ! the present analysis has hysteresis.
    ! This applies to the dynamic procedure only.
    if( nodyn /= 1 )then
      if( is_hysteresis_in_this_analysis .and. &
          .not. was_hysteresis_in_previous_analysis )then
        ! means re = 0.d0, need to disable alpha_f for first pass.
        alpha_f = 0.d0
      end if
    end if
  end if

  ! initialise seabed axes system and seabed origin
  if(irigsf /= 0)then
    call init_seabed_axes(iout)
  end if
  
  ! initialise guide surface node map
  call gnd_init_node_map(nonp, npnum)
  call gvd_init_node_map(nonp, npnum)

  ! initialise contact arrays, if this is not a restart analysis
  if(irstrt == 0) then
    
    ! initialise rigid seabed contact arrays
    call init_seabed_contact(ndeg,nonp,nart,ncord,nnode,numel,jel,idsbn0,idsbe0,imvrg0, &
        indic,npi,npbc,cord, iscrn, iout)

    ! initialise guide contact arrays
    call gvd_init_surf_cont()
    call gnd_init_surf_cont()

    ! initialise zero-gap guide contact arrays
    call initialise_zgg_data(ncord,ndeg,nonp,nnode,numel,nart,jel,npi,cord,npbc,npnum)    
  
  end if  
  
  ! initialise point buoy axis array, if appropriate
  if( nptboy > 0 ) then     
    call initialise_point_buoy_axes
  end if
  
  ! Check if any relative sliding of pipe-in-pipe sections has occurred
  ! This call is before the cable solution with bending and torque
  if( npipnod > 0 )then
    call check_sliding_pip_connections(nonp, ndeg, nnode, numel, npnum, dd0, npi, &
      .false., section_limit_exceeded(1))
    call check_outer_pip_elem(nonp, ndeg, nnode, numel, dd0, npi)
  end if
  if( npipsect > 0 )then
    call update_inner_pip_outer_elements(section_limit_exceeded(2))
    ! call update_inner_pip_outer_nodes(section_limit_exceeded(3))
    call update_inner_pip_outer_nodes(section_limit_exceeded(3), npipsect,  &
	& mxpipsect, pip_section, nonp, pip_inner_node, npipnod, mxpipnod,  &
	& pip_nodes, nested_section_limit, npnum, nndset, ndset_nod, ndset_defn,  &
	& ndset_name)
  end if

  if(warn_section_limit_exceeded == .false. .and. any(section_limit_exceeded)) then
    warn_section_limit_exceeded = .true.
    call warning(iout, "Warning - Nested section limit exceeded while searching for&
      & a sliding pipe-in-pipe connection. Hydrodynamic coupling has not been updated.")
  end if

    
  ! Update connectivity and possibly perform bandwidth optimisation 
  ! following relative sliding motion of pipe-in-pipe sections
  if( update_pip_connectivity )then   
    nad_temp = nad
    if( pip_bandwidth .and. (.not.noband) )then
      call bandwidth_optimisation(jdiag,labeq,idact,npi,idntp,iartnd,norder,iorder,istore)
    else
      call profile_setup(jdiag,labeq,idact,npi,idntp,iartnd,norder,rmsprf,imod_rst,npbc)    
    end if
    if( nad > nad_temp )then
      call deallocate_profile_arrays
      call reallocate_profile_arrays
    end if
    update_pip_connectivity = .false.
  end if

  ! Set up ramp factors for various load terms before cable solution
  call setup_ramp_variables(.true.)

  ! subroutine to compute from cable statics
  if (icaban == 1) then
    call cable_solution(sxx,npi,ss,sk,d,b,e,cord,data1,data2,th,data3,iti,dicos,tdicos,     &
      td,dd,tx,sigxx,disp,dispt,disp1,gr,nharz,depth,rhow,dragd,dint,dout,npbc,dd4,jel,stiff, &
      tq,data4,data5,nhrznd,iartnd,axpdat(iap1),xlen,icab,locm,admas,dd0,axpdat(iap3),hawst,  &
      numspp,idnlmt,sprfrc,idntp,spress,press,dsflcl,idsbe,idsbn,jdiag,idact,labeq,vlhsu,     &
      vlhsl,vrhs,trqdat(itq1),trqdat(itq3),defloc,rigvec,outp,tglu,trgb,rotact,rtct0,imvrg0,  &
      imvrg,athet,gdef,gdefs,reacsb,sbdres,tv,vcur,locdg,dgch,locs,xndspr,fluid,alphat,temp,  &
      npnum,nume,ptboye,iptby,tglsb,trgsf,seabed_axis,tvs,bndstf,kstiff,idisby,idsbe0,idsbn0, &
      rhs,rhs0,reacs0,idnlmtbs,kstiffnl,pslug,islug,islug_fill,disp2,vmass,sxxt,iei_flag,    &
      damper,cdamp,dcntct,rotk,raova,phasa)
  end if
      
  ! now initialise the tdaux array as having the same values
  ! as the initial coordinates of the auxiliary nodes.
  call initialise_auxiliary_displacements(acord,tdaux)

  ! floating body analysis - compute retardation functions and added mass
  if( fb_anal == 1 .and. nodyn /= 1 )then

    ! individual floating body retardation functions
    do ifbset = 1, num_fbset
      if( nraddampfq(ifbset) > 1 )then

        ! output message
        if( .not.(run_from_gui) )then
          call setcur(21,4)
          write(iscrn, fmt="(73(' '))")
          call setcur(21,4)
        end if
        write(iscrn, fmt="(3A)")"Computing retardation functions for ", &
          trim(fb_set(ifbset)), " ..." 
         
        ! compute retardation functions
        call compute_ind_fb_retard_fns(ifbset)
      end if

      ! compute added mass
      call get_ind_fb_added_mass(ifbset)
    end do

    ! coupled floating body retardation functions
    do icouple = 1, ncouple
      if( ncouplingfq(icouple,2) > 1 )then

        ! output message
        if( .not.(run_from_gui) )then
          call setcur(21,4)
          write(iscrn, fmt="(73(' '))")
          call setcur(21,4)
        end if
        ifloat1 = coupling_bodies(icouple,1)
        ifloat2 = coupling_bodies(icouple,2)
        write(iscrn, fmt="(5A)")"Computing retardation functions for couple ", &
          trim(fb_set(ifloat1)), " and ", trim(fb_set(ifloat2))," ..."

        ! compute retardation functions
        call compute_cpl_fb_retard_fns(icouple)
      end if

      ! compute added mass
      call get_cpl_fb_added_mass(icouple)
    end do

  end if

  ! output .cof file for output of drag and inertia coefficents
  ! (1) hydrodynamic coefficients are specified as a function of Reynolds' number
  ! (2) instantaneous Reynolds' number computation is invoked
  do j=1,nprint
    if( ireyn == 1 .and. inst_reyn .and. print_data(j,1) == Reynolds_COF )then
    open(unit=icof,status='unknown',file=filcof)
    end if
  end do
  
  ! write out header block for user requested clashing details
  call clash_output_header

  ! compute the viv drag amplification factors.
  if( viv_drag_nelem > 0 .and. viv_drag_npoints > 0 )then
    call calc_drag_amplific_factors
    
    ! apply the factors if this not a wake interference analysis
    ! or is a wake interference model
    ! and the respective element has constant reynolds no. hydrodynamic properties.
    ! make sure to treat the other call to apply_viv_drag, in the same manner.
    if( wki_model /= wki_user )then
      drag_apply = .true.
    else
      do i = 1,wki_nset(1)
        if( trim(wki_set_name(i,1)) .cic. trim(viv_drag_set_name) )then
          drag_apply = .true.
        end if
      end do
    end if
    if( drag_apply )then    
      do i = 1, numel
        if( nore(1,i) == 1 )then
          do j = 1, num_int
            call apply_viv_drag(i, j, cdnn(1,j,i))
          end do
        end if
      end do
    end if
  end if

  ! For a wake analysis get upstream position data.     
  if( wki_model /= wki_off )then
    call get_upstream_data
    call allocate_wake_output_array
  end if

  ! Initialise boundary condition arrays.
  call initialise_bcs(npbc,ires,react,bound,w0,tdst,rtct0,dd0,cord,itim,active_bcs_node)

  ! Additional output requested under *PRINT - time independent data
  call star_print_output(depth,gr,rhow,time,nume,trgb,tglu,omega,wvamp,wvrn,dir,wvnum,.true., &
    islug,islug_fill,pslug,centrifugal_nodal, idntp, .false.)
  
  ! function to read in the Slug time histories for Slugs with varying properties
  if( maxval(pslug(7,:)) >smallnum ) then
    
    ! Populate array with timetrace data and calculate second derivativesnum_int
   call assign_slug_timetrace_data(Num_columns)
  end if
  
  
  
  ! Time History wave
  if(is_wave_timehist) then
    ! Brief description of how time history wave data is handled from Flexcom V811.
    ! In case of wave time history ,what we are trying to do is divide the whole input wave time history data 
    ! into separate smaller time histories (instead of considering the whole lot at one go as was done upto V797)
    ! depending on the num of ensembles specified.If num of ensem specified is x, the input time history is divided 
    ! into x separate small time histories and these time histories are converted to frequency domain and 
    ! the regular wave parameters are updated x times. 
    ! The num of harmonics for each divided time history remain same.
    
    ! First call to convert wave time hist data to reg wave harmonics.The start time  of first block  time history is the analysis 
    ! start time  and the end time is the wave time history elapsed time(wth_elapstime)

    ! The smaller time history data is transformed to freq domain and regular waves are generated for this time history data.
    call process_timehistory_data(depth,gr,time, wth_elapstime,tfinsh) 
    ! Time history generated regular wave is converted to circular freq.
    call convrt_waveperiod_cirfreq(nharz,dd0,depth,npi,jel,nhrznd,xleno,damper, gr,wth_elapstime,time, first_wth_print )
  end if  

  ! Set dbm and dbf GUID here
  call getGUID(iresult, GUIDdata)
  if( iresult /= 0 )then
    call error_handling(iscrn, iout, 503,"Error occured when creating GUID")
  else
    loadcase_GUID = GUIDdata
  end if

  ! Prepare the databases, if database output was requested.
  ! Calculate parameters/dimensions and write header.
  if( idatb /= 0 .and. .not.final_static_required )then
    call prepare_database(ieldtb,inodtb,npi,depth,gr,rhow,xlen,sprop,icolor,nume, &
      npnum,cord,ires,fluid,iaxfl,data1,acord,iaelem,ielaux,nodea,npia,num_seas,dcntct, &
      locm,npbc,vcur,stinger_plane)
  end if
  
  ! Retrieve timetrace start time
  start_ttr    = time_trace_data(1)
  end_ttr      = time_trace_data(2)
  interval_ttr = time_trace_data(3)

  ! Set the valid analyses options when change-in-length adjustment is permitted when synthetic rope is present
  !if( nsynrpset > 0 )then
    
  static_anal  = nodyn == 1 .and. imstc == 0
  qstatic_anal = nodyn == 0 .and. istatc == 1
  not_mooring_anal = moor == 0
  length_adjustment_permitted = ( static_anal .or. qstatic_anal ) .and. not_mooring_anal
  
  
  ! Start of time loop for synthetic rope elements
  ! The concept here is that at end of time_loop, there are length adjustments for
  ! all the synthetic rope elements due to delayed elastic stretch or recovery.
  ! Thus, after the length adjustments, it is necessary to perfrom all the tasks
  ! in the time_loop to update the system configuration
  !
  ! The last step is performed only when the static analysis following adjustment is done.
  ! Initialise to false for now.
  do_synt_rope_loop = nsynrpset > 0 .and. length_adjustment_permitted
  perform_last_step = .not.do_synt_rope_loop
  tstep_user = tstep
  
  if( nodyn /= 1 )then
    call set_token_threshold_time(ndimwv,time,tramp,threshold_period,omega)
  end if
  
  synt_rope_loop: do
    ! Count how many times the synt_rope_loop has been executed
    itim_synrp = itim_synrp + 1
    elmrst_prev = elmrst

    ! Start of time loop.
    time_loop: do

#ifdef __edu
      ! Restrict total time to 1800sec for the educational version
      ! Now check the actual time
      if( time > 1800.d0 )then
        call error_handling(iscrn, iout, 237,"Your licence does not support " // & 
          "a simulation time greater than 1800 seconds.")
      end if
#endif

      ! Check if this is the last time step of the analysis.
      finished_time_loop = time >= tfinsh - 1.d-2 * tstep
      last_tstep = finished_time_loop .and. perform_last_step
      
      if( nodyn /= 1 )then
        if( time > (tramp + threshold_period) ) successful_token_analysis = .true.
      end if

      ! Set up ramp factors for various load terms at each new time step
      call setup_ramp_variables(.false.)

      ! write out velocity and position timetrace of reference node if this is a floating body analysis
      if(fb_anal == 1 .and. nodyn /= 1 )then
        do ifbset = 1, num_fbset
          if( ivel(ifbset) /= 0 )then
            do i = 1,6
              velglo(i) = tv(i,fb_refnode(ifbset))
            end do
            call get_fb_xltg_6x6(ifbset,nonp,rtct0,xltg)
            call matrix_transpose(xltg,xltgt,6,6)
            !call matrix_product(xltgt,velglo,velloc,6,6,1)
            velloc(1:6) = matmul(xltgt(1:6,1:6),velglo(1:6))
            iaddr_vel(ifbset) = iaddr_vel(ifbset) + 1
            nrecs_vel(ifbset) = nrecs_vel(ifbset) + 1
            write(ivel(ifbset),rec=iaddr_vel(ifbset))time,(velloc(i),i=1,6)     
          end if
        end do
      end if
    

      ! need to activate element data for inactive elements
      !call activate_element_data(numel,nonp,nnode,ncord,ndeg,
      !&nodyn,npi,cord,cordo,xlen,dd0,time,tstep)

      ! set up profile scheme
      !call profil(jdiag,labeq,idact,npi,idntp,iartnd,norder,rmsprf,
      !&  imod_rst,npbc)

      ! update bcs to take the newly activated elements into consideration
      !call update_active_bcs(ndeg,nonp,natnd,mxraos,nvset,natch,
      !1npbc,idatn,irstrt) 
    
      ! Initialise boundary condition arrays.
      ! This has to be called here because of PipeLay enhancements.
      call initialise_bcs(npbc,ires,react,bound,w0,tdst,rtct0,dd0,cord,itim,active_bcs_node)

      ! check for clashing
      if( num_clash_regs > 0 ) then
        call check_clashing(time, nume, npi, td, cord, dcntct, npnum, tv)     
      endif
    
      ! update the finite element mesh connectivity if required
      if( update_clash_connectivity )then
        nad_temp = nad
        call profile_setup(jdiag,labeq,idact,npi,idntp,iartnd,norder,rmsprf,imod_rst,npbc)    
        if( nad > nad_temp )then
          call deallocate_profile_arrays
          call reallocate_profile_arrays
        end if
        update_clash_connectivity = .false.
      end if

      ! model adjustments to iterate on installation criteria
      if( analysis_stage == criteria_static .and. crit_iter > 0 )then

        ! non-convergence caused by criteria adjustments
        if( crit_not_conv  )then

          ! try reducing the offset
          crit_inc_adjust = crit_inc_adjust / 2.d0
          crit_abs_adjust = crit_abs_adjust - crit_inc_adjust
          crit_adj_x2     = .false.

        end if

        ! adjust model in an attempt to satisfy installation criteria
        call adjust_model(ndeg,nonp,tdst,mxraos,voffset,numel,nume,xlen,depth)

        ! reset the non-convergence flag
        if( crit_not_conv  )crit_not_conv = .false.

      end if

      ! Check if any relative sliding of pipe-in-pipe sections has occurred
      ! This call is performed after every solution step
      if( npipnod > 0 )then
        call check_sliding_pip_connections(nonp, ndeg, nnode, numel, npnum, dd0, npi, &
          .true., section_limit_exceeded(1))
        call check_outer_pip_elem(nonp, ndeg, nnode, numel, dd0, npi)
      end if
      if( npipsect > 0 )then
        call update_inner_pip_outer_elements(section_limit_exceeded(2))
        ! If pip connectivity has been updated then inner connections needs to be updated as well
        if(update_pip_connectivity) then
          ! call update_inner_pip_outer_nodes(section_limit_exceeded(3))
          call update_inner_pip_outer_nodes(section_limit_exceeded(3),  &
	& npipsect, mxpipsect, pip_section, nonp, pip_inner_node, npipnod,  &
	& mxpipnod, pip_nodes, nested_section_limit, npnum, nndset, ndset_nod,  &
	& ndset_defn, ndset_name)
        end if
      end if

      if(warn_section_limit_exceeded == .false. .and. any(section_limit_exceeded)) then
        warn_section_limit_exceeded = .true.
        call warning(iout, "Warning - Nested section limit exceeded while searching for&
          & a sliding pipe-in-pipe connection. Hydrodynamic coupling has not been updated.")
      end if

    
      ! Update connectivity and possibly perform bandwidth optimisation 
      ! following relative sliding motion of pipe-in-pipe sections
      if( update_pip_connectivity )then   
        nad_temp = nad
        if( pip_bandwidth .and. (.not.noband) )then
          call bandwidth_optimisation(jdiag,labeq,idact,npi,idntp,iartnd,norder,iorder,istore)
        else
          call profile_setup(jdiag,labeq,idact,npi,idntp,iartnd,norder,rmsprf,imod_rst,npbc)    
        end if
        if( nad > nad_temp )then
          call deallocate_profile_arrays
          call reallocate_profile_arrays
        end if
        update_pip_connectivity = .false.
      end if

      ! calculate element lengths for winching
      if( nwinch > 0 )then
        call winch_elem_len(time,tfinsh,xlen,xlenp,numel,nwinch,winch,numwin,nodyn,iscrn,iout)
      
        !Update the stiffness matrix for just winch elements (length changed)
        do i = 1, nwinch
          n = numwin(i)
          ! Only update the matrix entries associated with winch elements.
          is_nl_flexjoint = fj_status(n) > 0
        
          if( idntp(n) == 1 .and. idnlmt(n) == 0 .and. &
            (kstiff(n) == 0 .or. kstiffnl(n) == 0) .and. &
            .not.(is_nl_flexjoint) ) then
            call assemble_k_linear(k_linear(1:n1,1:n1,n), kstiff(n), data1(n), data2(n), &
              data4(n), data5(n), xlen(n), imod_rst, bndstf)
          end if
        
          ! Update the integration points and shape functions for winch elements.
          if( idntp(n) == 1 )then
            call update_integration_points(num_int, xlen(n), fc(1:maxint,n), fw(1:maxint,n), fb(1:maxint+1,n))
            call update_shape_functions(num_int, xlen(n), fc(1:maxint,n), &
              t(1:ngens,1:n1,1:maxint,n), t2(1:ngens,1:n1,1:maxint,n))
          end if
        end do
          
      end if
    
      ! Set control variables for cd = f(re)
      if (ireyn == 1) then
        call set_reynolds_control(vrelcon,vrelc,vrelp)
      end if

      ! Check security of pc version. this is done at random
      ! time intervals (between 150s and 450s of real time).
      elapsed = dble(timef())
      if( elapsed >= checktime )then
        call random_number(rnum)
        checktime = 150.d0 + dble(nint(rnum * 300.e0)) + elapsed
        call check_dongle
        call check_flexcom_dynamic_license
      end if

      ! calculate ramp to be applied to added pipeline length
      !pipe_ramp = 1.d0
      !if(nodyn /= 1 .and. additional_pipe_length > smallnum)then
      !  call calc_active_element_ramp(time,pipe_ramp)
      !end if      

      ! raos applied to structure, if appropriate
      if(moor == 0)then
        ! ************************************************************************
        ! loop over number of vessels
        do ivset = 1,nvset
          if((irand /= 0.or.irnd2reg == 1).and..not.option_global)then
            if(btest(ismall, ivset - 1) == .false.)then
              call vessel_response_large(mxraos,ndeg,ivset,nfreq,ndeg1,nwvhd,gvd_num_surf,nmpool,nodyn, &
                ifbc2,iscrn,iout,time,tstep,isea(1),ndimwv,  &
                raova,phasa,ifairy,eomega,offset,voffset,voffini,ivdrift,ves_drift,tmpves,nfrqhd,&
                rao,ivesttr,veccg0,dspcg_lf,dspcg,dspcgd,dspcgdd,trcgd,trcgdd,irnd2reg,          &
                vessel_velocity,vessel_axis)

              ! check if the *regular wave equivalent keyword has been specified
              ! if so, do not apply bcs to the attached nodes
              if(irnd2reg /= 1)then
                call apply_vessel_bcs_large(mxraos,ndeg,ncord,nonp,ivset,natnd,nanode,offset,       &
                  veccg0,dspcg,natch,idatn,cord,offbcn,npbc,nbaux,acord,td,tdaux,icog,moor,rotact,  &
                  dd0,rtct0,pipe_ramp,nodyn)
              end if
            else
              call vessel_response_small(mxraos,ndeg,ivset,nfreq,ndeg1,nwvhd,gvd_num_surf,nmpool,nodyn,  &
                ifbc2,iscrn,iout,time,tstep,isea(1),nhh(1),   &
                raova,phasa,ifairy,eomega,offset,voffset,voffini,ivdrift,ves_drift,tmpves,nfrqhd, &
                rao,ivesttr,veccg0,dspcg_lf,dspcg,irnd2reg,vessel_velocity,vessel_axis)

              ! check if the *regular wave equivalent keyword has been specified
              ! if so, do not apply bcs to the attached nodes
              if(irnd2reg /= 1)then
                call apply_vessel_bcs_small(mxraos,ndeg,ncord,nonp,ivset,natnd,nanode,veccg0,dspcg, &
                  natch,idatn,cord,offbcn,npbc,nbaux,acord,td,tdaux,icog,moor,rotact,dd0,rtct0,     &
                  pipe_ramp,nodyn)
              end if
            end if
          else if((irand /= 0.or.irnd2reg == 1).and.option_global)then
            call vessel_response_global(mxraos,ndeg,ivset, &
              ndimwv,raova,phasa,offset,voffset,voffini,dspcg_lf,dspcg,dspcgd,dspcgdd,trcgd,trcgdd)

            ! check if the *regular wave equivalent keyword has been specified
            ! if so, do not apply bcs to the attached nodes
            if(irnd2reg /= 1)then
              call apply_vessel_bcs_large(mxraos,ndeg,ncord,nonp,ivset,natnd,nanode,offset,veccg0,&
                dspcg,natch,idatn,cord,offbcn,npbc,nbaux,acord,td,tdaux,icog,moor,rotact,dd0,     &
                rtct0,pipe_ramp,nodyn)
            end if
          end if
        end do
      end if

      ! if this is a static mooring analysis, store the last value of the
      ! vessel forces and the centre of gravity node displacement for use
      ! in determining the mooring system stiffness. this approach only
      ! works if a ramp has been specified.
      if (imstc == 1) then
        tdcogp(1) = td(2,icog)
        tdcogp(2) = td(3,icog)
        fvespr(1) = fves(1)
        fvespr(2) = fves(2)
      end if

      ! subroutine to call user subroutine in loop over type=subroutine displacements
      if( ndvar > 0 )then
        call vary_displacement(ndvar,npbc,npnum,td,ndeg,nonp,time,stan_ramp)
      end if

      ! find direct nodal displacements from user boundary condition files
      if (ndfile > 0) then
        call read_bc_disp_file(nbcfil,tmpffl,time,stan_ramp,ibcfil,npbc,td,npnum,tdst,rotact,dd0,rtct0,&
          irbcfl)
      end if

      ! find reference point displacements from user boundary condition files
      if (ndrefc > 0) then
        call read_bc_ref_file(nbcref,tmpfrf,refip,stan_ramp,ibcref,npbc,td,cord,statof,rotact,dd0, &
          rtct0,irbcrf)
      end if

      ! update boundary value array
      call start_bcs(npbc,ires,bound,w0,tdst,td,rotact,cord,stan_ramp,pipe_ramp)

      ! add sinusoidal boundary conditions
      call sinsoidal_bcs(npbc,td,rotact,dd0,rtct0,sindata,stan_ramp)

      ! set boundary conditions at auxiliary nodes if *regular wave equivalent present
      if(irnd2reg == 1.and.nodyn == 0)then
        call auxiliary_bcs(acord,tdaux,auxsin,stan_ramp,dspcg_lf,nbaux,veccg0,nauxsin,ndimauxsin, &
          voffset)
      end if

      ! find response of guide contact surfaces from vessel motions, if
      ! appropriate
      call gvd_guide_response(offset,dspcg,veccg0)

      ! find response of zero-gap guides from vessel motions, if appropriate
      if (nzgg > 1) then
        call zgg_response(ndeg,mxraos,offset,dspcg,veccg0)
      end if

      ! find response of vessel moonpools from vessel motions, if
      ! appropriate
      if (nmpool > 1) then
        call moonpool_response(imnpl_ves,offset,dspcg,veccg0,cord_mnpl,ax_mnpl,td_mnpl,tdax_mnpl)                        
      end if

      ! slug calculations
      if(nmslug > 1)call slug_flow(pslug,npi,dist,xlen,islug,islug_fill,nume,idntp)

      if( iincr /= -1 )then
        if( nodyn /= 1 )then
          ! Calculate the RE array used in this step.
          call calculate_re(re, npi, dd0, dd4, axpdat(iap3), trqdat(itq3), stiff, alpha_f)
        end if
      
        ! Scale the penalty terms if this a restart analysis.
        ! Do this once only.
        if( itim == 1 .and. irstrt /= 0 )then
          do ielem = 1, size_axpen
            ! axpn0, axpv0, axpa0
            axpdat(iap3-1+ielem) = rhop  * axpdat(iap3-1+ielem) / rhopo
            axpdat(iap6-1+ielem) = rhop  * axpdat(iap6-1+ielem) / rhopo
            axpdat(iap7-1+ielem) = rhop  * axpdat(iap7-1+ielem) / rhopo
            if( ielem > numel )cycle
            ! tqpn0, tqpv0, tqpa0
            trqdat(itq3-1+ielem) = rhptq * trqdat(itq3-1+ielem) / rhptqo
            trqdat(itq6-1+ielem) = rhptq * trqdat(itq6-1+ielem) / rhptqo
            trqdat(itq7-1+ielem) = rhptq * trqdat(itq7-1+ielem) / rhptqo
          end do
        end if
      end if

      ! Check for hysteresis, if hysteresis is included in the model
      ! then a pre hysteresis analysis step is required for each time step.
      if(is_hysteresis_in_this_model  )then
        pre_hysteresis_step             = .true.
        hysteresis_step_convergence     = .false.
        pre_hysteresis_step_convergence = .false.
      end if

      ! Call main computational routine static_analysis/dynamik

      if(nodyn == 1)then

        ! Increment static step counter unless step size was reduced at
        ! the last solution step
        if (iincr /= -1) istat_step = istat_step + 1

        call static_analysis(sxx,npi,ss,sk,d,b,e,cord,data1,data2,th,data3,gs,iti,dicos,tdicos,td, &
          dd,dd0,dd1,dd2,dd3,tv,ta,tvs,tx,dd5,ex,sigxx,disp,dispt,disp1,gr,nharz,depth,rhow,      &
          dragd,dint,dout,vcur,cdnn,cdtt,cmnn,cmtt,reno,nore,npbc,ts,dd4,locm,admas,jel,outp,     &
          react,ires,stiff,vmass,dsflcl,elmrst,tq,press,spress,data4,data5,nhrznd,iartnd,         &
          axpdat(iap1),axpdat(iap2),axpdat(iap3),axpdat(iap4),axpdat(iap5),xlen,icab,dreact,      &
          hawst,numspp,idnlmt,sprfrc,idntp,reacsb,sbdres,idsbe,idsbn,reacs0,idsbe0,idsbn0,jdiag,  &
          idact,labeq,vlhsu,vlhsl,vrhs,rfaca,irdof,trqdat(itq1),trqdat(itq2),trqdat(itq3),        &
          trqdat(itq4),trqdat(itq5),defloc,rigvec,ieldfx,rotact,tglu,trgb,rtct0,rtctg,imvrg,      &
          imvrg0,athet,gdef,gdefs,dgch,locdg,npnum,locs,xndspr,ptld,alllod,fluid,lsub,temp,       &
          alphat,nume,tqold,tqnew,kstiff,bndstf,rhs,rhs0,isbelm,vrelp,vrelc,ptboye,iptby,         &
          tglsb,trgsf,seabed_axis,sxxt,idisby,srestf,erestf,veccg0,dspcg,dspcgd,dspcgdd,trcgd,trcgdd,&
          deffec,implhyd,td_mnpl,tdax_mnpl,dim_mnpl,imnpl_ves,istat_step,idnlmtbs,kstiffnl,bsdiam,&
          indic,disp2,vadmas,vedam,wcoef,cuc,qtf,thrust,cmomnt,pslug,islug,islug_fill,num_seas,   &
          calmdata,renotab,iei_flag,damper,cdamp,curvyt,curvzt,seabed_tol,dcntct,rotk,norder,     &
          stiff_modes,vmass_modes,cann,calmmass,raova,phasa,iaxfl,sprop,voffset,offset,      &
          stinger_plane,tdaux,npia,acord,last_tstep,timep)

      else

        ! Hilber - Hughes time scheme used for dynamic solution
        call dynamic_analysis(sxx,npi,ss,sk,d,b,e,cord,data1,data2,th,data3,gs,iti,dicos,tdicos, &
          td,dd,dd0,dd1,dd2,dd3,dd31,tv,ta,tvs,tx,dd5,ex,sigxx,disp,dispt,disp1,disp2,gr,  &
          nharz,depth,rhow,dragd,dint,dout,vcur,cdnn,cdtt,cmnn,cann,cmtt,reno,nore,re,tf,npbc,  &
          ts0,ts,dd4,locm,admas,jel,outp,react,ires,stiff,vmass,dsflcl,elmrst,tq,press,spress,  &
          data4,data5,nhrznd,iartnd,axpdat(iap1),axpdat(iap2),axpdat(iap3),axpdat(iap4),        &
          axpdat(iap5),axpdat(iap6),axpdat(iap7),xlen,icab,dreact,hawst,numspp,idnlmt,sprfrc,   &
          idntp,reacsb,sbdres,idsbe,idsbn,reacs0,idsbe0,idsbn0,jdiag,idact,labeq,vlhsu,vlhsl,   &
          vrhs,rfaca,irdof,trqdat(itq1),trqdat(itq2),trqdat(itq3),trqdat(itq4),trqdat(itq5),    &
          trqdat(itq6),trqdat(itq7),defloc,rigvec,ieldfx,rotact,tglu,trgb,rtct0,rtctg,imvrg,    &
          imvrg0,athet,gdef,gdefs,dgch,locdg,npnum,locs,xndspr,ptld,alllod,fluid,lsub,temp,     &
          alphat,nume,tqold,tqnew,kstiff,bndstf,rhs,rhs0,isbelm,vrelp,vrelc,ptboye,             &
          iptby,tglsb,trgsf,seabed_axis,sxxt,rtstatm,rtstatf,elmrst0,idisby,srestf,erestf,veccg0,    &
          dspcg,dspcgd,dspcgdd,trcgd,trcgdd,implhyd,td_mnpl,tdax_mnpl,dim_mnpl,imnpl_ves,       &
          idnlmtbs,kstiffnl,bsdiam,indic,vadmas,vedam,wcoef,cuc,qtf,thrust,cmomnt,pslug,islug,  &
          islug_fill,num_seas,calmdata,calmmass,calmdamp,nodyn,renotab,iei_flag,damper,cdamp,   &
          curvyt,curvzt,seabed_tol,dcntct,rotk,raova,phasa,last_tstep,rtstatv,rtstata)
      end if

      ! if bending hysteresis is included in the model then a second analysis stage
      ! is required for this analysis timestep
      if( is_hysteresis_in_this_model )then

        ! Proceed if the pre-hysteresis step does not have to be repeated.
        if( iincr /= -1 )then
      
          ! update the expected hystersis stiffness based on change in moment in the pre hysteresis step
          call update_eihexpect(num_int,numel,iei_flag)

          ! the pre-hysteresis analysis stage is complete, therefore reset the hysteresis step flag
          ! note that pre_hysteresis_step should not be set to false before the call to update_eihexpect
          pre_hysteresis_step = .false.
      
          ! perform hysteresis timestep - might need to check for convergence of pre-hysteresis step first
          if(nodyn == 1)then

            call static_analysis(sxx,npi,ss,sk,d,b,e,cord,data1,data2,th,data3,gs,iti,dicos,tdicos, &
              td,dd,dd0,dd1,dd2,dd3,tv,ta,tvs,tx,dd5,ex,sigxx,disp,dispt,disp1,gr,nharz,depth,      &
              rhow,dragd,dint,dout,vcur,cdnn,cdtt,cmnn,cmtt,reno,nore,npbc,ts,dd4,locm,admas,jel,   &
              outp,react,ires,stiff,vmass,dsflcl,elmrst,tq,press,spress,data4,data5,nhrznd,iartnd,  &
              axpdat(iap1),axpdat(iap2),axpdat(iap3),axpdat(iap4),axpdat(iap5),xlen,icab,dreact,    &
              hawst,numspp,idnlmt,sprfrc,idntp,reacsb,sbdres,idsbe,idsbn,reacs0,idsbe0,idsbn0,      &
              jdiag,idact,labeq,vlhsu,vlhsl,vrhs,rfaca,irdof,trqdat(itq1),trqdat(itq2),trqdat(itq3),&
              trqdat(itq4),trqdat(itq5),defloc,rigvec,ieldfx,rotact,tglu,trgb,rtct0,rtctg,imvrg,    &
              imvrg0,athet,gdef,gdefs,dgch,locdg,npnum,locs,xndspr,ptld,alllod,fluid,lsub,          &
              temp,alphat,nume,tqold,tqnew,kstiff,bndstf,rhs,rhs0,isbelm,vrelp,vrelc,ptboye,        &
              iptby,tglsb,trgsf,seabed_axis,sxxt,idisby,srestf,erestf,veccg0,dspcg,dspcgd,dspcgdd,trcgd, &
              trcgdd,deffec,implhyd,td_mnpl,tdax_mnpl,dim_mnpl,imnpl_ves,istat_step,idnlmtbs,       &
              kstiffnl,bsdiam,indic,disp2,vadmas,vedam,wcoef,cuc,qtf,thrust,cmomnt,pslug,islug,     &
              islug_fill,num_seas,calmdata,renotab,iei_flag,damper,cdamp,curvyt,curvzt,seabed_tol,  &
              dcntct,rotk,norder,stiff_modes,vmass_modes,cann,calmmass,raova,phasa,iaxfl,sprop,     &
              voffset,offset,stinger_plane,tdaux,npia,acord,last_tstep,timep)
          else

            call dynamic_analysis(sxx,npi,ss,sk,d,b,e,cord,data1,data2,th,data3,gs,iti,dicos,tdicos, &
              td,dd,dd0,dd1,dd2,dd3,dd31,tv,ta,tvs,tx,dd5,ex,sigxx,disp,dispt,disp1,disp2,gr,       &
              nharz,depth,rhow,dragd,dint,dout,vcur,cdnn,cdtt,cmnn,cann,cmtt,reno,nore,re,tf,npbc,  &
              ts0,ts,dd4,locm,admas,jel,outp,react,ires,stiff,vmass,dsflcl,elmrst,tq,press,spress,  &
              data4,data5,nhrznd,iartnd,axpdat(iap1),axpdat(iap2),axpdat(iap3),axpdat(iap4),        &
              axpdat(iap5),axpdat(iap6),axpdat(iap7),xlen,icab,dreact,hawst,numspp,idnlmt,sprfrc,   &
              idntp,reacsb,sbdres,idsbe,idsbn,reacs0,idsbe0,idsbn0,jdiag,idact,labeq,vlhsu,vlhsl,   &
              vrhs,rfaca,irdof,trqdat(itq1),trqdat(itq2),trqdat(itq3),trqdat(itq4),trqdat(itq5),    &
              trqdat(itq6),trqdat(itq7),defloc,rigvec,ieldfx,rotact,tglu,trgb,rtct0,rtctg,imvrg,    &
              imvrg0,athet,gdef,gdefs,dgch,locdg,npnum,locs,xndspr,ptld,alllod,fluid,lsub,temp,     &
              alphat,nume,tqold,tqnew,kstiff,bndstf,rhs,rhs0,isbelm,vrelp,vrelc,ptboye,iptby,       &
              tglsb,trgsf,seabed_axis,sxxt,rtstatm,rtstatf,elmrst0,idisby,srestf,erestf,veccg0,dspcg,    &
              dspcgd,dspcgdd,trcgd,trcgdd,implhyd,td_mnpl,tdax_mnpl,dim_mnpl,imnpl_ves,             &
              idnlmtbs,kstiffnl,bsdiam,indic,vadmas,vedam,wcoef,cuc,qtf,thrust,cmomnt,pslug,islug,  &
              islug_fill,num_seas,calmdata,calmmass,calmdamp,nodyn,renotab,iei_flag,damper,cdamp,   &
              curvyt,curvzt,seabed_tol,dcntct,rotk,raova,phasa,last_tstep,rtstatv,rtstata)

            ! Reset alpha_f for the second and subsequent steps to user input.
            ! For the first pass, alpha_f is set to zero in hysteresis dynamic analyses,
            ! which restart from analyses without hysteresis data.
            alpha_f = alpha_f0
          end if

          ! Reset hysteresis data - copy to the 'previous' timestep values,
          ! unless a shorter time-step is to be reattempted.
          if( iincr /= -1 )then
            call reset_hysteresis_data()
            call reset_hysteresis_output_data()
            ! call write_hysteresis_timetrace(int,numel,time) ! debug code temp code
          end if
        end if
      end if
      
      if( itim == 1 .and. itim_synrp == 1 )then
        elmrst_prev = elmrst
      end if
      
      ! If this is a mooring analysis, update the location of auxiliary nodes
      if( moor == 1 )then
        call update_auxiliary_location(cord,dd,tdaux,acord,icog,ndeg,nonp,nanode,ncord)
      end if

      ! Update the displacements of auxiliary nodes, which are linked to structural 
      ! nodes (rather than the traditional approach of linking auxiliary nodes to vessels)
      call update_auxiliary_bodies_node_linked(nbaux, cord, td, acord, tdaux)

      ! Update the positions of the auxiliary nodes which display the slug
      if(nmslug > 1)then
        call update_slug_display(npi, nume, time, pslug, xlen, cord, td)
      end if

      ! set control variables for cd = f(re)
      nwvpass = 1

      if(iincr == -1)cycle time_loop
      do ivset = 1,nvset
        do iro = 1,6
          dspcg0(iro,ivset) = dspcg(iro,ivset)
        end do
      end do

      ! Write out low frequency displacements of cog node for of a
      ! dynamic mooring analysis. for version 6.1.1, this is written out to an
      ! ascii file as well as to a binary file.
      if(moor == 1.and.nodyn /= 1)then
        write(mooring_file,rec=iaddr) time
        iaddr = iaddr + 1
        write(mooring_file,rec=iaddr) (td(ii,icog),ii=2,4)
        iaddr = iaddr + 1
        write(imr,fmt="(7(2x,e15.8))")time,(td(ii,icog),ii=1,6)
      end if

      ! Optional output to database
      if (idatb /= 0) then
        if ((iendb == 0.and.(time >= tfinsh.or.ialdb /= 0)).or.(iendb == 1.and.time >= tfinsh))then
          itimst = itimst + 1
          if( .not.final_static_required )then
            if( threaded_db_flag )then
              call database_output_mt(ieldtb,inodtb,dd,tv,ta,dreact,elmrst,npi,dragd,dint,dout,  &
                depth,gr,rhow,cord,ires,fluid,rtstatm,rtstatf,tdaux,idisby,num_seas,veccg0,dspcg, &
                curvyt,curvzt,idsbn,reacsb,trgsf,vmass,rtstatv,rtstata,nsprtb,mxsptv, &
                ngens,idnlmt,numspp,data2,hawst,islug,islug_fill,pslug,idntp)
            else
              call database_output(ieldtb,inodtb,dd,tv,ta,dreact,elmrst,npi,dragd,dint,dout,  &
                depth,gr,rhow,cord,ires,fluid,rtstatm,rtstatf,tdaux,idisby, num_seas,veccg0,dspcg,&
                curvyt,curvzt,idsbn,reacsb,trgsf,vmass,rtstatv,rtstata,nsprtb,mxsptv, &
                ngens,idnlmt,numspp,data2,hawst,islug,islug_fill,pslug,idntp)
            end if
          end if
          if( final_static_required .and. time >= tfinsh)then
            call write_prc_mean_values(depth, gr, rhow, islug, islug_fill, pslug)
          end if
        else
          if (iendb == 0.and.time >= timed) then
            itimst = itimst + 1
            if( .not.final_static_required )then
              if( threaded_db_flag )then
                call database_output_mt(ieldtb,inodtb,dd,tv,ta,dreact,elmrst,npi,dragd,dint,dout,  &
                  depth,gr,rhow,cord,ires,fluid,rtstatm,rtstatf,tdaux,idisby,num_seas,veccg0,dspcg,&
                  curvyt,curvzt,idsbn,reacsb,trgsf,vmass,rtstatv,rtstata,nsprtb,mxsptv,&
                  ngens,idnlmt,numspp,data2,hawst,islug,islug_fill,pslug,idntp)
              else
                call database_output(ieldtb,inodtb,dd,tv,ta,dreact,elmrst,npi,dragd,dint,dout,  &
                  depth,gr,rhow,cord,ires,fluid,rtstatm,rtstatf,tdaux,idisby,num_seas,veccg0,dspcg,&
                  curvyt,curvzt,idsbn,reacsb,trgsf,vmass,rtstatv,rtstata,nsprtb,mxsptv,&
                  ngens,idnlmt,numspp,data2,hawst,islug,islug_fill,pslug,idntp)
              end if
            end if
            timed  = timed + timdb
          end if
        end if

      end if        

      ! In the cases where the end of the timetrace output is not specified, so that
      ! it defaults to the analysis finish time, the last time-step may have no timetrace
      ! output, because variable time maybe marginally greater than variable end_ttr.
      ! The following condition will cater for these cases:
      less_than_end = dabs(end_ttr - tfinsh) < smallnum .or. time <= end_ttr

      ! Optional output of timetrace to .grf file
      ! timetrace_output is called immediately after start time and after subsequent intervals
      if( (nttr > 1).and.(time >= start_ttr) .and. less_than_end )then
        if( icgrf == 0 )then          
          call timetrace_output(icgrf,dd,react,elmrst,dragd,dint,dout,depth,gr,rhow,idttr,  &
            iloc,forout,npi,cord,fluid,sprop,iaxfl,npnum,nume,idisby,num_seas,curvyt,curvzt, &
            nsprtb,mxsptv,ngens,idnlmt,numspp,data2,hawst,islug,islug_fill,pslug)
          icgrf = icgrf + 1
          time_elapsed = time
        else if( time >= (time_elapsed + interval_ttr) )then
          call timetrace_output(icgrf,dd,react,elmrst,dragd,dint,dout,depth,gr,rhow,idttr,   &
            iloc,forout,npi,cord,fluid,sprop,iaxfl,npnum,nume,idisby,num_seas,curvyt,curvzt, &
            nsprtb,mxsptv,ngens,idnlmt,numspp,data2,hawst,islug,islug_fill,pslug)
          icgrf = icgrf + 1
          time_elapsed = time
        end if
      end if

      ! Check if installation criteria has been satisfied
      if( (analysis_stage == criteria_static .or. analysis_stage == sheave_static) .and. &
        crit_terminate  )then
        last_tstep = .true.
        tfinsh = time
        timep  = time
      end if

      ! Get the latest analysis command from the GUI
      gui_cmd = flexcom_get_analysis_cmd(dll_handle)
    
      ! Check for pause command
      if( cmd_pause == gui_cmd )then
        call suspend_resume(dll_handle, stopped_after_pause)
      end if   

      ! Check for stop command or stopped_after_pause flag. This If block must follow
      ! the pause block.
      if( cmd_stop == gui_cmd .or. stopped_after_pause )then
        analysis_stopped = flexcom_set_analysis_state(dll_handle, state_stopped)
        tfinsh = time
        timep = time
        call error_handling(iscrn, iout, 502, "Analysis stopped by user.")
      end if
   

      ! Printed output ... if appropriate
      if( (time > timep - 1.d-2 * ptime) .and. (time <= print_end_time) )then
        if(moor == 0.or.nodyn == 1.or.(moor == 1.and.nodyn /= 1))then

          ! Additional output requested under *PRINT - time dependent data
          call star_print_output(depth,gr,rhow,time,nume,trgb,tglu,omega,wvamp,wvrn,dir,wvnum,.false., &
            islug, islug_fill, pslug, centrifugal_nodal, idntp, .false.)

          ! Increment the print time
          timep = timep + ptime
        end if

        ! Call subroutine to calculate mooring system stiffness at mean vessel
        ! position if this is a static mooring analysis
        if (imstc == 1) then
          call mv_system_stiffness(td,fves,tdcogp,fvespr,stfmor)
        end if
      end if

     
      ! If last timestep then output the SUCCESSFUL FLEXCOM ANALYSIS box
      if( last_tstep )then
      
        ! Additional output requested under *PRINT - end analysis dependent data
        call star_print_output_end()
      
        if( analysis_stage /= criteria_static .or. all(crit_sat) )then
       
          call write_XML_tag(iout, 2, "Open", "Flexcom Solution Output", "Solution Output")
          call write_XML_tag(iout, 3, "Open", "Solution Output")
          write(iout,510)time
          call write_XML_tag(iout, 3, "Close")
          call write_XML_tag(iout, 2, "Close")
        
        end if
          
        if( do_synt_rope_loop .and. synthetic_rope_analysis_requested) then
          call check_convergence_in_offset_change(nonp,ndeg,dd0,fbpos,fbpos_prev,offset_conv_satisfied)
          
          ! If convergence achieved, terminate with success
          if( offset_conv_satisfied )then
            write(iout,505)itim_synrp
            
          ! If maximum number of iterations reached, terminate with unconverged results
          else if( synrp_maxitr_reached )then
            write(iout,506)itim_synrp+1
          end if
          
        ! Accept the case that Flexcom Analysis is successful but Synthetic Rope Analysis is not converged (Format 506)
        ! Commented below, otherwise "SUCCESSFUL FLEXCOM ANALYSIS" printed twice in .out file of non-synt rope examples
        !else
        !  write(iout,510)time
        end if

        call write_XML_tag(iout, 3, "Close")
        call write_XML_tag(iout, 2, "Close")
      end if
      
      ! Exit the synt_rope_loop if this step is the last step in the synthetic rope loop
      if( do_synt_rope_loop .and. last_tstep ) then
          exit synt_rope_loop
      end if
      
      ! Check whether more time steps remain
      if( .not.last_tstep )then
        itim = itim + 1
        ! Reduce the time step if necessary so that the finish time isnt exceeded
        if( time + tstep > tfinsh )then
          tstep = tfinsh - time
          time = tfinsh
        else
          time = time + tstep
        end if
      else
        exit time_loop
      end if

      ! Time history wave
      if(is_wave_timehist) then
        ! Check if the analysis time is greater than wavetime history elapsed time.
        ! The next block of time history starts only if the analysis time  > previous block wave timehist elapsed time. 
        if(time > wth_elapstime)then
          ! The smaller time history data is transformed to freq domain and regular waves are generated for this time history data.
          call process_timehistory_data(depth,gr,time, wth_elapstime,tfinsh) 
          ! Time history generated regular wave is converted to circular freq.
          call convrt_waveperiod_cirfreq(nharz,dd0,depth,npi,jel,nhrznd,xleno,damper, gr,wth_elapstime,time, first_wth_print )
        end if
      end if
      
    end do time_loop
    
    if( finished_time_loop .and. do_synt_rope_loop .and. length_adjustment_permitted ) then
      
      ! If the model contains synthetic rope and the analysis type is static or quasi-static, then
      ! adjust delayed elastic stretch (DES) and delayed elastic recovery (DER) for the synthetic rope
      ! elements. 
      ! This step is skipped for dynamic analysis and is performed only when the model contains synthetic 
      ! rope elements and the flag length_adjustment_permitted is true.
      if( nsynrp > 0 )then        ! .and. itim > 1 .and. .not.last_tstep) then
        do i = 1,nsynrp
          iel = numsynrp(i)
          
          ! Adjust the lengths for synthetic rope elements using the LENGTH=Curve
          call adjust_length_for_des_der (iel,elmrst,elmrst_prev,cord,dragd,dint,dout,npi,depth,gr,rhow, &
                td,fluid,idisby,islug,islug_fill,pslug,nsprtb,mxsptv,ngens,hawst,numspp,ilen_curv_tab,data2, &
                xlen,xlenp)  ! It seems that STATIC=Curve vs. STATIC=Stiffness show slightly varied strains

          ! Update the stiffness matrix for synthetic rope elements only due to the change in length.
          ! Only update the matrix entries associated with synthetic rope elements.
          n = numsynrp(i)
          is_nl_flexjoint = fj_status(n) > 0

          if( idntp(n) == 1 .and. idnlmt(n) == 0 .and. &
            (kstiff(n) == 0 .or. kstiffnl(n) == 0) .and. &
            .not.(is_nl_flexjoint) ) then
            call assemble_k_linear(k_linear(1:n1,1:n1,n), kstiff(n), data1(n), data2(n), &
              data4(n), data5(n), xlen(n), imod_rst, bndstf)
          end if

          ! Update the integration points and shape functions for synthetic rope elements.
          if( idntp(n) == 1 )then
            call update_integration_points(num_int, xlen(n), fc(1:maxint,n), fw(1:maxint,n), fb(1:maxint+1,n))
            call update_shape_functions(num_int, xlen(n), fc(1:maxint,n), &
              t(1:ngens,1:n1,1:maxint,n), t2(1:ngens,1:n1,1:maxint,n))
          end if
        end do
        
        ! Save element restoring forces in another array to be used to calculate element strain
        ! during next synthetic rope iteration
        elmrst_prev = elmrst
        
      end if
      
      ! Execute static analysis with adjusted lengths of synthetic rope elements
      ! *SYNTHETIC ROPE ANALYSIS keyword is not present in this case
      if( .not.synthetic_rope_analysis_requested )then
        ! Set the flag to execute time_loop one more time
        perform_last_step = .true.
        
        ! For initial static analysis should be performed in one iteration.
        ! It is required that time should be incremented from start to finish in one step.
        ! This constraint has been implemented in the ACM.
        ! Exit when itim_synrp becomes 1. There is an exit in the time_loop. So, this exit is actually redundant.
        if( itim_synrp >= 2 ) then
          exit synt_rope_loop
        else
          ! Initialize the time variables
          time         = tstart
          tstep        = tstep_user
          if(time <= 1.d-20)time = time + tstep
          itim         = 1
          itimst       = 0
        end if
        
        ! To perform static/quasi-static analysis with length adjument at each iteration
        ! *SYNTHETIC ROPE ANALYSIS is present with convergence tolerance and max number of iterations
      else if( synthetic_rope_analysis_requested ) then
        ! Check that the synt_rope_loop has been executed more than two times
        if( itim_synrp >= 2 )then
          ! Check convergence: is the normalized change in offset from the previous to the present 
          ! vessel positions below the convergence tolerance? If yes, generate the usual outputs and
          ! proceed to the next analysis.
          call check_convergence_in_offset_change(nonp,ndeg,dd0,fbpos,fbpos_prev,offset_conv_satisfied)
          ! Check here if convergence has been achieved
          if( offset_conv_satisfied )then
            ! Exit the synt_rope_loop ineration loop
            exit synt_rope_loop
          end if
        end if
          
        ! Check if next iteration would be last synthetic rope iteration, given the maximum number of iteration
        if( itim_synrp >= synrp_maxitr-1 )then
          ! Set the flag to execute time_loop in the last synt_rope_loop ineration
          perform_last_step = .true.
          ! Set a flag to be used in the time_loop to write the unconverged solution
          synrp_maxitr_reached = .true.
        end if
        
        ! Save the position of the vessel COG at this iteration, to be used during next convergence check
        do ifbset = 1, num_fbset
          do i = 2, 3
            fbpos(i,ifbset) = dd0(i,fb_cognode(ifbset))
          end do
        end do
        
        ! initialize the time variables
        time         = tstart
        tstep        = tstep_user
        if(time <= 1.d-20)time = time + tstep
        itim         = 1
        itimst       = 0
      end if
    else
      exit synt_rope_loop
    end if

  end do synt_rope_loop

  tfinsh = time
  
  ! Wait here for final database writing thread to complete and then close it.
  call wait_for_and_close_thread
    
  !Check for presence of dongle after analysis is finished
  call check_dongle
  call check_flexcom_dynamic_license

  
  ! For dynamic analyses, write out statistics if appropriate
  if( idbstat==1 .and. nodyn /= 1 .and. istatc == 0 .and. icalst == 1 .and. time > statim ) then
    call write_XML_tag(iout, 2, "Open", "Statistics Output", "Data Output")
    call output_stats(npnum,nume,npi,rtstatm,rtstatf,jel,damper)  
    call write_XML_tag(iout, 2, "Close")        
        
  ! For static and quasi-static analyses, write out snapshot 
  ! of nodal displacements and element forces
  else if( nodyn == 1 .or. (nodyn == 0 .and. istatc == 1) )then
    if( analysis_stage /= criteria_static .or. all(crit_sat) ) then
      if(noutyp /= 2)then
        call write_XML_tag(iout, 2, "Open", "Static Results", "Data Output")
        call output_static_results(npi,td,tv,ta,outp,react,elmrst,jel,cord,rotact,npbc,   &
            dragd,dint,dout,depth,gr,rhow,nume,npnum,fluid,vrelcon,idisby,    &
            damper,natch,idatn,islug,islug_fill,pslug, idntp)
        call write_XML_tag(iout, 2, "Close")
      end if
      if(noutyp /= 1)then
        call write_XML_tag(iout, 2, "Open", "Integration Point Results", "Data Output")
        call output_integ_pt_results(ex,sigxx,jel,damper)
      call write_XML_tag(iout, 2, "Close")
      end if
    end if   
  end if
  
  ! Write out end-of-low frequency data indicator at the end of mooring analysis
  if(moor == 1.and.nodyn /= 1)then
    eof = 99999.0d0
    write(mooring_file,rec=iaddr) eof
  end if

  ! If this is a dynamic mooring analysis output statistical parameters
  if(moor == 1.and.nodyn /= 1)then
    call write_XML_tag(iout, 2, "Open", "Mooring Analysis Statistics Output", "Data Output")
    call mv_output_stats(stfmor,vadmas)
    call write_XML_tag(iout, 2, "Close")
  end if

  ! Close .mor file at the end of dynamic mooring analysis
  if(moor == 1.and.nodyn /= 1)then
    close(unit=imr)
  end if

  ! Write out restart variables
  rhopo  = rhop
  rhptqo = rhptq
  icrold = icur
  if(icursu == 1)icrold = 1
  iwvold = isea(1)
  call setup_restart_file_addresses(ierr)
  if( ierr /= 0 )then
    call error_handling(iscrn, iout, 253, &
      & "Error: Unable to allocate sufficient memory for restart file data.")
  end if
  call restart_file_write_record(iap3,iap6,iap7,itq3,itq6,itq7,stfmor,acord,npbc)
  call deallocate_plas_store_data
  call deallocate_plas_output_data

  ! Close and delete direct access user boundary condition files and
  ! vessel timetrace files, if appropriate
  if (ndfile > 0 .or. ndrefc > 0 .or. ndrift_files > 0 .or. nvesttr > 0) then   
    call close_bc_files(nbcfil,tmpffl,tmpfrf,ivdrift,ivesttr,tmpves)
  end if

  ! Output required tolerance criteria for static solution convergence to
  ! temporary file for reading by the analysis control module
  if (nodyn == 1) call output_tolerance(epsi)
  
  ! Set colour contour data flag and write colour contour data
  if( writedbm )call rewrite_Colour_Contour_Ranges_And_Element_Lengths(iodbm)
  if( writedbf )call rewrite_Colour_Contour_Ranges_And_Element_Lengths(iodbf)

  call deallocate_flexjoints  ! Deallocate mainb arrays
  call mainb_deallocate

  call deallocate_refnod_data
  call deallocate_vessel_data

  return

300   format('#STRT',F13.5)
400   format('#FNSH',F13.5)
500   format(3(/),1X,28('='),'END OF INPUT DATA ECHO',28('='))
      
505   format(10(/),15X,50('#'),/,15X,'#',48X,'#',/,15X,'#',9X,'SUCCESSFUL FLEXCOM ANALYSIS   ',&
    9X,'#',/,15X,'#',48X,'#',/,15X,'#',6X,'(SYNTHETIC ROPE ANALYSIS CONVERGED) ',6X,'#',/,15X, &
    '#',48X,'#',/,15X,'#',15X,'OUTPUT OF RESULTS',16X,'#',/,15X,'#',48X,'#',/,15X,'#',11X,     &
      'ITERATION NUMBER IS',I7,11X,'#',/,15X,'#',48X,'#',/,15X,50('#'),///)
      
506   format(10(/),15X,50('#'),/,15X,'#',48X,'#',/,15X,'#',14X,'FLEXCOM ANALYSIS    ',&
    14X,'#',/,15X,'#',48X,'#',/,15X,'#',4X,'(SYNTHETIC ROPE ANALYSIS NOT CONVERGED) ',4X,'#',/,15X, &
    '#',48X,'#',/,15X,'#',15X,'OUTPUT OF RESULTS',16X,'#',/,15X,'#',48X,'#',/,15X,'#',11X,     &
      'ITERATION NUMBER IS',I7,11X,'#',/,15X,'#',48X,'#',/,15X,50('#'),///)
      
510   format(10(/),15X,50('#'),/,15X,'#',48X,'#',/,15X,'#',9X,'SUCCESSFUL FLEXCOM ANALYSIS   ',&
    9X,'#',/,15X,'#',48X,'#',/,15X,'#',15X,'OUTPUT OF RESULTS',16X,'#',/,15X,'#',48X,'#',/,15X,&
    '#',11X,'SOLUTION TIME IS',F10.3,11X,'#',/,15X,'#',48X,'#',/,15X,50('#'),///)

end subroutine mainb