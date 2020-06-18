
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************

!>
!! Subroutine to find the added mass and inertia coefficients in Morison's equation which 
!! are used to determine the mass density of an element. Refer to Paddy O'Brien's thesis
!! section 5.4 for more details
!<
subroutine calculate_reynolds_coeffs(cdn, cdt, can, cmt, cmn, cdnn, cdtt, cann,  &
	& cmtt, cmnn, reno, nore, vrelp, dragd, maxre, viskin)
  
  
  implicit none
  !*--CALCULATE_REYNOLDS_COEFFS318
  !
  !*** Start of declarations rewritten by SPAG
  !
  ! Dummy arguments
  !
  real(8) :: can
  real(8) :: cdn
  real(8) :: cdt
  real(8) :: cmn
  real(8) :: cmt
  real(8) :: dragd
  integer, intent(in) :: maxre
  real(8), intent(in) :: viskin
  real(8) :: vrelp
  real(8), dimension(maxre) :: cann
  real(8), dimension(maxre) :: cdnn
  real(8), dimension(maxre) :: cdtt
  real(8), dimension(maxre) :: cmnn
  real(8), dimension(maxre) :: cmtt
  integer, dimension(maxre) :: nore
  real(8), dimension(maxre) :: reno
  
  intent(in) cdnn,cdtt,cann, cmtt, cmnn, dragd, nore, reno, vrelp
  intent(out) cdn,cdt,can, cmt, cmn
  !Local variables
  integer :: i
  real(8) :: reyfac
  real(8) :: reyn
  
  !
  !*** End of declarations rewritten by SPAG
  !
 
  ! if cm constant ....

  if(nore(1) == 1)then
    cdn = cdnn(1)
    cdt = cdtt(1)
    can = cann(1)
    cmt = cmtt(1)
    cmn = cmnn(1)
  else

    ! else if ca = f(re) => calculate re = f(vrelp) ...

    reyn = vrelp * dragd / viskin
    ! & calculate cof. = f(re)

    if(reyn <= reno(1))then
      cdn = cdnn(1)
      cdt = cdtt(1)
      can = cann(1)
      cmt = cmtt(1)
      cmn = cmnn(1)
    elseif(reyn >= reno(nore(1)))then
      cdn = cdnn(nore(1))
      cdt = cdtt(nore(1))
      can = cann(nore(1))
      cmt = cmtt(nore(1))
      cmn = cmnn(nore(1))
    else
     do i=2,nore(1)
      if(reyn < reno(i))then
        reyfac = (reyn-reno(i-1)) / (reno(i)-reno(i-1))
        cdn = cdnn(i-1) + (cdnn(i)-cdnn(i-1)) * reyfac
        cdt = cdtt(i-1) + (cdtt(i)-cdtt(i-1)) * reyfac
        can = cann(i-1) + (cann(i)-cann(i-1)) * reyfac
        cmt = cmtt(i-1) + (cmtt(i)-cmtt(i-1)) * reyfac
        cmn = cmnn(i-1) + (cmnn(i)-cmnn(i-1)) * reyfac
        goto 55
      endif
     end do
55      continue
    endif

  endif

  end subroutine calculate_reynolds_coeffs