
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************

!>
!! Subroutine to set control variables for cd = f(re)
!! This subroutine should be described in more detail. 
!<
subroutine set_reynolds_control(vrelcon, vrelc, vrelp, tstart, time, numel,  &
	& num_int, nodyn, tramp, nwvpass, nperiod, istatc, ndimwv, omega)
  
  
  
  
  
  
  
  
  ! Error: Module not found math
  use math, only : pi    

  implicit none
  !*--SET_REYNOLDS_CONTROL2211
  !
  !*** Start of declarations rewritten by SPAG
  !
  ! Dummy arguments
  !
  integer, intent(in) :: istatc
  integer, intent(in) :: ndimwv
  integer, intent(in) :: nodyn
  integer, intent(inout) :: nperiod
  integer, intent(in) :: num_int
  integer, intent(in) :: numel
  integer, intent(out) :: nwvpass
  real(8), intent(in) :: time
  real(8), intent(in) :: tramp
  real(8), intent(in) :: tstart
  real(8), intent(in), dimension(ndimwv) :: omega
  real(8), dimension(num_int, numel) :: vrelc
  real(8), dimension(5) :: vrelcon
  real(8), dimension(num_int, numel) :: vrelp
  
  intent(in) vrelc, vrelp
  intent(inout) vrelcon
  !Local variables
  real(8) :: dabs
  integer :: i
  integer :: j
  integer :: k
  real(8) :: wavperd
  
  !
  !*** End of declarations rewritten by SPAG
  !

  if (nodyn == 1) then                          ! static analysis
    nwvpass = 0
  else if (nodyn == 0.and.istatc == 1) then     ! quasi-static analysis
    nwvpass = 0
  else                                          ! dynamic analysis
    wavperd = 0.0d0 
    if (omega(1) > 1.0d-8) wavperd = (2.0d0 * pi) / omega(1)
    ! check for new wave period

    if (time > (tstart + tramp + (nperiod * wavperd))) then
      nwvpass = 0
      nperiod = nperiod + 1
      do i = 1,4                                ! swap convergence
        vrelcon(6-i) = vrelcon(5-i)             ! check from previous
      end do                                    ! 4 wave periods
      vrelcon(1) = 0.0d0
      do j = 1,numel                            ! calculate current
        ! check is this an active element
        !if(active_elem(j,2) /= 1)cycle 
        do k=1,num_int                              ! convergence check
          if (dabs(vrelc(k,j)) > 1.d-20) then
            vrelcon(1) = vrelcon(1) + dabs((vrelc(k,j) - vrelp(k,j)) / vrelc(k,j))
          end if
        end do
      end do
      vrelcon(1) = vrelcon(1) / float(numel * num_int)
      vrelcon(1) = vrelcon(1) * 100.0           ! in percent
    end if
  end if

end subroutine set_reynolds_control