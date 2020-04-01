

! ****************************************************************************************
! ****************************************************************************************
! ****************************************************************************************
! ****************************************************************************************  


subroutine delete_sb_moment_loads(idsbn,node1,node2,disp1, nonp, n1, rlmulg, &
	& rlmutv, chmax, irigsf, nmsbe, nmsbn, iarbsb, nsbdpt, sbstf, stflat, sslope, &
	& ielasf, ilatstf)
 
 implicit none
  ! ----------------------------------------------------------------------
  ! Subroutine to delete moment loads for elements in contact with the 
  ! seabed or guide contact surfaces.
  ! ----------------------------------------------------------------------
  
  ! common block vars
  
  


  ! delcared common block types
  real(8) :: chmax
  integer :: iarbsb
  integer :: ielasf
  integer :: ilatstf
  integer :: irigsf
  integer :: n1
  integer :: nmsbe
  integer :: nmsbn
  integer, intent(in)             :: node1
  integer, intent(in)             :: node2
  integer :: nonp
  integer :: nsbdpt
  real(8) :: rlmulg
  real(8) :: rlmutv
  real(8) :: sbstf
  real(8) :: sslope
  real(8) :: stflat
  double precision, intent(out)   :: disp1(n1)
  integer, intent(in)             :: idsbn(nonp)
  
  !Local variables
  integer :: i
  integer :: inode
  integer :: ncontn
  

  ! first delete moment loads if both nodes of element are in contact
  ! with a rigid seabed

  if (irigsf == 1.and.ielasf == 0) then
    ncontn = 0
    do i = 1,nmsbn
      if (idsbn(i) == 0) cycle
      inode = idsbn(i)
      if (inode == node1.or.inode == node2) then
        ncontn = ncontn + 1
      end if
      if (ncontn == 2) then
        disp1(4)  = 0.d0
        disp1(5)  = 0.d0
        disp1(6)  = 0.d0
        disp1(10) = 0.d0
        disp1(11) = 0.d0
        disp1(12) = 0.d0
        exit
      end if
    end do
  end if

  return

end subroutine delete_sb_moment_loads