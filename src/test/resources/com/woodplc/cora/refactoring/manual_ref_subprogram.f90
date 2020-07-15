
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************
! ********************************************************************************


!>
!! Subroutine to check if the pip contact flag has been set for any connection 
!! whose end node 
!<
subroutine check_pip_contact(int_node, contact, npipnod, nkpipnod, mxpipnod,  &
	& pip_nodes, pip_ignore, n0kpipnod, pip_contact, numel, pip_inner_elem,  &
	& size_epi, nonp, epi)
  ! This should not be refactored
  use FiniteElementSolverAPIs, only : finite_element_solver
  ! Test error is produced for unknown module
  ! Error: Module not found some_unknown_module
  use some_unknown_module
  
  ! Error: Module variable not found pipe_in_pipe_data :: unknown_variable
  ! Error: Module variable not found pipe_in_pipe_data :: and_2
  use pipe_in_pipe_data, only: unknown_variable, and_2
  
  
  
  implicit none
  
  ! Declare variables that are passed as arguments
  integer, intent(out)  :: contact
  integer, intent(in)   :: int_node
  integer, intent(in) :: mxpipnod
  integer, intent(in) :: n0kpipnod
  integer, intent(in) :: nkpipnod
  integer, intent(in) :: nonp
  integer, intent(in) :: npipnod
  integer, intent(in) :: numel
  integer, intent(in) :: size_epi
  integer, intent(in), dimension(size_epi,nonp) :: epi
  logical, intent(in), dimension(mxpipnod-n0kpipnod) :: pip_contact
  logical, intent(in), dimension(mxpipnod) :: pip_ignore
  integer, intent(in), dimension(numel) :: pip_inner_elem
  integer, intent(in), dimension(mxpipnod,2) :: pip_nodes
  
  !Local variables
  integer :: ipair
  integer :: inner_node
  integer :: outer_node
  integer :: nelem
  integer :: ielem
  integer :: allocate_error
  integer :: iattach
  integer :: i
  integer, allocatable, dimension(:) :: elem
  integer, allocatable, dimension(:) :: outer_elem
  
  
  contact = 0

  ! Show active inner section nodes - for debugging - not optimised
  if(.false.) then

    ! Count the elements that contain this node
    nelem = count(epi(1:size_epi,int_node) > 0)
    
    ! If node is not on an element exit
    if( nelem == 0 )return

    ! First alocate arrays containing the elements with this node
    ! and the corresponding outer_elements
    error_loop: do i = 1,1
      allocate(elem(nelem), stat=allocate_error)
      if( allocate_error /= 0 )exit
      elem = 0
      allocate(outer_elem(nelem), stat=allocate_error)
      if( allocate_error /= 0 )exit
      outer_elem = 0
    end do error_loop
    if( allocate_error /=0 )then
      return
    end if

    ! Find elements containing this node
    do iattach = 1, nelem
      elem(iattach) = epi(iattach,int_node) ! Attached element number
    end do
    
    ! Check if any element that contains the node is part of an
    ! inner element set in a *PIP SECTION. The returned result is the
    ! outer element number. Zero means no correspondent.
    do i = 1, nelem
      ielem = elem(i)
      if( ielem > 0 )then
        outer_elem(i) = pip_inner_elem(ielem)
      end if
    end do
    
    ! If at least one outer element exists, then this node is an inner PIP node
    if( product(outer_elem) > 0 ) then
      contact = mod(outer_elem(1)-1,4)+1
    end if
    
    ! Deallocate local arrays
    deallocate(elem)
    deallocate(outer_elem)

  ! Show pipe-in-pipe connections - for debugging 
  else if(.false.) then

    ! Loop over each pipe-in-pipe connection
    do ipair = 1, npipnod
    
      ! Find connected nodes
      outer_node  = pip_nodes(ipair,1)
      inner_node  = pip_nodes(ipair,2)
    
      if(int_node == outer_node .or. int_node == inner_node) then
        contact = mod(ipair-1,4)+1
        return
      end if
    end do

  ! Show pipe-in-pipe contact 
  else

    ! Loop over each pipe-in-pipe connection
    do ipair = 1, nkpipnod
    
      ! Skip inactive connections
      if(pip_ignore(ipair))cycle
      
      ! Find connected nodes
      outer_node  = pip_nodes(ipair,1)
      inner_node  = pip_nodes(ipair,2)
    
      if(int_node == outer_node .or. int_node == inner_node) then
        if(pip_contact(ipair)) then
          contact = 1
        end if
        return
      end if
    end do

  end if

end subroutine check_pip_contact