
  ! ********************************************************************************************
  ! ********************************************************************************************
  ! ********************************************************************************************
  ! ********************************************************************************************

  !>
  !! Subroutine to set the outer node for each inner section node. If the inner section node is
  !! a primary connection identify this by setting the connection number as a negative value. If 
  !! no node is found in the outer set then look for an outer node in any nested sections
  !<
  subroutine update_inner_pip_outer_nodes(section_limit_exceeded, npipsect,  &
	& mxpipsect, pip_section, nonp, pip_inner_node, npipnod, mxpipnod,  &
	& pip_nodes, nested_section_limit, npnum, nndset, ndset_nod, ndset_defn,  &
	& ndset_name)
  
  
  
  

  implicit none
  
  ! Declare variables that are passed as arguments
  integer, intent(in) :: mxpipnod
  integer, intent(in) :: mxpipsect
  integer, intent(in) :: nested_section_limit
  integer, intent(in) :: nndset
  integer :: nonp
  integer, intent(in) :: npipnod
  integer, intent(in) :: npipsect
  logical, intent(out)  :: section_limit_exceeded
  integer, intent(in), dimension(nndset,nonp) :: ndset_defn
  character(len=256), intent(in), dimension(nndset) :: ndset_name
  integer, intent(in), dimension(nndset) :: ndset_nod
  integer, dimension(nonp) :: npnum
  integer, intent(inout), dimension(nonp) :: pip_inner_node
  integer, intent(in), dimension(mxpipnod,2) :: pip_nodes
  character(len=32), intent(in), dimension(mxpipsect,2) :: pip_section
  
  !Local variables
  integer :: ipair
  integer :: inode
  integer :: jnode
  integer :: user_node_no
  integer :: jsect
  integer :: section_no
  integer :: current_section
  integer :: next_section
  integer :: inner_node_set_no
  integer :: outer_node_set_no
  integer :: inner_node
  integer :: outer_node
  integer :: node_user_to_internal
   !function

  section_limit_exceeded = .false.

  ! Loop over all PIP sections
  do section_no = 1, npipsect

    ! Find the node set name that is the same as the inner element
    do inner_node_set_no=nndset,1,-1
      if(pip_section(section_no, 2) == ndset_name(inner_node_set_no)) exit
    end do

    ! Loop over the inner node set
    do inode = 1,ndset_nod(inner_node_set_no)
      user_node_no = ndset_defn(inner_node_set_no,inode)
      inner_node  = node_user_to_internal(user_node_no,npnum,nonp)

      current_section = section_no

      ! To account for pipe-in-pipe-in-pipe... we need to loop over all sections here again
      ! starting at the current section. If no outer element is found, continue to try other sections that 
      ! have an inner set equal to the current outer set, but not indefinitely
      section_loop: do jsect=1,nested_section_limit

        ! Find the node set name that is the same as the outer element set
        do outer_node_set_no=nndset,1,-1
          if(pip_section(current_section, 1) == ndset_name(outer_node_set_no)) exit
        end do

        ! Loop over the outer node set
        do jnode = 1,ndset_nod(outer_node_set_no)
          user_node_no = ndset_defn(outer_node_set_no,jnode)
          outer_node  = node_user_to_internal(user_node_no,npnum,nonp)

          ! Loop over all PIP connections searching for the one that has the inner node and one 
          ! of the outer nodes. Also determine if the inner node is a secondary node in the 
          ! connection.
          do ipair = 1, npipnod
            if( pip_nodes(ipair,1) == inner_node .and. outer_node == pip_nodes(ipair,2) )then
              ! Identify that the inner node is primary by setting a negtive connection number
              pip_inner_node(inner_node) = -ipair
              exit
            else if( pip_nodes(ipair,2) == inner_node .and. outer_node == pip_nodes(ipair,1) )then
              pip_inner_node(inner_node) = ipair
              exit
            end if
          end do
          
        end do ! End loop over the outer nodes in this section

        ! Have we found the inner node connection?
        if(pip_inner_node(inner_node) /= 0) exit section_loop

        ! If no connection has been found at this stage dont give up yet.
        ! Check any other sections whos inner set name matches the current outer set name
        do next_section = 1, npipsect
          if(current_section == next_section) cycle

          if(pip_section(current_section, 1) == pip_section(next_section, 2)) then
            current_section = next_section
            cycle section_loop
          end if

        end do

        ! No connection found so just move onto the next inner node
        exit section_loop

      end do section_loop

      if( jsect > nested_section_limit ) then
        ! Very unlikely but if the number of nested sections has been exceeded and no connection is found
        ! then a warning should be issued
        section_limit_exceeded = .true.
      end if

    end do ! End loop over all inner nodes in this section

  end do ! End loop over all PIP sections

  end subroutine update_inner_pip_outer_nodes