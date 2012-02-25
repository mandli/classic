module utility_module

    implicit none
    
contains
    
    ! =========================================================================
    !  subroutine open_data_file(unit_num, file_name)
    !     integer, intent(in) :: unit_num
    !     character(len=*), intent(in) :: file_name
    !
    ! Open the file file_name in unit number unit_num fast forwarding past
    ! the comment lines demarked by "#"
    !
    subroutine open_data_file(unit_num, file_name)

        implicit none

        ! Input arguments
        integer, intent(in) :: unit_num
        character(len=*), intent(in) :: file_name
        
        ! Local storage
        integer :: line, comment_lines
        character(len=1) :: first_char
        character(len=59) :: read_format = &
                "('Reading data file, first',i2,' lines are comments: ',a12)"
        
        ! Open file and read until the first character in the line is not #
        open(unit=unit_num,file=file_name,status='old',form='formatted')
        first_char = "#"
        comment_lines = -1
        do while (first_char == "#")
            read(unit_num,*) first_char
            comment_lines = comment_lines + 1
        enddo
        print read_format, comment_lines, file_name
        
        ! Rewind the file
        rewind(unit_num)
        do line=1,comment_lines
            read(unit_num,*) first_char
        enddo
        
    end subroutine open_data_file
    ! =========================================================================
    
end module utility_module