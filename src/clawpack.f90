program clawpack

    use parameter_module    
    use solution_module
    use solver_module

    implicit none
    
    ! Local storage
    character(len=*) :: parameter_file
    integer :: frame
    
    ! User function interfaces for setup
    interface
        subroutine solution_init(solution)
            use solution_module
            implicit none
            type(solution) :: solution
        end subroutine
    end interface
    
    ! Parse parameter file
    parameter_file = 'claw.data'
    claw_params = read_claw_parameters(parameter_file)
    
    ! Call user set problem function
    call setprob()
    
    ! Create new solution
    solution = new_solution(claw_params)
    ! Call user solution initialization function
    call solution_init(solution)
    
    ! Initialize solver variables
    solver = new_solver(claw_params)
    
    ! Initialize output loop variables
    frame = 0
    
    ! Output initial condition
    if (output_init_condtion) then
        call output_solution(frame,solution)
    endif
    
    ! Main output loop
    do n=1,len(claw_params%out_times)
        
        ! Evolve to next output time
        status = evolve_to_time(solution,out_times(n),single_step_mode)
        
        if (status /= 0) then
            stop "Error occured in evolve_to_time"
        endif
        
        ! Output solution
        frame = frame + 1
        call output_solution(frame,solution)
        
    enddo
    
end program clawpack


