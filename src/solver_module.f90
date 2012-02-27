module solver_module

    implicit none
    
    ! Verbose output parameters
    integer :: s_unit = 14

    type solver
        
        ! Solver status 
        integer :: num_steps_taken
        real(kind=dp) :: dt,cfl
        
        ! Time stepping parameters
        logical :: dt_variable
        real(kind=dp) :: dt_max,cfl_max,cfl_desired
        integer :: max_steps_per_output
        
        ! Method parameters
        integer :: order,trans_order,source_method
    
    end type solver

    type solver_status
        real(kind=dp) :: cfl_max,dt_max,dt_min
    end type solver_status
    
    ! Function interfaces
    interface
        subroutine step(solution,solver)
            implicit none
            type(solution), intent(inout) :: solution
            type(solver), intent(inout) :: solver
        end subroutine step
    end interface
    
    interface
    
    end interface

contains

    type(solver) function new_solver(claw_params, source_func)
    
        type(claw_parameters), intent(inout) :: claw_params
    
        ! Source function interface definition
        interface 
            subroutine source_func()
            end subroutine source_func
        end interface 
        
        ! Status of solver
        new_solver%cfl = 0.0_dp
        new_solver%num_steps_taken = 0
    
        ! Time stepping parameters
        new_solver%dt = claw_params%dt_init
        new_solver%dt_max = claw_params%dt_max
        new_solver%cfl_max = claw_params%cfl_max
        new_solver%cfl_desired = claw_params%cfl_desired
        new_solver%max_steps_per_output = claw_params%max_steps_per_output
        if (claw_params%dt_variable > 0) then
            new_solver%dt_variable = .true.
        endif
        
        ! Method parameters
        new_solver%order = claw_params%order
        new_solver%trans_order = claw_params%trans_order
        new_solver%source_method = claw_params%source_method
        allocate(new_solver%limiter_method(len(claw_params%limiter_method)))
        new_solver%limiter_method = claw_params%limiter_method
        
        ! Boundary conditions
        new_solver%num_ghost = claw_params%num_ghost
        allocate(lower_bc_method(claw_params%num_dim))
        allocate(upper_bc_method(claw_params%num_dim))
        new_solver%lower_bc_method = claw_params%lower_bc_method
        new_solver%upper_bc_method = claw_params%upper_bc_method

        ! Check to make sure all parameters are valid
        ! Periodic boundary condition
        if ((new_solver%lower_bc_method(i) == 2) .and. 
            (new_solver%upper_bc_method(i) /= 2)) then
            print *,"Periodic boundary conditions requested but"
            print *,"the ",i,"th dimensions are not both set to 2!"
        endif
        ! CFL conditions
        if (new_solver%dt_variable .and. &
            new_solver%cfl_desired > new_solver%cfl_max) then
            stop "Variable time-steppig and desired CFL > maximum CFL."
        endif
    
    end function new_solver
    
    type(solver_status) function evolve_to_time(solution,solver, &
                                                t_end,step_mode) &
                                                result(status)
                                                
        use solution_module
    
        implicit none
        
        ! Input arguments
        type(solution), intent(inout) :: solution
        type(solver), intent(inout) :: solver
        real(kind=dp), intent(in) :: t_end
        logical, intent(in) :: step_mode

        ! Setup status
        status%cfl_max = solver%cfl
        status%dt_max = solver%dt
        status%dt_min = solver%dt
        status%num_steps_taken = 0
        
        ! Check to make sure we are not already at or past t_end
        if (t_end <= solution.t) then
            print *,"WARNING:  Already at requested t_end."
            return
        endif
        
        ! Setup for the run
        t_start = solution%t
        max_steps = solver%max_steps_per_output
        
        ! If we are in single step mode, assume that t_final is actually an
        ! integer value telling us how many time steps to take
        if (step_mode) then
            max_steps = int(t_final)
        else
            if (.not.dt_variable) then
                ! If we are not using adaptive time steps, check to make sure
                !  we can reach the end of the time interval
                max_steps = int((t_final - solution%t + 1._dp-10) / solver%dt)
                if (abs(max_steps * solver%dt - (t_final)))
            endif
        endif
        
        ! Primary time stepping loop
        do n=1,max_steps
            
            ! Adjust dt so we hit t_end exactly if we are near t_end
            if ((solution%t + solver%dt > t_end).and.(t_start < t_end).and. &
                    (.not.step_mode)) then
                solver%dt = t_end - solution%t
            endif
            
            ! Keep a backup in case we reject this time step
            if (dt_variable) then
                ! TODO: keep backup
                t_old = solution%t
            endif
            
            ! Take one step on the system
            call step(solution,solver)
            
            ! Check CFL condition
            if (solver%cfl <= solver%cfl_max) then
                ! Accept this step
                status%cfl_max = max(cfl,status%cfl_max)
                if (dt_variable) then
                    solution%t = solution%t + solver%dt
                else 
                    ! Avoid round off error if taking static dt steps
                    solution%t = t_start + (n+1) * solver%dt
                endif
                
                if (verbose) then
                    write(s_unit,step_fmt) n,solver%cfl,solver%dt,solution%t
                endif
            
                ! Successful time step
                status%num_steps_taken = status%num_steps_taken + 1
            
                ! Check if we are at the final time
                if (solution%t >= t_final .or. &
                    (single_step_mode .and. int())) then
                    break
                endif
            else
                ! Step should be rejected
                if (solver%dt_variable) then
                    ! TODO: Restore backup solution
                    solution%t = t_old
                else
                    status%cfl_max = max(solver%cfl,status%cfl_max)
                    write(s_unit,*) "CFL too large, giving up!"
                    write(s_unit,*) "  CFL = ",solver%cfl
                    write(s_unit,*) "  CFL max = ",solver%cfl_max
                    stop
                endif
            endif
            
            ! Choose new time step if using adaptive time stepping
            if (solver%dt_variable) then
                if (solver%cfl > 0.0_dp) then
                    solver%dt = min(solver%dt_min,solver%dt &
                                    * solver%cfl_desired / solver%cfl)
                    status%dt_min = min(solver%dt,status%dt_min)
                    status%dt_max = max(solver%dt,status%dt_max)
                else
                    solver%dt = solver%dt_max
                endif
            endif
        enddo
        
        ! Check to see if we reached the max_steps rather than t_end
        if (solver%dt_variable .and. (solution%t < t_end) .and. &
            (status%num_steps_taken == max_steps)) then
            write(s_unit,*) "Maximum number of time steps ",max_steps," taken."
            stop 
        endif
    
    end function evolve_to_time

end module solver_module
