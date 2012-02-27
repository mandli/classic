module solution_module

    implicit none

    type solution
    
        ! Grid parameters
        integer :: n(1),num_states,num_aux
        real(kind=gp) :: lower(1), upper(1), delta(1)

        ! State arrays
        real(kind=qp), allocatable :: q(:,:)
        real(kind=ap), allocatable :: aux(:,:)
    
    end type solution

contains
    
    type(solution) function new_solution(claw_params)
        implicit none
        
        ! Parameter derived type from claw.data file
        type(claw_parameters) :: claw_params
    
        ! Error return flag
        integer :: err
        
        ! Put input arguments into new solution
        new_solution%n(1) = claw_params%n(1)
        new_solution%num_states = claw_params%num_states
        new_solution%num_aux = claw_params%num_aux
        new_solution%lower(1) = claw_params%lower(1)
        new_solution%upper(1) = claw_params%upper(1)
        new_solution%delta(1) = real((upper(1) - lower(1)) / n(1),kind=gp)
        new_solution%t = claw_params%t_init
        
        ! Allocate state arrays
        allocate(new_solution%q(num_states,1:n(1)), stat=err)
        if (err /= 0) stop "Allocation of q array unsuccesful!"
        allocate(new_solution%aux(num_aux,1:n(1)), stat=err)
        if (err /= 0) stop "Allocation of aux array unsuccesful!"
        
    end function new_solution

end module solution_module