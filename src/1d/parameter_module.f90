module parameter_module

    use precision_module

    implicit none
    save
    
    ! Solution  parameters
    integer :: num_dim
    integer, allocatable :: n(:)
    integer :: num_state,num_aux,capa_index
    real(kind=dp) :: t_init
    real(kind=dp), allocatable :: lower(:),upper(:)
    
    ! Output parameters
    logical :: single_step_mode
    integer :: num_steps_to_output, num_steps_final
    integer, allocatable :: out_times(:)
    
    ! Method parameters
    real(kind=dp) :: dt_init,dt_max,cfl_max,cfl_desired 
    integer :: max_steps_per_output
        
    ! Parameters for method
    integer :: dt_variable,order,trans_order,verbosity,source_method
    integer, allocatable :: limiter_method(:)
    integer :: num_ghost
    integer, allocatable :: lower_bc_method(:),upper_bc_method(:)
    
contains

    type(claw_parameters) function read_claw_parameters(parameter_file) result(params)
    
        implicit none
        
        ! Input file
        character(len=*), intent(in) :: parameter_file
        
        ! Locals
        integer, parameter :: param_unit = 10
        integer :: i
        integer :: num_out,out_style,num_step_out,num_waves
        real(kind=dp) :: t_final,dt_out
        
        ! Open and fast forward past header
        call open_data_file(param_unit,parameter_file)
        
        ! Number of space dimensions
        read(param_unit,"(I1)") params%num_dim
        
        ! Allocate all the rest of the parameter dimensions based on num_dim
        allocate(params%n(params%num_dim))
        do i=1,params%num_dim
            read(param_unit,"(I5)") params%n(i)
        enddo
        
        ! Output variables
        read(param_unit,"(I5)") num_out
        allocate(params%out_times(nout))
        read(param_unit,"(I5)") out_style
        select case(out_style)
            case(1)
                params%single_step_mode = .false.
                read(param_unit,"(F16.8)") t_final
                dt_out = (t_final - t_init) / float(num_out)
                params%out_times = [t_init + dt_out * i, i=0,num_out]
            case(2)
                params%single_step_mode = .false.
                read(param_unit,*) (params%out_times(i), i=1,nout)
            case(3)
                params%single_step_mode = .true.
                read(param_unit,*) num_steps_to_output, num_steps_final
                do i=1,num_steps_final,num_steps_to_output
                    params%out_times(i) = num_steps_to_output
                enddo
            case default
                print *, "Invalid outstyle = ",out_style
        end select case
        
        ! Time stepping control parameters
        read(param_unit,"(F16.8)") params%dt_init
        read(param_unit,"(F16.8)") params%dt_max
        read(param_unit,"(F16.8)") params%cfl_max
        read(param_unit,"(F16.8)") params%cfl_desired 
        read(param_unit,"(I5)") params%max_steps_per_output
        
        ! Parameters for method
        read(param_unit,"(I1)") params%dt_variable
        read(param_unit,"(I1)") params%order
        read(param_unit,"(I1)") params%trans_order
        read(param_unit,"(I5)") params%verbosity
        read(param_unit,"(I5)") params%source_method
        read(param_unit,"(I5)") params%capa_index
        read(param_unit,"(I5)") params%num_aux
        
        ! State and solver dimensions
        read(param_unit,"(I2)") params%num_states
        read(param_unit,"(I2)") num_waves
        allocate(limiter_method(num_waves))
        read(param_unit,"(I1)") (params%limiter_method(i), i=1,num_waves)
        
        ! Initial time
        read(param_unit,"(F16.8)") params%t_init
        
        ! Grid dimensions
        allocate(lower(num_dim))
        allocate(upper(num_dim))
        do i=1,num_dim
            read(param_unit,"(F16.8)") params%lower(i)
            read(param_unit,"(F16.8)") params%upper(i)
        enddo
        
        ! Number of ghost cells
        read(param_unit,"(I2)") params%num_ghost
        ! Boundary condition method
        allocate(params%lower_bc_method(num_dim))
        allocate(params%upper_bc_method(num_dim))
        do i=1,num_dim
            read(param_unit,"(I2)") params%lower_bc_method(i)
            read(param_unit,"(I2)") params%upper_bc_method(i)
        enddo
        
        ! Check for consistency of parameters
        
    end function read_claw_parameters

end module parameter_module
