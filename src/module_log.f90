!! Debug: This level of logging provides detailed information about the program's execution, 
!!        including values of variables, function calls, and control flow. It is usually used during 
!!        development or testing phases.

!! Info: This level of logging provides general information about the program's execution, 
!!       such as the start and end of tasks or significant events. It is often used for monitoring 
!!       or operational purposes.

!! Warning: This level of logging indicates potential issues or unexpected behavior that may 
!!          not be critical but require attention.

!! Error: This level of logging indicates critical errors or failures that require 
!!        immediate attention.

!! Fatal: This level of logging indicates unrecoverable errors that cause 
!!        the program to terminate.
    
module module_log
    
    integer, parameter::DEBUG=0
    integer, parameter::INFO=1
    integer, parameter::WARNING=2
    integer, parameter::ERROR=3
    integer, parameter::FATAL=4
    integer::LOGGING_LEVEL=0
    integer::CURRENT_PROGRESSION=-1
    
contains
    
    subroutine Logger(level, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
        implicit none
        integer, intent(in) ::level
        character(*), intent(in) :: arg1
        character(*), intent(in), optional :: arg2
        character(*), intent(in), optional :: arg3
        character(*), intent(in), optional :: arg4
        character(*), intent(in), optional :: arg5
        character(*), intent(in), optional :: arg6
        character(*), intent(in), optional :: arg7
        character(*), intent(in), optional :: arg8
        character(*), intent(in), optional :: arg9
                
        if (level>=LOGGING_LEVEL) then
            
            if (level==DEBUG) then
                write (*,'(a)',advance='no') "[DEBUG] "
            else if (level==INFO) then
                write (*,'(a)',advance='no') "[INFO] "
            else if (level==WARNING) then
                write (*,'(a)',advance='no') "[WARNING] "
            else if (level==ERROR) then
                write (*,'(a)',advance='no') "[ERROR] "
            endif 
            
            write (*,'(a)',advance='no') arg1
            
            if (present(arg2)) write (*,'(a)',advance='no') arg2
            if (present(arg3)) write (*,'(a)',advance='no') arg3
            if (present(arg4)) write (*,'(a)',advance='no') arg4
            if (present(arg5)) write (*,'(a)',advance='no') arg5
            if (present(arg6)) write (*,'(a)',advance='no') arg6
            if (present(arg7)) write (*,'(a)',advance='no') arg7
            if (present(arg8)) write (*,'(a)',advance='no') arg8
            if (present(arg9)) write (*,'(a)',advance='no') arg9
            write (*,*)""
        endif 
    end subroutine
    
end module
    