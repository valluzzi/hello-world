module strings
    implicit none

    contains

        ! write a subroutine that print one or more strings
        ! can accept a variable number of arguments

        subroutine print(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, &
                            arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20)
            implicit none
            character(len=*), intent(in), optional :: arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10
            character(len=*), intent(in), optional :: arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20
            character(len=3) :: frmt

            ! Set the format string to print each argument
            frmt = "(A)"

            ! Print the optional arguments using the format string
            if (present(arg1)) write(*, "(A)", advance="no") arg1
            if (present(arg2)) write(*, "(A,A)", advance="no") " ", arg2
            if (present(arg3)) write(*, "(A,A)", advance="no") " ", arg3
            if (present(arg4)) write(*, "(A,A)", advance="no") " ", arg4
            if (present(arg5)) write(*, "(A,A)", advance="no") " ", arg5
            if (present(arg6)) write(*, "(A,A)", advance="no") " ", arg6
            if (present(arg7)) write(*, "(A,A)", advance="no") " ", arg7
            if (present(arg8)) write(*, "(A,A)", advance="no") " ", arg8
            if (present(arg9)) write(*, "(A,A)", advance="no") " ", arg9
            if (present(arg10)) write(*, "(A,A)", advance="no") " ", arg10
            if (present(arg11)) write(*, "(A,A)", advance="no") " ", arg11
            if (present(arg12)) write(*, "(A,A)", advance="no") " ", arg12
            if (present(arg13)) write(*, "(A,A)", advance="no") " ", arg13
            if (present(arg14)) write(*, "(A,A)", advance="no") " ", arg14
            if (present(arg15)) write(*, "(A,A)", advance="no") " ", arg15
            if (present(arg16)) write(*, "(A,A)", advance="no") " ", arg16
            if (present(arg17)) write(*, "(A,A)", advance="no") " ", arg17
            if (present(arg18)) write(*, "(A,A)", advance="no") " ", arg18
            if (present(arg19)) write(*, "(A,A)", advance="no") " ", arg19
            if (present(arg20)) write(*, "(A,A)", advance="no") " ", arg20
            
            write(*,*) "" ! Print a blank line
        end subroutine print


end module
