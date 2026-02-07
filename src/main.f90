program main
    use module_strings
    use package
    use check_of_args
    implicit none
    ! This program prints "Hello, World!" to the screen.
    integer :: i, n
    !! Check and validate command line arguments
    if (.not. check_args()) then
        call exit(1)
    end if
    
    print *, "Hello World!"
    print *, "Version: ", version

    
    


end program main
