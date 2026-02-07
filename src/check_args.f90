!! Copyright (c) 2022 Luzzi Valerio for Gecosistema S.r.l.
!! all rights reserved  
!!
!! Name:        main.f90
!! Purpose:     
!!
!! Author:      Luzzi Valerio
!!
!! Created:     04/11/2021
module check_of_args
    use module_strings
    use module_filesystem
    use module_click
    use module_log
    use package
    implicit none
    
    !! Option type definition
    type :: option_t
        character(len=20) :: flag
        character(len=20) :: arg_type
        character(len=200) :: description
        logical :: required
    end type option_t
    
    !! List of all available options
    type(option_t), parameter :: OPTIONS(*) = [ &
        option_t("--dem",     "PATH",       "DEM raster file. Horizontal and vertical units must be meters",                  .true.),  &
        option_t("--rain",    "FLOAT|PATH", "Uniform rain intensity [mm/h] or raster rain file",                              .false.), &
        option_t("--esl",     "FLOAT",      "Extreme sea level relative to duration (--d)",                                   .false.), &
        option_t("--d",       "FLOAT",      "Duration of rain or esl event [s] (default: 3600)",                              .false.), &
        option_t("--ti",      "INT",        "Output interval [s] (e.g., 3600 = 1 snapshot per hour)",                         .false.), &
        option_t("--tmax",    "INT",        "Simulation stop time [s] (default: 3600)",                                       .false.), &
        option_t("--delt",    "INT",        "Time step delta [s] (default: 600)",                                             .false.), &
        option_t("--nl",      "INT",        "Number of pixels per cell side (default: 100)",                                  .false.), &
        option_t("--man",     "FLOAT",      "Manning coefficient (default: 0.02)",                                            .false.), &
        option_t("--force",   "",           "Force preprocessing even if .grd file exists",                                   .false.), &
        option_t("--tmp",     "PATH",       "Temporary directory",                                                            .false.), &
        option_t("--out",     "PATH",       "Water depth output raster (default: <dem>.wd@<time>.tif)",                       .false.), &
        option_t("--max",     "PATH",       "Maximum water depth output raster",                                              .false.), &
        option_t("--seamask", "PATH",       "Sea mask raster (1=sea, 0=land)",                                                .false.), &
        option_t("--verbose", "",           "Enable verbose output",                                                          .false.), &
        option_t("--version", "",           "Display version information",                                                    .false.), &
        option_t("--debug",   "",           "Enable debug mode with detailed output",                                         .false.), &
        option_t("--help",    "",           "Display this help message",                                                      .false.)  &
    ]
    
    contains
    !!
    !!  welcome
    !!
    subroutine welcome
        implicit none
            
        call echo("")
        call echo("+-----------------------------------------------------------------------------+")
        call echo("|                                                                             |")
        call echo("|    Program "//get_command_name()//".exe v"//trim(version)//" on "//osname()//"                                 |")
        call echo("|    Mathematical model by Vincenzo Casulli, University of Trento (Italy)     |")
        call echo("|    Command line interface by Stefano Bagli, Gecosistema S.r.l. (Italy)      |")
        call echo("|                              Valerio Luzzi, Gecosistema S.r.l. (Italy)      |")
        call echo("|                              Tommaso Redaelli, Gecosistema S.r.l. (Italy)   |")
        call echo("|                                                                             |")
        call echo("|    copyright 2021-2026 all rights reserved                                  |")
        call echo("|                                                                             |")
        call echo("+-----------------------------------------------------------------------------+")
    end subroutine
        
    !!
    !! show_version
    !!
    subroutine show_version
        implicit none
        print *, get_command_name()//" "//trim(version)
    end subroutine
    
    !!
    !!  help
    !!
    subroutine help
        use module_log
        implicit none
        integer :: i
        character(len=30) :: flag_part
        character(len=250) :: description
        
        call echo("")
        call echo("Version: " // version)
        call echo("Usage: " // get_command_name() // " [OPTIONS]")
        call echo("")
        call echo("Options:")
        
        !! Print all options from the list
        do i = 1, size(OPTIONS)
            !! Build the flag + type part
            if (len_trim(OPTIONS(i)%arg_type) > 0) then
                write(flag_part, '(A,1X,A)') trim(OPTIONS(i)%flag), trim(OPTIONS(i)%arg_type)
            else
                flag_part = trim(OPTIONS(i)%flag)
            end if
            
            !! Add (Mandatory) suffix if required
            description = trim(OPTIONS(i)%description)
            if (OPTIONS(i)%required) then
                description = trim(description) // " (Mandatory)"
            end if
            
            !! Print with word wrapping at 120 chars
            call print_wrapped_option(flag_part, description)
        end do
        
        call echo("")
        call echo("Examples:")
        call echo("  " // get_command_name() // " --dem elevation.tif --rain 50 --d 7200")
        call echo("  " // get_command_name() // " --dem input.asc --esl 1.5 --tmax 10800 --verbose")
    
    end subroutine
    
    !!
    !!  print_wrapped_option - Print option with word wrapping at 120 chars
    !!
    subroutine print_wrapped_option(flag, description)
        use module_log
        implicit none
        character(len=*), intent(in) :: flag, description
        character(len=512) :: line
        integer, parameter :: MAX_WIDTH = 119  ! 120 chars total including newline
        integer, parameter :: INDENT = 2
        integer, parameter :: FLAG_WIDTH = 25
        integer, parameter :: DESC_START = INDENT + FLAG_WIDTH
        integer :: desc_width, pos, last_space, line_start, line_end
        character(len=250) :: remaining
        logical :: first_line
        
        desc_width = MAX_WIDTH - DESC_START
        remaining = description
        first_line = .true.
        
        do while (len_trim(remaining) > 0)
            if (first_line) then
                !! First line: includes flag
                if (len_trim(remaining) <= desc_width) then
                    !! Fits on one line
                    write(line, '(A2,A25,A)') "  ", flag, trim(remaining)
                    call echo(trim(line))
                    exit
                else
                    !! Need to wrap - find last space before width limit
                    last_space = 0
                    do pos = 1, min(desc_width, len_trim(remaining))
                        if (remaining(pos:pos) == ' ') last_space = pos
                    end do
                    
                    if (last_space == 0) last_space = desc_width  !! No space found, hard break
                    
                    write(line, '(A2,A25,A)') "  ", flag, remaining(1:last_space)
                    call echo(trim(line))
                    remaining = adjustl(remaining(last_space+1:))
                    first_line = .false.
                end if
            else
                !! Continuation lines: indent to align with description
                if (len_trim(remaining) <= desc_width) then
                    !! Last line
                    write(line, '(A27,A)') " ", trim(remaining)
                    call echo(trim(line))
                    exit
                else
                    !! Find last space before width limit
                    last_space = 0
                    do pos = 1, min(desc_width, len_trim(remaining))
                        if (remaining(pos:pos) == ' ') last_space = pos
                    end do
                    
                    if (last_space == 0) last_space = desc_width
                    
                    write(line, '(A27,A)') " ", remaining(1:last_space)
                    call echo(trim(line))
                    remaining = adjustl(remaining(last_space+1:))
                end if
            end if
        end do
        
    end subroutine
        
    
    !!
    !!  is_valid_option - Check if a flag is in the OPTIONS list
    !!
    function is_valid_option(flag) result(valid)
        implicit none
        character(len=*), intent(in) :: flag
        logical :: valid
        integer :: i
        
        valid = .false.
        do i = 1, size(OPTIONS)
            if (trim(OPTIONS(i)%flag) == trim(flag)) then
                valid = .true.
                return
            end if
        end do
    end function
    
    !!
    !!  has_option - Check if a flag is present in command line arguments
    !!
    function has_option(flag) result(found)
        implicit none
        character(len=*), intent(in) :: flag
        logical :: found
        integer :: i, n
        character(:), allocatable :: arg
        
        found = .false.
        n = command_argument_count()
        
        do i = 1, n
            arg = get_argument(i)
            if (trim(arg) == trim(flag)) then
                found = .true.
                return
            end if
        end do
    end function has_option
    
    !!
    !!  validate_unknown_options - Check for unrecognized command line options
    !!
    subroutine validate_unknown_options()
        implicit none
        integer :: i, n
        character(:), allocatable :: arg
        logical :: is_value
        
        n = command_argument_count()
        is_value = .false.
        
        do i = 1, n
            arg = get_argument(i)
            
            if (is_value) then
                !! This argument is a value for the previous flag
                is_value = .false.
                cycle
            end if
            
            !! Check if it's a flag (starts with --)
            if (len_trim(arg) > 2) then
                if (arg(1:2) == "--") then
                    if (.not. is_valid_option(trim(arg))) then
                        call echo("Error: Unknown option: " // trim(arg))
                        call echo("Use --help to see available options")
                        call exit(1)
                    end if
                    
                    !! Check if this option expects a value
                    if (option_expects_value(trim(arg))) then
                        is_value = .true.
                    end if
                end if
            end if
        end do
    end subroutine validate_unknown_options
    
    !!
    !!  option_expects_value - Check if an option expects a value argument
    !!
    function option_expects_value(flag) result(expects)
        implicit none
        character(len=*), intent(in) :: flag
        logical :: expects
        integer :: i
        
        expects = .false.
        do i = 1, size(OPTIONS)
            if (trim(OPTIONS(i)%flag) == trim(flag)) then
                !! Options with arg_type expect a value
                expects = (len_trim(OPTIONS(i)%arg_type) > 0)
                return
            end if
        end do
    end function option_expects_value
    
    !!
    !!  check_required_options - Verify all required options are present
    !!
    function check_required_options() result(all_present)
        implicit none
        logical :: all_present
        integer :: i
        logical :: missing_found
        
        all_present = .true.
        missing_found = .false.
        
        do i = 1, size(OPTIONS)
            if (OPTIONS(i)%required) then
                if (.not. has_option(trim(OPTIONS(i)%flag))) then
                    if (.not. missing_found) then
                        call echo("")
                        call echo("Error: Missing required options:")
                        missing_found = .true.
                    end if
                    call echo("  " // trim(OPTIONS(i)%flag) // " - " // trim(OPTIONS(i)%description))
                    all_present = .false.
                end if
            end if
        end do
        
        if (.not. all_present) then
            call echo("")
            call echo("Use --help for usage information")
        end if
    end function check_required_options
    
    !!
    !!  check_args - Main argument validation entry point
    !!
    function check_args() result(res)
        implicit none
        logical :: res
        
        res = .true.
        
        !! Handle --help first (highest priority)
        if (has_option("--help") .or. has_option("-h")) then
            call help
            call exit(0)
        end if
        
        !! Handle --version
        if (has_option("--version") .or. has_option("-v")) then
            call show_version
            call exit(0)
        end if
        
        !! If no arguments provided, show help
        if (command_argument_count() == 0) then
            call welcome
            call help
            call exit(0)
        end if
        
        !! Validate unknown options
        call validate_unknown_options()
        
        !! Check if all required options are present
        if (.not. check_required_options()) then
            res = .false.
            call exit(1)
        end if
    
    end function check_args
    
end module check_of_args