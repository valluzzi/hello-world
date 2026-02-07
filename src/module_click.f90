!! Copyright (c) 2022 Luzzi Valerio for Gecosistema S.r.l.
!! all rights reserved  
!!
!! Name:        module_click.f90
!! Purpose:     
!!
!! Author:      Luzzi Valerio
!!
!! Created:     15/02/2022


module module_click
    use module_strings
    use module_filesystem
    use module_log
    implicit none
    contains
   
    !!
    !!  echo
    !!
    subroutine echo(text)
        implicit none 
        character(*), intent(in)::text
        print *, text
    end subroutine
    
    !!
    !!  WarnIfNotExists
    !!
    subroutine WarnIfNotExists(filename)
        implicit none 
        character(*), intent(in)::filename
        if (.not.isfile(filename))then
            ! print *, "Warning!:<", filename , "> does not exists!"
            call Logger(WARNING, "<", filename, "> does not exits!")
        endif
    end subroutine
    
    !!
    !!  AssertFileExists
    !!
    subroutine AssertFileExists(filename)
        implicit none 
        character(*), intent(in)::filename
        if (.not.isfile(filename))then
            write(*,"(A)") "Exception:", filename , " does not exists!"
            call exit(1)
        endif
    end subroutine
    
    !!
    !!  get_flag - Check if a boolean flag is present in command line
    !! 
    function get_flag(name, default) result(res)
        implicit none
        character(*), intent(in)::name
        logical, optional, intent(in)::default
        logical :: res
        integer::j,n
       
        res = .false.
        if (present(default)) res=default
        n = command_argument_count() 
        do j = 1, n
            !! Support both --name and -name formats
            if (get_argument(j)=="--"//name .or. get_argument(j)=="-"//name) then
               res = .true.
               exit
            end if
        end do 
    end function
    
    !!
    !!  get_path
    !!
    function get_path(name, default, exists) result(res)

        implicit none
        character(*), intent(in)::name 
        character(*), optional, intent(in)::default
        logical, optional, intent(in)::exists
        character(:), allocatable :: res, default_value
        integer::j,n
        logical::must_exists
        res = '' 
        must_exists=.false.
        default_value=''
        if (present(exists)) must_exists=exists
        if (present(default)) default_value=default
        n = command_argument_count() 
        do j = 1, n
            if (get_argument(j)=="--"//name .or. get_argument(j)=="-"//name) then
                if (j+1<=n) then
                    res = trim(get_argument(j+1))                
                    exit
                end if
            end if
        end do
        res = iif(len_trim(res)>0,res,default_value)
        if ( must_exists .and. .not.isfile(res)) then
            print *, "Warning!", name, "<", res, "> is missing"
            res = ''
        end if 
    end function
    
    !!
    !!  get_opt
    !!
    function get_opt(name, default) result(res)
        implicit none
        character(*), intent(in)::name
        character(*), optional,intent(in)::default
        character(:), allocatable :: res
        integer::j,n
        character(:), allocatable :: default_value
        res = '' 
        default_value = ''
        if (present(default)) default_value=default
        n = command_argument_count() 
        do j = 1, n
            if (get_argument(j)=="--"//name .or. get_argument(j)=="-"//name) then
                if (j+1<=n) then
                    res = trim(get_argument(j+1))                   
                else
                    res = default_value
                end if
                return
            end if
        end do
        res = default
    end function
    
    !!
    !!  get_list
    !!
    function get_list(name) result(res)
        implicit none
        character(*), intent(in)  :: name
        character(:), allocatable :: res(:)
        character(:), allocatable :: item, items
        integer::j,j1,n
        items = '' 
        n = command_argument_count() 
        do j = 1, n
            if (get_argument(j)=="--"//name .or. get_argument(j)=="-"//name) then
                j1 = j+1
                do while (j1<=n)
                    item =trim(get_argument(j1))
                    if (item(1:2)=="--") exit
                    if (len(trim(item))>0) items = trim(items//","//strip(item,",")) 
                    j1=j1+1
                enddo
                exit
            end if
        end do
        items = strip(items,",")
        res = splitx(items, ",", .true., .true.)
    end function
    
    
    !!
    !!  get_float_list
    !!
    function get_float_list(name) result(res)
        implicit none
        character(*), intent(in)  :: name
        character(:), allocatable :: items(:)
        real, allocatable :: res(:)
        integer::j,n
        items = get_list(name)
        n = size(items)
        allocate(real::res(n))
        do j=1,n
            res(j) = parseFloat(items(j))
        end do
        deallocate(items)
    end function
    
    !!
    !!  get_int
    !!
    function get_int(name, default) result(res)
        implicit none
        character(*), intent(in)::name
        integer, intent(in)::default
        integer::res
        character(:),allocatable::opt
        opt = get_opt(name, ""//str(default))
        res = parseInt(opt)
    end function 
    
    !!
    !!  get_float
    !!
    function get_float(name, default) result(res)
        implicit none
        character(*), intent(in)::name
        real, intent(in)::default
        real :: res
        character(:),allocatable::opt
        opt = get_opt(name, ""//str(default))
        res = parseFloat(opt)
    end function 
    
end module module_click

