!! Copyright (c) 2022 Luzzi Valerio for Gecosistema S.r.l.
!! all rights reserved 
!!
!!
!! Name:        strings.f90
!! Purpose:     
!!
!! Author:      Luzzi Valerio
!!
!! Created:     04/11/2021
module module_strings 
    
    use , intrinsic :: iso_c_binding
    implicit none
        
    interface iif
        module procedure l_iif, i_iif, f_iif, c_iif
    end interface 
    
    interface str
        module procedure b_str, l_str, i_str, f32_str, f64_str, c_str
    end interface

    contains
    
    !!
    !! osname
    !!
    function osname() result(res)
        implicit none
        character(:), allocatable::res
        character(len=32)::os_name
        character(len=200) :: line
        integer :: iostat, unit

        ! Get the operating system name
        call get_environment_variable("OS", os_name)
        res = trim(os_name)
        if (res == "") then
            open(unit=unit, file='/etc/os-release', status='old', action='read', iostat=iostat)
            if (iostat == 0) then
                do
                    read(unit, '(A)', iostat=iostat) line
                    if (iostat /= 0) exit
                    !if (line(1:3) == 'ID=') then
                    !    os_name = trim(line(4:))
                    !    exit
                    !end if
                    if (line(1:12) == 'PRETTY_NAME=') then
                        os_name = strip(trim(line(13:)), '"')    
                        exit
                    end if
                end do
            end if 
            close(unit)
        endif
        res = trim(os_name)
    end function
    
    !!
    !! isWindows
    !!
    function isWindows() result(res)
        logical::res
        character(len=32)::os_name
        ! Get the operating system name
        call get_environment_variable("OS", os_name)
        res = index(os_name, "Win") > 0
    end function
    
    
    !!
    !! get_commandname
    !!
    function get_command_name() result(res)
        character(:), allocatable::res
        integer:: length, status, j, k
        character(255)::arg
        
        call get_command_argument (0, arg, length, status)
        res = iif(status==0,arg(1:length),'')
        
        j = index(res,"\", .true.)
        j = iif(j>0, j+1, 1)
        k = index(res, ".", .true.)
        k = iif(k>0, k-1, length)
        res = res(j:k)

    end function
    
    !!
    !! get_argument
    !!
    function get_argument(j) result(res)
        integer::j
        character(:), allocatable::res
        integer:: length, status
        character(255)::arg
        if (j>0 .and. j<=command_argument_count()) then
            call get_command_argument (j, arg, length, status)
            res = iif(status==0,arg(1:length),'')
        else
            res= ''
        end if 
    end function

    !!
    !! parseInt
    !!
    function parseInt(text, default) result(res)
        character(*),intent(in) :: text
        integer, intent(in), optional :: default
        integer::res
        res = 0
        if(present(default))res=default
        if (len(text)>0) then
            read(text, *)res
        endif
    end function
    
    !!
    !! parseFloat
    !!
    function parseFloat(text, default) result(res)
        character(*), intent(in) :: text
        real(c_double), intent(in), optional :: default
        real(c_double)::res
        res = 0.0_c_double
        if(present(default))res=default
        if (len(text)>0) then
            read(text, *)res
        endif
    end function

    !!
    !! upper
    !!
    function upper(text)
        character(len=*)         ::	text
        character(len=len(text)) ::	upper
	    integer                  ::	i,j

        do i=1,len(text)
            j = ichar (text(i:i))
            if ( (j >= 97) .and. (j <= 122) ) then
               j = j - 32
               upper(i:i) = char(j)
            else
               upper(i:i) = text(i:i)
            endif
        enddo
    end function
    
    !!
    !! lower
    !!
    function lower(text)
        character(len=*)         ::	text
        character(len=len(text)) ::	lower
	    integer                  ::	i,j

        do i=1,len(text)
            j = ichar (text(i:i))
            if ( (j >= 65) .and. (j <= 90) ) then
               j = j + 32
               lower(i:i) = char(j)
            else
               lower(i:i) = text(i:i)
            endif
        enddo
    end function
    
    !!
    !! l_iif - ternary operator on strings
    !!
    function l_iif(cond, a, b) result(res)
        logical, intent(in)::cond
        logical, intent(in)::a
        logical, intent(in)::b
        logical::res
        if (cond) then
            res = a 
        else 
            res = b 
        end if
    end function
    
    !!
    !! i_iif - ternary operator on strings
    !!
    function i_iif(cond, a, b) result(res)
        logical, intent(in)::cond
        integer, intent(in)::a
        integer, intent(in)::b
        integer::res
        if (cond) then
            res = a
        else
            res = b
        end if 
    end function
    
    !!
    !! f_iif - ternary operator on strings
    !!
    function f_iif(cond, a, b) result(res)
        logical, intent(in)::cond
        real, intent(in)::a
        real, intent(in)::b
        real::res
        if (cond) then
            res = a
        else
            res = b
        end if 
    end function
    
    !!
    !! c_iif - ternary operator on strings
    !!
    function c_iif(cond, a, b) result(res)
        logical, intent(in)::cond
        character(*), intent(in)::a
        character(*), intent(in)::b
        character(:), allocatable::res
        if (cond) then
            res = a
        else
            res = b
        end if 
    end function

    
    !!
    !! b_str - convert to string
    !!
    function b_str(x) result(res)
        logical, intent(in)::x
        character(:),allocatable :: res
        res = iif( x, 'T','F')
    end function
    
    !!
    !! l_str - convert to string
    !!
    function l_str(x) result(res)
        integer(kind=8), intent(in)::x
        character(:),allocatable :: res
        character(32) :: tmp
        write(tmp, '(i0)') x
        res = trim(adjustl(tmp))
    end function
    
    !!
    !! i_str - convert to string
    !!
    function i_str(x) result(res)
        integer, intent(in)::x
        character(len=:),allocatable::res
        character(16) :: tmp
        write(tmp, '(i0)') x
        res = trim(adjustl(tmp))
    end function
    
    !!
    !! f32_str - convert to string
    !!
    function f32_str(x) result(res)
        real(c_float), intent(in)::x
        character(:),allocatable :: res
        character(range(x)+2) :: tmp
        write(tmp, '(f20.10)') x
        res = trim(adjustl(tmp))
        res = iif(index(res,'.')>0, strip(res,'0'), res) !trailing and leading zeros
        res = trimr(res, ".")  !trailing .
        res = iif( res=="", "0", res)
    end function

    !!
    !! f64_str - convert to string
    !!
    function f64_str(x) result(res)
        real(c_double), intent(in)::x
        character(:),allocatable :: res
        character(range(x)+2) :: tmp
        write(tmp, '(f20.10)') x
        res = trim(adjustl(tmp))
        res = iif(index(res,'.')>0, strip(res,'0'), res) !trailing and leading zeros
        res = trimr(res, ".")  !trailing .
        res = iif( res=="", "0", res)
    end function
    
    !!
    !! c_str - convert to string
    !!
    function c_str(x) result(res)
        character(*), intent(in)::x
        character(:),allocatable :: res
        res = x
    end function
    
    !!
    !! triml 
    !!
    function triml(text, prefix) result(res)
        character(*),intent(in) :: text
        character(*),intent(in), optional :: prefix
        character(:),allocatable::c,res 
        integer::j,b,e,l
        c = ' '
        if(present(prefix)) c = prefix
        e = len(text)
        l = len(c)
        b = 1

        do j=1,e-l+1,1
            if (text(j:j+l-1)==c) then
               b = b + l
            else
               exit
            end if
        end do
        res = text(b:e)
    end function
    
    !!
    !! trimr 
    !!
    function trimr(text, suffix) result(res)
        character(*),intent(in) :: text
        character(*),intent(in), optional :: suffix
        character(:),allocatable::c,res 
        integer::j,e,l
        c = ' '
        if(present(suffix)) c=suffix 
        e = len(text)
        l = len(c)

        do j=e-l+1,1,-1
            if (text(j:j+l-1)==c) then
               e = e - l
            else
               exit
            end if
        end do
        res = text(1:e)
    end function
    
    !!
    !! strip 
    !!
    function strip(text, prefix, suffix) result(res)
        character(*),intent(in) :: text
        character(*),intent(in), optional :: prefix,suffix
        character(:),allocatable::c,s,res 
        if(present(prefix)) then 
            s = prefix
        else 
            s = ' ' 
        end if 
        if(present(suffix)) then
            c = suffix
        else 
            c = s
        end if 
        res = triml(trimr(text,c),s)
    end function
    
    
    !!
    !! f -  format strings
    !!
    function f(frmt, arg1) result(res)
        character(*),intent(in) :: frmt
        real,intent(in) :: arg1
        character(len=80)::text 
        character(:), allocatable::res
        write( text, frmt) arg1
        res = trim(text)
    end function
    
    !!
    !!  mcount - count max length of occurrence
    !!
    function mcount(text, c) result(res)
        character(*),intent(in) :: text
        character(*),intent(in) :: c
        integer::res(2)
        integer::j, ccount, wmax, wcount
        ccount=0;wcount=0; wmax=0
        
        do j = 1,len(text) 
            if (text(j:j+len(c)-1)==c)then
                wmax = max(wcount, wmax)
                wcount=0
                ccount = ccount+1
            else
                wcount = wcount+1
                wmax = max(wcount, wmax)
            endif
        end do
        
        res = (/ccount, wmax/)
    end function

    !!
    !!  splitx
    !!
    function splitx(text, sep, trim, remove_empty) result(res)
        character(*),intent(in) :: text
        character(*),intent(in) :: sep
        logical,optional,intent(in) :: trim, remove_empty
        logical::trim_item = .false.
        logical::remove_empty_item = .false.
        integer::arr(2)
        integer::i, j, n, w, lentxt, lenc, m=0
        character(:),allocatable::word 
        character(:),allocatable::res(:), temp(:)
        
        if (present(trim)) trim_item = trim
        if (present(remove_empty)) remove_empty_item = remove_empty
        i=1;arr=mcount(text,sep);lenc = len(sep);lentxt=len(text)
        n = arr(1)+1;w = arr(2)
        allocate(character(w)::res(n))
        word = ""
        do j=1,lentxt
            if (text(j:j+lenc-1)==sep .or. j==lentxt) then
                if (j==lentxt.and.text(j:j+lenc-1).ne.sep ) word = word//text(j:j)
                if (trim_item) word = strip(word)
                res(i) = word
                i=i+1
                m = m + iif(remove_empty_item.and.len(word)==0,0,1)
                word=""
            else
                word = word//text(j:j)
            endif
        end do
        
        if (m<n) then
            allocate(character(w)::temp(n))
            temp(:) = res(:)
            deallocate(res)
            allocate(character(w)::res(m))
            j=0
            do j=1,n
                if (len(temp(j))>0) then
                    res(i)= temp(j)
                    i=i+1
                end if
            end do
            deallocate(temp)
        endif
    end function
end module module_strings