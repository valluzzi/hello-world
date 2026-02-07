!! Copyright (c) 2022 Luzzi Valerio for Gecosistema S.r.l.
!! all rights reserved 
!!
!!
!! Name:        filesystem.f90
!! Purpose:     
!!
!! Author:      Luzzi Valerio
!!
!! Created:     02/11/2021
    
module module_filesystem
    use module_strings
    implicit none
    
    contains
    
    !!
    !! pwd- return the current workdir
    !!
    function pwd() result(res)
        character(:),allocatable:: res 
        character*255 dirname
        call getcwd(dirname)
        res = trim(dirname)
    end function
    
    !!
    !! normpath
    !!
    function normpath(pathname) result(res)
        implicit none
        character(*),intent(in) :: pathname
        character(:),allocatable:: res 
        integer::i
        res = pathname(:)
        do i=1,len(res)
            if (res(i:i)=="/") res(i:i)="\"
        enddo
    end function 
    
    
    !!
    !! mkdir
    !!
    ! function mkdir(pathname) result(res)
    !     implicit none
    !     character(*),intent(in) :: pathname
    !     logical::res
    !     call system("mkdir "//normpath(pathname)//" 2>nul")
    !     res = .true. 
    ! end function   
    subroutine mkdir(pathname)
        implicit none
        character(*),intent(in) :: pathname
        call system("mkdir "//normpath(pathname)//" 2>nul")
    end subroutine 
    
    !!
    !! forceext 
    !!
    function forceext(filename, ext) result(res)
        implicit none
        character(*),intent(in) :: filename
        character(*),intent(in) :: ext
        character(:),allocatable:: res 
        integer::i
        i = index(filename,".", .true.)
        i = iif(i>0, i-1, len(filename))
        res = normpath(filename(:i))//'.'//ext
    end function
    
    !!
    !!  justext
    !!
    function justext(filename) result(res)
        implicit none
        character(*),intent(in) :: filename
        character(:),allocatable:: res 
        integer::i
        i = index(filename,".", .true.)
        res = ""
        if (i>0) res = trim(filename(i+1:))
    end function
    
    !!
    !! justpath 
    !!
    function justpath(filename) result(res)
        implicit none
        character(*),intent(in) :: filename
        character(:),allocatable:: res
        integer::i
        res = normpath(filename)
        i = index(res,'\', .true.)
        res = iif(i>0,res(:i),".")
    end function
    
    !!
    !! justfname 
    !!
    function justfname(filename) result(res)
        character(*),intent(in) :: filename
        character(:),allocatable:: res
        integer::i
        res = normpath(filename)
        i = index(res,'\', .true.)
        res = res(i+1:)
    end function
    
    !!
    !! juststem
    !!
    function juststem(filename) result(res)
        character(*),intent(in) :: filename
        character(:),allocatable:: res
        integer::i
        res = justfname(filename)
        i = index(res,'.', .true.)
        i = iif(i>0,i-1,len(res))
        res = res(:i)
    end function
    
    !!
    !! startswith 
    !!
    function startswith(text, prefix) result(res)
        implicit none
        character(*),intent(in) :: text
        character(*),intent(in) :: prefix
        logical::res
        res =  index(text, prefix) == 1
    end function
    
    !!
    !! endswith 
    !!
    function endswith(text, suffix) result(res)
        implicit none
        character(*),intent(in) :: text
        character(*),intent(in) :: suffix
        logical:: res
        res =  (text(len(text)-len(suffix)+1:) == suffix)
    end function
        
    !!
    !! unit - get the first free file logical unit
    !!
    function unit() result(res)
        implicit none
        integer::u = 11
        integer::res
        character(:),allocatable::filename
        logical::openedq
        openedq =.true.
        do while (openedq)
            inquire(unit=u, opened=openedq)
            !!print *, 'unit #', u, 'is busy?', openedq
            u = u + 1
        end do
        res = u 
    end function
    
    !!
    !! isfile - return true if the file exists
    !!
    function isfile(filename) result(res)
        implicit none
        character(*), intent(in)::filename
        character(:), allocatable::nfilename
        logical :: res
        res = .false.
        nfilename = normpath(filename)
        
        if (len(nfilename)>0) then
            inquire(file=nfilename, exist=res)
        endif
    end function
    
    !!
    !! isdir - return true if the directory exists
    !!
    function isdir(pathname) result(res)
        implicit none
        character(*), intent(in)::pathname
        character(:), allocatable::npathname
        logical :: res
        res = .false.
        npathname = normpath(pathname)
        if (len(npathname)>0) then
            inquire(file=trim(npathname)//'/.', exist=res)
        endif
    end function
    
    !!
    !! israster - return true if the file exists and ext==tif
    !!
    function israster(filename) result(res)
        implicit none
        character(*), intent(in)::filename
        character(:), allocatable::nfilename
        logical :: res
        res = .false.
        nfilename = normpath(filename)
        if (endswith(nfilename,".tif").or.endswith(nfilename,".asc")) then
            inquire(file=nfilename, exist=res) 
        endif
    end function
    
    !!
    !! str2file - create a file with text
    !!
    subroutine str2file(text, filename, append)
        implicit none
        character(*), intent(in)::text
        character(*), intent(in)::filename
        logical,  optional::append 
        logical:: append_mode
        integer::u
        append_mode = .false.
        if(present(append)) append_mode = append
        u = unit()
        if (.not.append_mode) then
            open(u, file=filename, status='replace')
        else
            if (.not.isfile(filename)) then 
                open(u, file=filename, status='new', action='write')
            else
                open(u, file=filename, status='old', position='append', action='write')
            end if 
        end if 
        write(u,'(a)',advance='no')text
        close(u)
    end subroutine 
    
    !!
    !! filesize - return the numer of bytes of filename
    !!
    function filesize(filename) result(res)
        implicit none
        character(*), intent(in)::filename
        integer::res,sz
        inquire(file=filename, size=sz)
        res=sz
    end function
    
    !!
    !! file2str - read a file and return content
    !!
    function file2str(filename) result(res)
        implicit none
        character(*), intent(in)::filename
        character(:), allocatable::res
        integer::u,n
        
        n = filesize(filename)
        if (n>0) then
            allocate( character(n)::res )
            u = unit()
            open(u, file=filename, access="direct", recl=n, form='unformatted')
            read(u, rec=1)res
            close(u)
        else
            res = ''
        end if 
    end function
    
    !!
    !! cp - copy a file
    !! inspired from https://stackoverflow.com/questions/5169295/how-to-copy-a-file-in-fortran-90
    !! NOTE: Windows-only implementation - disabled for Unix/macOS compatibility
    !!
    ! subroutine cp(src, dst)
    !     use kernel32,only:CopyFile,FALSE 
    !     implicit none  
    !     integer::ret  
    !     character*(*), intent(in) :: src, dst  
    !     ret = CopyFile(trim(src)//""C, trim(dst)//""C, FALSE)  
    ! end subroutine 
    
    !!
    !! rm - remove a file
    !!
    subroutine rm(filename)
        implicit none
        character(*), intent(in)::filename
        integer::u
        u = unit()
        open(u, file=filename)
        close(u, status="delete")
    end subroutine
   
    
    !!
    !! gettempdir - returns the /tmp 
    !!
    function gettempdir() result(res)
        ! use, intrinsic :: iso_c_binding
        implicit none
        ! logical :: ret
        character(:), allocatable::res

        ! NOTE: Simplified for Unix/macOS - removed Windows GetTempPathA dependency
        ! interface
        !     ! Declare the interface to the GetTempPath function
        !     function GetTempPathA(nBufferLength, lpBuffer) bind(C, name="GetTempPathA")
        !         import :: c_int, c_char
        !         integer(c_int), value :: nBufferLength
        !         character(c_char) :: lpBuffer(*)
        !         integer(c_int) :: GetTempPathA
        !     end function GetTempPathA
        ! end interface
        
        character(len=261) :: tempPath  ! Max path length + 1 for null terminator
        ! integer :: pathLength

        if (isWindows()) then
            ! Call the function
            ! pathLength = GetTempPathA(len(tempPath), tempPath)

            ! if (pathLength > 0 .and. pathLength <= len(tempPath)) then
            !     tempPath = tempPath(1:pathLength)
            ! else
            !     tempPath = "c:\Temp"
            ! end if
            tempPath = "c:\Temp"
            call mkdir(tempPath)  ! Ensure the temp directory exists
        else 
            tempPath = "/tmp/"
        end if

        res = trim(tempPath)
        
    end function
    
    
    
end module module_filesystem