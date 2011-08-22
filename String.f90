MODULE STRING
    IMPLICIT NONE
    PUBLIC
CONTAINS
    !----------------------------------------------------------------------
    ! Returns the number of arguments on the il-th line in file iunit.
    !
    integer function largc(iunit,il)

        INTEGER, INTENT(IN) :: iunit, il
        INTEGER :: iread,ichar,last_char,num_arg

        CHARACTER(LEN=128) string

        num_arg=0
        rewind(iunit)    
        do iread=1,il
           read(iunit,'(A)') string
        enddo
        last_char=last_nblank(string)
        if(string(1:1) .ne. ' ') num_arg=num_arg+1
        do ichar=1,last_char-1
           if(string(ichar:ichar)    .eq.' '   .AND.  &
                string(ichar+1:ichar+1).ne.' ') num_arg=num_arg+1
        enddo
        largc=num_arg
        return
    end function largc

    !----------------------------------------------------------------------
    ! Returns argument "iarg" from line number "line".
    !
    subroutine lgetarg(iunit,il,iarg,arg)
        IMPLICIT NONE

        INTEGER, INTENT(IN) :: iunit, il, iarg

        INTEGER :: k,kk,ichar,iread,iend,num_arg
        INTEGER :: first_char, last_char

        CHARACTER(len=128) ::  string
        character*(*) arg

        do k=1,len(arg)
           arg(k:k)=' '
        enddo

        num_arg=0
        rewind(iunit)    
        do iread=1,il
           read(iunit,'(A)') string
        enddo
        last_char=last_nblank(string)

        if(string(1:1) .ne. ' ') then    !Check if first arg is desired
           num_arg=num_arg+1
           if(num_arg.eq.iarg) then
              first_char=1
              iend=0
              do k=first_char,last_char
                 if(string(k:k).eq.' '.AND.iend.eq.0) iend=k-1
              enddo
              do k=first_char,iend       !Copy string(first_char:iend) to arg
                 kk=k+1-first_char
                 arg(kk:kk)=string(k:k)
              enddo
              return
           endif
        endif

        do ichar=1,last_char-1        
           if(string(ichar:ichar)    .eq.' '   .AND.  &
                string(ichar+1:ichar+1).ne.' ')  then

              num_arg=num_arg+1        !Search for desired arg #
              if(num_arg.eq.iarg) then
                 first_char=ichar+1
                 iend=0
                 do k=first_char,last_char
                    if(string(k:k).eq.' ' .AND. iend.eq.0) iend=k-1
                 enddo
                 if (iend.eq.0) iend=last_char
                 do k=first_char,iend    !Copy string(first_char:iend) to arg
                    kk=k+1-first_char
                    arg(kk:kk)=string(k:k)
                 enddo
                 return
              endif

           endif
        enddo

        return
    end subroutine lgetarg

    !----------------------------------------------------------------------
    ! Returns the last non-blank character  of "string" 
    !
    integer function last_nblank(string)
        IMPLICIT NONE

        character*(*) string
        INTEGER :: i

        do i=len(string),1,-1
           if (string(i:i) .ne. ' ') then
              last_nblank=i
              return
           endif
        enddo
        last_nblank=len(string)
        return
    end function last_nblank

    !----------------------------------------------------------------------
    !       Converts a character to a digit
    !
    integer function num(string)
        IMPLICIT NONE
        character*(*) string

        if (string .eq. '0') then
           num = 0
        elseif (string .eq. '1') then
           num = 1
        elseif (string .eq. '2') then
           num = 2
        elseif (string .eq. '3') then
           num = 3
        elseif (string .eq. '4') then
           num = 4
        elseif (string .eq. '5') then
           num = 5
        elseif (string .eq. '6') then
           num = 6
        elseif (string .eq. '7') then
           num = 7
        elseif (string .eq. '8') then
           num = 8
        elseif (string .eq. '9') then
           num = 9
        else
           num = -1
        endif
        return
    end function num

END MODULE STRING
