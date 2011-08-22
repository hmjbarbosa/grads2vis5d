MODULE grads
    USE STRING
    USE TIME
    USE CONSTANTS

    IMPLICIT NONE

    PUBLIC

    ! Variable list
    INTEGER :: nvar  ! number of variables
    CHARACTER(LEN=80), DIMENSION(max_var) :: cname*10 ! Names
    INTEGER, DIMENSION(max_var) :: nl ! Number of levels

    ! UNDEF
    REAL :: undef

    ! XDEF: Western and increment
    INTEGER :: nlon  ! number of longitudes
    REAL :: rlonw, rlone, rloninc

    ! YDEF: Southern, northern and increment
    INTEGER :: nlat  ! number of latitudes
    REAL :: rlats, rlatn, rlatinc

    ! ZDEF: Top, Bottom and increment
    INTEGER :: nlev  ! max number of levels
    REAL :: vtop, vbot, vinc
    REAL, ALLOCATABLE, DIMENSION(:) :: p,z

    ! Indexes
    ! Variables in V5D must be U,V,W and T
    ! It does not understand things like uvel or TEMP
    !
    INTEGER :: ivt ! index of temperature variable (in Kelvins) 
    INTEGER :: ivw ! index of vertical velocity variable 

    ! Initial Time and Date
    INTEGER :: idate, itime
    INTEGER :: iscs, idys

    ! Number of time-steps
    INTEGER :: ntime

    ! File Parameters
    INTEGER, PARAMETER :: ifbin = 10 ! binary input unit
    CHARACTER(LEN=80)  :: ctlfile
    CHARACTER(LEN=80)  :: binfile

    LOGICAL :: yrev
    character(len=20) :: bige
    LOGICAL :: sequ

    REAL, ALLOCATABLE, DIMENSION(:,:) :: ggrid

CONTAINS

    !----------------------------------------------------------------------
    !       Reads CTL
    !
    subroutine init_grads(iter,infile)
        implicit none
        logical, intent(in) :: iter
        CHARACTER(LEN=80), intent(in) :: infile

        INTEGER :: nline ! number of lines on ctl file
        INTEGER, PARAMETER :: ifctl = 30 ! ctl input unit
        INTEGER :: ierr  ! I/O error flag
        INTEGER :: lrecl ! record length

        CHARACTER(LEN=80) :: aword, aline

        ! Counters
        INTEGER :: iline ! index of line read from CTL

        character*1 ans

        ! Local Counters
        INTEGER :: i, j, nargs, inzdef

        namelist /grads/ ctlfile,inzdef

        !=======================================================================
        write(*,*) '-----------------------------------------------------------'
        write(*,*) 'INIT_GRADS'
        write(*,*) '-----------------------------------------------------------'

        if (iter) then
           ctlfile = infile
           print*, 'CTL file: ', trim(ctlfile)
        else
           open(15,file=trim(infile),form='formatted',status='old',action='read')
           ctlfile=''
           inZDEF=-1
           read(15,grads)
           close(15)
        endif


        yrev=.FALSE.
        bige='little_endian'
        sequ=.FALSE.

        ! Open CTL file and find number of lines
        open(ifctl,file=ctlfile,form='formatted',status='old',action='read')
        nline=0
        do while(.true.)
           read(ifctl,'(A)',iostat=ierr) aline
           if (ierr.ne.0) exit
           nline=nline+1
        enddo
        print*, 'Number of lines: ',nline

        !     
        !---------------READ CTL FILE ---------------------
        !     
        iline = 1
        do while (iline .lt. nline)
           call lgetarg(ifctl,iline,1,aword)

           !---------------------------------------------------------
           if (aword(1:4).eq.'DSET' .OR. aword(1:4).eq.'dset') then
              call lgetarg(ifctl,iline,2,aword)
              binfile=trim(aword)
              if(binfile(1:1).eq.'^') then
                 binfile=binfile(2:last_nblank(binfile))
              endif
              print*, 'Data file: ', trim(binfile)

           !---------------------------------------------------------
           elseif (aword(1:5).eq.'UNDEF' .OR. aword(1:5).eq.'undef') then
              call lgetarg(ifctl,iline,2,aword)
              read(aword,*) undef
              print*, 'undef: ', undef

           !---------------------------------------------------------
           elseif (aword(1:7).eq.'OPTIONS' .OR. aword(1:7).eq.'options') then
              do i=2,10
                 call lgetarg(ifctl,iline,i,aword)
                 if     (aword(1:4).eq.'YREV' .OR. aword(1:4).eq.'yrev') then
                    yrev=.TRUE.
                 elseif (aword(1:10).eq.'BIG_ENDIAN' .OR. aword(1:10).eq.'big_endian') then
                    bige='big_endian'
                 elseif (aword(1:13).eq.'LITTLE_ENDIAN' .OR. aword(1:13).eq.'little_endian') then
                    bige='little_endian'
                 elseif (aword(1:10).eq.'SEQUENTIAL' .OR. aword(1:10).eq.'sequential') then
                    sequ=.TRUE.
                 elseif (aword(1:6).eq.'STREAM' .OR. aword(1:6).eq.'stream') then
                    sequ=.FALSE.
                 endif
              enddo
              print*, 'yrev=',yrev
              print*, 'bige=',bige
              print*, 'sequ=',sequ
           !---------------------------------------------------------
           elseif (aword(1:4).eq.'XDEF' .OR. aword(1:4).eq.'xdef') then
              call lgetarg(ifctl,iline,2,aword)
              read(aword,*) nlon
              call lgetarg(ifctl,iline,3,aword)
              if (index(aword,'I').eq.0 .AND. index(aword,'i').eq.0) then
                 print *, 'X-grid must be regular!'
                 stop
              else               ! X is "linear"
                 call lgetarg(ifctl,iline,4,aword)
                 read(aword,*) rlonw
                 call lgetarg(ifctl,iline,5,aword)
                 read(aword,*) rloninc
                 rlone=rlonw+rloninc*(nlon-1)
                 print*, 'nlon, rlonw, rloninc: ', nlon, rlonw, rloninc
              endif
           !---------------------------------------------------------
           elseif (aword(1:4).eq.'YDEF' .OR. aword(1:4).eq.'ydef') then
              call lgetarg(ifctl,iline,2,aword)
              read(aword,*) nlat
              call lgetarg(ifctl,iline,3,aword)
              if (index(aword,'I').eq.0 .AND. index(aword,'i').eq.0) then
                 print *, 'Y-grid is not regular! Calculating an approximate regular grid...'
                 j = 4
                 nargs = largc(ifctl,iline)
                 do i=1,nlat
                    if (j .gt. nargs) then
                       iline = iline + 1
                       j = 1
                       nargs = largc(ifctl,iline)
                    endif
                    call lgetarg(ifctl,iline,j,aword)
                    if (i.eq.1) then
                       read(aword,*) rlats
                       print*, 'iarg, y(iarg): ', i, rlats
                    else
                       read(aword,*) rlatn
                    endif
                    j = j + 1
                 enddo
                 print*, 'iarg, y(iarg): ', i-1, rlatn
                 rlatinc=(rlatn-rlats)/real(nlat-1.0)
                 print*, 'nlat, rlats, rlatinc: ', nlat, rlats, rlatinc
              else                ! Y is "linear"
                 call lgetarg(ifctl,iline,4,aword)
                 read(aword,*) rlats
                 call lgetarg(ifctl,iline,5,aword)
                 read(aword,*) rlatinc
                 rlatn=rlats+rlatinc*(nlat-1)
                 print*, 'nlat, rlats, rlatinc: ', nlat, rlats, rlatinc
              endif
           !---------------------------------------------------------
           elseif (aword(1:4).eq.'ZDEF' .OR. aword(1:4).eq.'zdef') then
              call lgetarg(ifctl,iline,2,aword)
              read(aword,*) nlev
              ALLOCATE(p(nlev))
              ALLOCATE(z(nlev))
              print*, 'nlev: ', nlev
              call lgetarg(ifctl,iline,3,aword)
              if (index(aword,'I').eq.0 .AND. index(aword,'i').eq.0) then
                 j = 4
                 nargs = largc(ifctl,iline)
                 do i=1,nlev    ! P is "levels"
                    if (j .gt. nargs) then
                       iline = iline + 1
                       j = 1
                       nargs = largc(ifctl,iline)
                    endif
123                 call lgetarg(ifctl,iline,j,aword)
                    ! jump over tabs
                    if (trim(aword).eq.'	') then
                       j=j+1
                       goto 123
                    endif
                    read(aword,*) p(i)
                    print*, 'iarg, p(iarg): ', i, p(i)
                    j = j + 1
                 enddo
              else                ! P is "linear"
                 call lgetarg(ifctl,iline,4,aword)
                 read(aword,*) vbot
                 call lgetarg(ifctl,iline,5,aword)
                 read(aword,*) vinc
                 print*, 'vbot, vinc: ', vbot, vinc
                 do i=1,nlev
                    p(i)=vbot+(i-1)*vinc
                 enddo
              endif
              if (iter) then
                 print*, 'ZLEV in (p)ressure or (h)eight ?'
                 call getch(ans)
                 if (ans.eq.'p') then
                    inzdef=1
                 elseif (ans.eq.'h') then
                    print*,'is ZLEV in (k)m or in (m)eters ?'
                    read(*,'(A1)') ans
                    if (ans.eq.'k') then
                       inzdef=2
                    elseif (ans.eq.'m') then
                       inzdef=3
                    else
                       print*, 'Error, valid answer is (k)m or (m)eters '
                       stop
                    endif
                 else
                    print*, 'Error, valid answer is (p)ressure or (h)eight'
                    stop
                 endif
              else
                 if (inzdef.eq.1) then
                    print*, 'READING ZLEV as pressure'
                 elseif(inzdef.eq.2) then
                    print*, 'READING ZLEV as height in km'
                 elseif(inzdef.eq.3) then
                    print*, 'READING ZLEV as height in m'
                 else
                    print*, 'ERROR, inZDEF=',inZDEF
                    print*, 'Valid options are 1,2 or 3'
                    stop
                 endif
              endif
              print*, 'nlev: ', nlev
              if (inzdef.eq.1) then
                 z=H*log(Po/p)
              else
                 if (inzdef.eq.2) then
                    z=p
                 else
                    z=p/1000.
                 endif
                 p=Po*exp(-z/H)
              endif
              do i=1,nlev
                 print*, 'z(lev), p(lev), lev: ', z(i), p(i), i
              enddo
           !---------------------------------------------------------
           elseif (aword(1:4).eq.'TDEF' .OR. aword(1:5).eq.'tdef') then
              call lgetarg(ifctl,iline,2,aword)
              read(aword,*) ntime
              print*, 'ntime: ', ntime
              call lgetarg(ifctl,iline,4,aword)
              call get_time(trim(aword),idate,itime)
              print*, 'idate, itime: ', idate, itime
              call lgetarg(ifctl,iline,5,aword)
              call get_tinc(aword, idys, iscs)
              print*, 'idys, iscs: ', idys, iscs
           !---------------------------------------------------------
           elseif (aword(1:4).eq.'VARS' .OR. aword(1:5).eq.'vars') then
              call lgetarg(ifctl,iline,2,aword)
              read(aword,*) nvar
              do i=1,nvar
                 call lgetarg(ifctl,iline+i,1,cname(i))
                 call lgetarg(ifctl,iline+i,2,aword)
                 read(aword,*) nl(i)
                 if (nl(i) .lt. 1) nl(i) = 1
                 print*, 'i, cname, nl: ', i, cname(i), nl(i)
                 if (trim(cname(i)).eq.'T'.or.trim(cname(i)).eq.'t') ivt=i
                 if (trim(cname(i)).eq.'W'.or.trim(cname(i)).eq.'w') ivw=i
              enddo
           endif
           iline = iline+1
        enddo

        close(ifctl)

        ! Open Binary input file
        ALLOCATE(ggrid(nlon,nlat))

        if (sequ) then
           OPEN(ifbin,file=binfile,access='sequential',status='old',action='read', &
                convert=trim(bige), form='unformatted')
        else
           INQUIRE(IOLENGTH=lrecl) ggrid
           OPEN(ifbin,file=binfile,access='direct',status='old',action='read', &
                convert=trim(bige), form='unformatted', recl=lrecl)
        endif

    END SUBROUTINE INIT_GRADS

    SUBROUTINE TERM_GRADS
        IMPLICIT NONE

        DEALLOCATE(p)
        DEALLOCATE(z)
        DEALLOCATE(ggrid)
        
        CLOSE(ifbin)
    END SUBROUTINE TERM_GRADS

    !----------------------------------------------------------------------
    !       Returns date and time increment as days and secs
    !
    subroutine get_tinc(string, idys, iscs)
        IMPLICIT NONE

        character*(*) string

        INTEGER :: idys, iscs
        INTEGER :: i,i2,k

        i = num(string(1:1))
        i2 = num(string(2:2))
        if (i2 .ge. 0 .and. i2 .le. 9) then
           i = 10 * i + i2
           k = 3
        else
           k = 2
        endif
        if (last_nblank(string) .ne. k+1) then
           print*, 'bad tinc(1): ', string
           stop
        endif
        if (string(k:k+1) .eq. 'mn' .or. &
             string(k:k+1) .eq. 'MN') then
           idys = 0
           iscs = 60 * i
        elseif (string(k:k+1) .eq. 'hr' .or. &
             string(k:k+1) .eq. 'HR') then
           idys = 0
           iscs = 60 * 60 * i
        elseif (string(k:k+1) .eq. 'dy' .or. &
             string(k:k+1) .eq. 'DY') then
           idys = i
           iscs = 0
        elseif (string(k:k+1) .eq. 'mo' .or. &
             string(k:k+1) .eq. 'MO') then
           idys = 30 * i
           iscs = 60 * 60 * 10
        elseif (string(k:k+1) .eq. 'yr' .or. &
             string(k:k+1) .eq. 'YR') then
           idys = 365 * i
           iscs = 0
        else
           print*, 'bad tinc(2): ', string
           stop
        endif
        return
    end subroutine get_tinc

    !----------------------------------------------------------------------
    !       Returns start date and time as yyddd and hhmmss
    !
    subroutine get_time(string, idate, itime)
        IMPLICIT NONE

        INTEGER, INTENT(OUT) :: idate, itime

        INTEGER :: ihh, idd, imm, iyy, leap
        INTEGER :: i,i2,k

        character*(*) string
        string=trim(string)
        if (string(3:3) .eq. ':') then
           i = num(string(1:1))
           i2 = num(string(2:2))
           if (i .lt. 0 .or. i .gt. 9 .or. i2 .lt. 0 .or. i2 .gt. 9) then
              print*, 'bad date(1): ', string
              stop
           endif
           ihh = 10 * i + i2
           i = num(string(4:4))
           i2 = num(string(5:5))
           if (i .lt. 0 .or. i .gt. 9 .or. i2 .lt. 0 .or. i2 .gt. 9) then
              print*, 'bad date(2): ', string
              stop
           endif
           imm = 10 * i + i2
           if (string(6:6) .ne. 'Z' .and. string(6:6) .ne. 'z') then
              print*, 'bad date(3): ', string
              stop
           endif
           k = 7
        else if (string(3:3) .eq. 'Z' .or. string(3:3) .eq. 'z') then
           i = num(string(1:1))
           i2 = num(string(2:2))
           if (i .lt. 0 .or. i .gt. 9 .or. i2 .lt. 0 .or. i2 .gt. 9) then
              print*, 'bad date(4): ', string
              stop
           endif
           ihh = 10 * i + i2
           imm = 0
           k = 4
        else if (string(2:2) .eq. ':') then
           i = num(string(1:1))
           if (i .lt. 0 .or. i .gt. 9) then
              print*, 'bad date(1a): ', string
              stop
           endif
           ihh = i
           i = num(string(3:3))
           i2 = num(string(4:4))
           if (i .lt. 0 .or. i .gt. 9 .or. i2 .lt. 0 .or. i2 .gt. 9) then
              print*, 'bad date(2a): ', string
              stop
           endif
           imm = 10 * i + i2
           if (string(5:5) .ne. 'Z' .and. string(5:5) .ne. 'z') then
              print*, 'bad date(3a): ', string
              stop
           endif
           k = 6
        else if (string(2:2) .eq. 'Z' .or. &
             string(2:2) .eq. 'z') then
           i = num(string(1:1))
           if (i .lt. 0 .or. i .gt. 9) then
              print*, 'bad date(4a): ', string
              stop
           endif
           ihh = i
           imm = 0
           k = 3
        else
           ihh = 0
           imm = 0
           k = 1
        endif
        itime = 100 * (100 * ihh + imm)

        idd = num(string(k:k))
        i2 = num(string(k+1:k+1))
        if (idd .lt. 0 .or. idd .gt. 9) then
           print*, 'bad date(5): ', string
           stop
        endif
        if (i2 .ge. 0 .and. i2 .le. 9) then
           idd = 10 * idd + i2
           k = k + 2
        else
           k = k + 1
        endif

        ! mmm in k to k+2
        ! yy in k+3 to k+4 or yyyy in k+3 to k+6

        if (last_nblank(string) .eq. k+4) then
           i = num(string(k+3:k+3))
           i2 = num(string(k+4:k+4))
           if (i .lt. 0 .or. i .gt. 9 .or. i2 .lt. 0 .or. i2 .gt. 9) then
              print*, 'bad date(6): ', string
              stop
           endif
           iyy = 10 * i + i2
        else if (last_nblank(string) .eq. k+6) then
           if (string(k+3:k+4) .ne. '19') then
              print*, 'bad date(7): ', string
              print*, 'string=',trim(string)
              print*, 'i,i2,k=',i,i2,k
              print*, 'last_nblank(string)=',last_nblank(string)
              stop
           endif
           i = num(string(k+5:k+5))
           i2 = num(string(k+6:k+6))
           if (i .lt. 0 .or. i .gt. 9 .or. i2 .lt. 0 .or. i2 .gt. 9) then
              print*, 'bad date(8): ', string
              print*, 'i,i2,k=',i,i2,k
              stop
           endif
           iyy = 10 * i + i2
           !          read(string(k+5:k+6),'(i)') iyy
           print*, 'iyy=',iyy
        else
           print*, 'bad date(9): ', string, last_nblank(string), k
           stop
        endif

        if (mod(iyy, 4) .eq. 0) then
           leap = 1
        else
           leap = 0
        endif

        if (string(k:k+2) .eq. 'jan' .or.&
             string(k:k+2) .eq. 'JAN') then
           idd = idd + 0
        else if (string(k:k+2) .eq. 'feb' .or.&
             string(k:k+2) .eq. 'FEB') then
           idd = idd + 31
        else if (string(k:k+2) .eq. 'mar' .or.&
             string(k:k+2) .eq. 'MAR') then
           idd = idd + 59 + leap
        else if (string(k:k+2) .eq. 'apr' .or.&
             string(k:k+2) .eq. 'APR') then
           idd = idd + 90 + leap
        else if (string(k:k+2) .eq. 'may' .or.&
             string(k:k+2) .eq. 'MAY') then
           idd = idd + 120 + leap
        else if (string(k:k+2) .eq. 'jun' .or.&
             string(k:k+2) .eq. 'JUN') then
           idd = idd + 151 + leap
        else if (string(k:k+2) .eq. 'jul' .or.&
             string(k:k+2) .eq. 'JUL') then
           idd = idd + 181 + leap
        else if (string(k:k+2) .eq. 'aug' .or.&
             string(k:k+2) .eq. 'AUG') then
           idd = idd + 211 + leap
        else if (string(k:k+2) .eq. 'sep' .or.&
             string(k:k+2) .eq. 'SEP') then
           idd = idd + 242 + leap
        else if (string(k:k+2) .eq. 'oct' .or.&
             string(k:k+2) .eq. 'OCT') then
           idd = idd + 272 + leap
        else if (string(k:k+2) .eq. 'nov' .or.&
             string(k:k+2) .eq. 'NOV') then
           idd = idd + 303 + leap
        else if (string(k:k+2) .eq. 'dec' .or.&
             string(k:k+2) .eq. 'DEC') then
           idd = idd + 333 + leap
        else
           print*, 'bad date(10): ', string
           stop
        endif
        idate = 1000 * iyy + idd

        return
    end subroutine get_time




END MODULE grads
