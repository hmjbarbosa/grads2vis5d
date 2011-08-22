PROGRAM g2vis5d
    USE grads
    USE vis5d
    IMPLICIT NONE

!--------------------------------------------------------------------------
! I/O
!--------------------------------------------------------------------------

    ! File Names
    CHARACTER(LEN=80) :: vfile, gfile, nlist

    ! Keep minimum and maximum of each variable for diagnosis
    REAL, DIMENSION(max_var) :: zmin, zmax

    ! Counters
    INTEGER :: irec  ! index of record read from Binary 
    INTEGER :: igrid ! index of grid written to V5D

!--------------------------------------------------------------------------
! COUNTERS
!--------------------------------------------------------------------------

    INTEGER :: i, j, narg
    INTEGER :: ivar, ilev, it

    real :: fact
    character*1 ans
    integer :: convw
    LOGICAL :: lexist, iter

    namelist /options/ convw

!--------------------------------------------------------------------------
! EXTERNAL FUNCTIONS
!--------------------------------------------------------------------------

    INTEGER :: v5dwrite
    EXTERNAL v5dwrite

!--------------------------------------------------------------------------
! Initialize
!--------------------------------------------------------------------------

    write(*,*) '-----------------------------------------------------------'
    write(*,*) 'GRADS TO VIS5D'
    write(*,*) '-----------------------------------------------------------'

    narg=iargc()
    if(narg.eq.1) then
       iter=.false.
       call getarg(1,nlist)    
       print*, 'Non-iterative RUN'
       print*, 'Reading configuration from namelist: ',trim(nlist)
       INQUIRE (FILE=TRIM(nlist),exist=lexist)
       IF (.NOT.lexist) THEN
          print*, 'Namelist file "',trim(nlist),'" not found!'
          stop
       ENDIF

       open(15,file=trim(nlist),form='formatted',status='old',action='read')
       convw=0
       read(15,options)
       close(15)
       
       CALL INIT_GRADS(iter,nlist)
       CALL INIT_VIS5D(iter,nlist)
    elseif (narg.eq.2) then
       iter=.true.
       call getarg(1,gfile)    
       call getarg(2,vfile)
       print*, 'Iterative RUN'
       print*, 'CTL file: ', trim(gfile)
       INQUIRE (FILE=TRIM(gfile),exist=lexist)
       IF (.NOT.lexist) THEN
          print*, 'CTL file "',trim(nlist),'" not found!'
          stop
       ENDIF
       INQUIRE (FILE=TRIM(vfile),exist=lexist)
       print*, 'V5D file: ', trim(vfile)
       IF (lexist) THEN
          print*, 'File "',trim(vfile),'" exists! Overwrite (y/n) ?'
          call getch(ans)
          if (.not.(ans.eq.'y'.or.ans.eq.'Y')) stop
       ENDIF

       CALL INIT_GRADS(iter,gfile)
       CALL INIT_VIS5D(iter,vfile)
    else
       print*, 'Usage: '
       print*, '     g2vis5d <grads file> <vis5d file>'
       print*, 'or'
       print*, '     g2vis5d <namelist>'
       stop
    endif


!--------------------------------------------------------------------------
! Read, Convert and Write
!--------------------------------------------------------------------------

    write(*,*) '-----------------------------------------------------------'
    write(*,*) 'CONVERT_GRADS_2_V5D'
    write(*,*) '-----------------------------------------------------------'

    if (iter) then
       print*, 'Convert vertical velocity ?'
       print*, '0- none'
       print*, '1- from Pa/s to m/s'
       print*, '2- from Pa/s to cm/s'
       print*, '3- from m/s to Pa/s'
       print*, '4- from cm/s to Pa/s'
       call getch(ans)
       convw=num(ans)
    endif

    irec=0
    igrid=0
    zmin = 1.0e10
    zmax = -1.0e10
    do it=1,ntime
       vgrid=0.0
       ! read all data for current time
       do ivar=1,nvar
          do ilev=1,nl(ivar)
             irec=irec+1
             if (sequ) then
                read(ifbin) ggrid
             else
                read(ifbin,rec=irec) ggrid
             endif
             do j=clat1,clat2
                do i=clon1,clon2
                   if (yrev) then
                      vgrid(j,i,ilev,ivar)=ggrid(i,j)
                   else
                      vgrid(nlat+1-j,i,ilev,ivar)=ggrid(i,j)
                   endif
                   if(ggrid(i,j).ne.undef) then
                      if (ggrid(i,j) .lt. zmin(ivar)) then
                         zmin(ivar) = ggrid(i,j)
                      endif
                      if (ggrid(i,j) .gt. zmax(ivar)) then
                         zmax(ivar) = ggrid(i,j)
                      endif
                   end if
                enddo
             enddo
          enddo
       enddo
       ! convert pa/s to m/s
       if (convw>0) then
          ! from pressure to length
          if (convw==1.or.convw==2) then
             if (convw==1) fact=R/g/100.0
             if (convw==2) fact=R/g
             do ilev=1,outnl(ivw)
                do j=nlat+1-clat2,nlat+1-clat1
                   do i=clon1,clon2
                      if((vgrid(j,i,ilev,ivw).ne.undef).and.(vgrid(j,i,ilev,ivt).ne.undef)) then
                         ! dz = - dP * R * T / g P
                         vgrid(j,i,ilev,ivw) = - vgrid(j,i,ilev,ivw) * vgrid(j,i,ilev,ivt) * fact / p(ilev)
                      end if
                   enddo
                enddo
             enddo
          endif
          ! from length to pressure
          if (convw==3.or.convw==4) then
             if (convw==3) fact=g*100.0/R
             if (convw==4) fact=g/R
             do ilev=1,outnl(ivw)
                do j=nlat+1-clat2,nlat+1-clat1
                   do i=clon1,clon2
                      if((vgrid(j,i,ilev,ivw).ne.undef).and.(vgrid(j,i,ilev,ivt).ne.undef)) then
                         ! dP = - dz * g * P / R * T
                         vgrid(j,i,ilev,ivw) = - vgrid(j,i,ilev,ivw) * fact * p(ilev) / vgrid(j,i,ilev,ivt)
                      end if
                   enddo
                enddo
             enddo
          endif
          ! check new max/min values
          do ilev=1,outnl(ivw)
             do j=nlat+1-clat2,nlat+1-clat1
                do i=clon1,clon2
                   if(vgrid(j,i,ilev,ivw).ne.undef) then
                      if (vgrid(j,i,ilev,ivw).lt. zmin(nvar+1)) then
                         zmin(nvar+1) = vgrid(j,i,ilev,ivw)
                      endif
                      if (vgrid(j,i,ilev,ivw) .gt. zmax(nvar+1)) then
                         zmax(nvar+1) = vgrid(j,i,ilev,ivw)
                      endif
                   end if
                enddo
             enddo
          enddo
       endif

       ! write data in v5d format
       do ivar=1,nvar
          iflag=v5dwrite(it,ivar,vgrid(nlat+1-clat2:nlat+1-clat1,clon1:clon2,1:outnl(ivar),ivar))
          igrid=igrid+1
          write(*,'(''Wrote grid '',i4,''   status: '',i4)') igrid,iflag
       enddo
    enddo

    print*
    write(*,'("Variables")')
    if (convw>0) then
       nvar=nvar+1
       if (convw==1) then
          cname(nvar)='W(m/s)'
          cname(ivw)='W(Pa/s)'
       elseif (convw==2) then
          cname(nvar)='W(cm/s)'
          cname(ivw)='W(Pa/s)'
       elseif (convw==3) then
          cname(nvar)='W(Pa/s)'
          cname(ivw)='W(m/s)'
       elseif (convw==4) then
          cname(nvar)='W(Pa/s)'
          cname(ivw)='W(cm/s)'
       endif
    endif
    do ivar=1,nvar
       write(*,'("var=",A8," min=",G15.8," max=",G15.8)') &
            trim(cname(ivar)),zmin(ivar),zmax(ivar)
    enddo

!--------------------------------------------------------------------------
! Terminate
!--------------------------------------------------------------------------

    CALL TERM_VIS5D
    CALL TERM_GRADS

    STOP
END PROGRAM G2VIS5D
