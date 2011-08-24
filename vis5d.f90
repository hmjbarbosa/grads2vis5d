MODULE VIS5D
    USE Constants
    USE TIME
    IMPLICIT NONE

    PUBLIC

    ! Flags
    INTEGER :: iflag     ! error flag
    INTEGER :: icompress ! compression flag
    INTEGER :: ivert     ! vertical coordinate type
    INTEGER :: iproj     ! horizontal projection type

    ! Time/date stamps
    INTEGER :: itime_stamp(max_time),idate_stamp(max_time)

    ! v5d undefined, from v5d.h
    real, parameter :: vundef=1.0e35

    ! Output vertical coordinate
    REAL, ALLOCATABLE, DIMENSION(:) :: vert_arg

    ! Horizontal projection arguments
    REAL, DIMENSION(5) :: proj_arg

    CHARACTER(LEN=80) :: v5dfile

    INTEGER, DIMENSION(max_var) :: outnl ! Number of levels

    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: vgrid ! for output

    INTEGER :: clon1, clon2, clat1, clat2, cz
    INTEGER :: vnlon, vnlat

CONTAINS

    SUBROUTINE INIT_VIS5D(iter,infile)
!(nlev,ntime,idate,itime,iscs,idys)
        USE Grads
        IMPLICIT NONE
        logical, intent(in) :: iter
        CHARACTER(LEN=80), intent(in) :: infile

        !        INTEGER, intent(in) :: nlev  ! max number of levels
        !        INTEGER, intent(in) :: ntime
        !        INTEGER, intent(in) :: idate, itime
        !        INTEGER, intent(in) :: iscs, idys

        INTEGER :: i,j,id,it,outzdef
        real :: ilon, flon, ilat, flat, maxz

        character*1 ans

        namelist /vis5d/ v5dfile, outzdef, ilon, flon, ilat, flat, maxz

        INTEGER :: v5dcreate
        EXTERNAL v5dcreate 

        !=======================================================================
        write(*,*) '-----------------------------------------------------------'
        write(*,*) 'INIT_V5D'
        write(*,*) '-----------------------------------------------------------'

        if (iter) then
           v5dfile = infile
        else
           open(15,file=trim(infile),form='formatted',status='old',action='read')
           v5dfile=''
           outZDEF=-1
           ilon=0.0
           flon=360.0
           ilat=-90.0
           flat=90.0
           maxz=1.0e10
           read(15,vis5d)
           close(15)
        endif
        print*, 'Output file: ', trim(v5dfile)

        write(*,*) 'nlev=',nlev
        write(*,*) 'ntime=',ntime

        id = IDAYS(iyear,idate)
        it = ISECS(itime)
        do i=1,ntime
           idate_stamp(i)=IYYDDD(id)
           itime_stamp(i)=IHMS(it)
           it = it + iscs
           j = it / 86400
           it = it - 86400 * j
           id = id + idys + j
!           write(*,*) 'stamp=',idate_stamp(i),itime_stamp(i)
        enddo

        ! Crop original grid
        clon1=min(nlon,max(1,int((ilon-rlonw)/rloninc+1)))
        clon2=min(nlon,max(1,int((flon-rlonw)/rloninc+1)))
        if (clon1>=clon2) then
           clon1=1
           clon2=nlon
        endif
        clat1=min(nlat,max(1,int((ilat-rlats)/rlatinc+1)))
        clat2=min(nlat,max(1,int((flat-rlats)/rlatinc+1)))
        if (clat1>=clat2) then
           clat1=1
           clat2=nlat
        endif
        cz=nlev
        do while(p(cz)<maxz.and.cz>0)
           cz=cz-1
        enddo

        print*, 'CROP REGION:'
        print*, 'Longitude=',ilon,flon
        print*, 'Latitude=' ,ilat,flat
        print*, 'Vertical=',maxz,'mb'
        print*
        print*, 'Selected grid points:'
        print*, 'Longitude,',clon1,'=',rlonw+rloninc*(clon1-1),clon2,'=',rlonw+rloninc*(clon2-1)
        print*, 'Latitude ,',clat1,'=',rlats+rlatinc*(clat1-1),clat2,'=',rlats+rlatinc*(clat2-1)
        print*, 'Vertical ,',1,'=',p(1),'mb ',cz,'=',p(cz),'mb '

        !     - Projection must be linear, rectangular, cyl-equidistant -
        !0-Generic rectilinear
        !1-Cylindrical Equidistant
        !2-Lambert Conformal
        !3-Polar Stereographic
        !4-Rotated
        !5-Mercator
        iproj=1
        proj_arg(1)=rlats+rlatinc*(clat2-1)
        proj_arg(2)=-(rlonw+rloninc*(clon1-1))
        proj_arg(3)=rlatinc
        proj_arg(4)=rloninc

        !     - Unequally spaced vertical coordinates in km -
        !0-Linear, Equally spaced, Generic units
        !1-Linear, Equally spaced, Km
        !2-Linear, Unequally spaced, Km
        !3-Pressure, Unequally spaced, Mb
        if (iter) then
           print*, 'WRITE ZLEV in (p)ressure or (h)eight ?'
           call getch(ans)
           if (ans.eq.'p') then
              outzdef=1
           elseif (ans.eq.'h') then
              outzdef=2
           else
              print*, 'Error, valid answer is (p)ressure or (h)eight'
              stop
           endif
        else
           if (outzdef.eq.1) then
              print*, 'WRITING ZLEV as pressure'
           elseif (outzdef.eq.2) then
              print*, 'WRITING ZLEV as height'
           else
              print*, 'ERROR, outZDEF=',outZDEF
              print*, 'Valid options are 1 or 2'
              stop
           endif
        endif

        ALLOCATE(vert_arg(cz))
        if (outzdef.eq.1) then
           ivert=3
           vert_arg=p(1:cz)
        elseif (outzdef.eq.2) then
           ivert=2
           vert_arg=z(1:cz)
        endif
        do i=1,nvar
           outnl(i)=min(cz,nl(i))
        enddo

        !     - compress to 1 byte
        ! now defined outside
        !icompress=1 

        !     - Create VIS5D file -
        vnlon=clon2-clon1+1
        vnlat=clat2-clat1+1
        iflag=v5dcreate(v5dfile,ntime,nvar,vnlat,vnlon, &
             outnl,cname,itime_stamp,idate_stamp,icompress, &
             iproj,proj_arg,ivert,vert_arg)
        if(iflag.ne.1) then
           print*, 'v5dcreate error: ', iflag
           stop
        endif

        !     - Dynamically allocate memory for grids -
        print*,'allocate vgrid=',vnlat,vnlon,nlev,nvar
        ALLOCATE(vgrid(vnlat,vnlon,nlev,nvar))

    END SUBROUTINE INIT_VIS5D


    SUBROUTINE TERM_VIS5D
        IMPLICIT NONE
        INTEGER :: v5dclose 
        EXTERNAL v5dclose 

        iflag=v5dclose()
        print*, 'Closing v5d file... iflag=',iflag

        DEALLOCATE(vert_arg)
        DEALLOCATE(vgrid)

    END SUBROUTINE TERM_VIS5D

END MODULE VIS5D
