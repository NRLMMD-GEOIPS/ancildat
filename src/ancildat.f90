module ancildat

    use omp_lib
    use config
    use errors
    use prettyprint
    use datetime_utils
    use file_io_utils
    use string_operations

    implicit none
    ! integer, parameter, private :: bd = 8
    integer :: name_length = 6
    character(len=10), dimension(8) :: supported_sensors = (/ 'ahi       ', 'AHI       ', &
                                                              'abi       ', 'ABI       ', &
                                                              'clavrx-abi', 'CLAVRX-ABI', &
                                                              'seviri    ', 'SEVIRI    ' /)

    contains

    subroutine get_storage_fname(pref, lon_res, lat_res, lons, lats, fname, code)
        !-------------------------------------------------------------------------------------------
        ! Produces a unique filename based on the input longitude resolution,
        ! latitude resolution, longitude array, and latitude array.  The fname
        ! is output as only the basename.
        !
        ! The filename is composed of six elements and is in the form:
        !   pref_lonres_latres_coor0_coor1_coor2.dat
        ! where:
        !   pref, lonres, and latres are the input dummy argument values.
        !   coor0 is the center coordinate in the form lon0lat0.
        !   coor1 is the point 25% of the way in from the top left corner in both
        !       directions in the form lon0lat0.
        !   coor2 is the point 25% of the way in from the bottom right corner in both
        !       directions in the form lon0lat0.
        !
        ! Inputs:
        !   pref            - A string providing a prefix for the filename to
        !                     distinguish it from other similar files.
        !   lon_res         - Resolution in the longitudinal direction of the
        !                     rectangularly gridded dataset in degrees.
        !   lat_res         - Resolution in the latitudinal direction of the
        !                     rectangularly gridded dataset in degrees.
        !   lons            - A 2D array of longitudes in the range (-180, 180).
        !   lats            - A 2D array of latitudes in the range (-90, 90).
        !
        ! Outputs:
        !   fname           - The filename of the storage data file that should
        !                     be used for the input resolutions and coordinates.
        !-------------------------------------------------------------------------------------------

        implicit none

        integer, parameter :: bd = 8
        character(len=*), parameter :: storage_path = trim(ancildat_autogen_path)

        ! INPUTS
        character(len=*) :: pref
        real(kind=bd), intent(in) :: lon_res, lat_res
        real(kind=bd), dimension(:,:), intent(in) :: lons, lats

        ! INTERNAL
        integer :: stat
        logical :: dexist
        integer(kind=bd), dimension(2) :: datshape
        integer(kind=bd), dimension(2) :: coord0_inds, coord1_inds, coord2_inds
        real(kind=bd) :: lon0, lat0, lon1, lat1, lon2, lat2
        character(len=*), parameter :: FNAME_FMT = &
            '(f0.5, a, f0.5, a, f0.5, f0.5, a, f0.5, f0.5, a, f0.5, f0.5, a4)'
        character(len=256) :: basename

        ! OUTPUT
        character(len=*), intent(out) :: fname
        integer(kind=bd), intent(out) :: code

        code = 0

        ! Make sure the path exists
        inquire(file=storage_path, exist=dexist)
        if (.not. dexist) then
            call system('mkdir '//storage_path, stat)
            if (stat /= 0) then
                call IOStatusError(stat, code)
                return
            endif
        endif

        ! Get coordinate values
        datshape = shape(lons)
        coord0_inds = (/ datshape(1)/2, datshape(2)/2 /)
        coord1_inds = (/ datshape(1)/4, datshape(2)/4 /)
        coord2_inds = (/ 3*datshape(1)/4, 3*datshape(2)/4 /)
        lon0 = lons(coord0_inds(1), coord0_inds(2))
        lat0 = lats(coord0_inds(1), coord0_inds(2))
        lon1 = lons(coord1_inds(1), coord1_inds(2))
        lat1 = lats(coord1_inds(1), coord1_inds(2))
        lon2 = lons(coord2_inds(1), coord2_inds(2))
        lat2 = lats(coord2_inds(1), coord2_inds(2))

        ! Correct this if we are off of the disk somewhere
        if ((lon0 < -180) .or. (lon0 > 180)) then
            lon0 = -999.0
        endif
        if ((lat0 < -90) .or. (lat0 > 90)) then
            lat0 = -999.0
        endif
        if ((lon1 < -180) .or. (lon1 > 180)) then
            lon1 = -999.0
        endif
        if ((lat1 < -90) .or. (lat1 > 90)) then
            lat1 = -999.0
        endif
        if ((lon2 < -180) .or. (lon2 > 180)) then
            lon2 = -999.0
        endif
        if ((lat2 < -90) .or. (lat2 > 90)) then
            lat2 = -999.0
        endif
        
        print *, datshape, lon_res, lat_res, lon0, lat0, lon1, lat1, lon2, lat2
        write(basename, FNAME_FMT), lon_res, '_', lat_res, '_', lon0, lat0, '_', &
            lon1, lat1, '_',  lon2, lat2, '.dat'
        fname = trim(storage_path)//'/'//trim(pref)//'_'//trim(basename)

    end subroutine get_storage_fname

    subroutine get_inds(lon_res, lat_res, lons, lats, line_inds, sample_inds, code)
        !-------------------------------------------------------------------------------------------
        ! Converts the input latitudes and longitudes to lines and samples in a
        ! rectangularly gridded global dataset that starts and ends at the prime
        ! meridian.  All input values are in degrees.
        !
        ! Inputs:
        !   lon_res         - Resolution in the longitudinal direciton of the
        !                     rectangularly gridded dataset in degrees.
        !   lat_res         - Resolution in the latitudinal direction of the
        !                     rectangularly gridded dataset in degrees.
        !   lons            - A 2D array of longitudes in the range (-180, 180) for
        !                     which to calculate line numbers.
        !   lats            - A 2D array of latitudes in the range (-90, 90) for
        !                     which to calculate sample numbers.
        !
        ! Outputs:
        !   lines_inds      - Line numbers where each latitude can be found in the
        !                     rectangularly gridded datset as a 2D array of the same
        !                     shape as lats.
        !   samples_inds    - Sample numbers where each longitude can be found in the
        !                     rectangularly gridded dataset as a 2D array of the
        !                     same shape as lons.
        !-------------------------------------------------------------------------------------------
        implicit none
        integer, parameter :: bd = 8

        real(kind=bd), intent(in) :: lon_res, lat_res
        real(kind=bd), dimension(:, :), intent(in) :: lats, lons
        integer(kind=bd), dimension(2) :: arr_shape
        real(kind=bd), allocatable, dimension(:, :) :: pos
        integer(kind=bd), dimension(:, :), intent(out) :: line_inds, sample_inds
        integer(kind=bd), intent(out) :: code

        !f2py real(kind=bd), intent(in) :: lon_res, lat_res
        !f2py real(kind=bd), dimension(:, :), intent(in) :: lon, lat
        !f2py integer(kind=bd), dimension(:, :), intent(out) :: line_inds, sample_inds
        !f2py integer(kind=bd), intent(out) :: code

        code = 0

        ! Get the shape of the arrays and allocate pos_lats and pos_lons
        arr_shape = shape(lats)
        allocate(pos(arr_shape(1), arr_shape(2)))

        ! ! Check to be sure that all latitude values are between -90 and 90
        ! if ((minval(lats) .lt. -90) .or. (maxval(lats) .gt. 90)) then
        !     call ValueError('Latitudes out of range [-90, 90]', code)
        ! endif
        ! Put latitudes in the range 0, 180
        where (lats .ge. 0)
            pos = 90 - lats
        elsewhere
            where (lats .le. -999)
                pos = -999
            elsewhere
                pos = -1 * lats + 90
            endwhere
        endwhere
        ! Get line indexes (moved here to avoid allocating pos twice)
        line_inds = nint((pos + lat_res) / lat_res) + 1

        ! ! Check to be sure that all longitudes are between -180 and 180
        ! if ((minval(lons) .lt. -180) .or. (maxval(lons) .gt. 180)) then
        !     call ValueError('Longitudes out of range (-180, 180)', code)
        ! endif
        ! Put longitudes in the range 0, 360 where 180W == 0
        where (lons .lt. 0)
            pos = -1 * (-180 - lons)
        elsewhere
            where (lons .le. -999)
                pos = -999
            elsewhere
                pos = lons + 180
            endwhere
        endwhere
        ! Get sample indexes
        sample_inds = nint((pos + lon_res/2.0) / lon_res) + 1
    end subroutine

    ! It would be nice to consolidate all reading here, but
    ! need to be able to handle multiple data types.
    ! SUBROUTINE read_real_data(fname, line_inds, sample_inds, out_dat)
    !     character(*), intent(in) :: fname
    !     integer(bd), dimension(:, :), intent(in) :: line_inds, sample_inds
    !     logical(bd) :: exists
    !     real(bd), dimension(:, :), intent(out) :: out_dat

    !     inquire(file=fname, exist=exists)
    !     if (.not. exists) then
    !         print *, 'File not found: ', fname
    !         stop 108
    !     endif
    !     open(1, file=fname, recl=4, access='direct', form='unformatted')
    !     do i=1, lines
    !         do j=1, samples
    !             rec = ((line_inds(i, j) -1) * 540) + sample_inds(i, j)
    !             read(1, rec=rec) out_dat(i, j)
    !         enddo
    !     enddo

    subroutine surface_temperature(lines, samples, datetime, lons, lats, temps, code)
        !-------------------------------------------------------------------------------------------
        ! This subroutine will retrieve MERRA data values for a day of year and time
        ! of day at the specified latitudes and longitudes.
        !
        ! The MERRA database used here is a dataset of ten-year average surface
        ! temperature on a 2/3 x 1/2 (longitude x latitude) degree grid.
        ! Interpolation is performed using nearest neighbor.
        ! Values are output in Kelvin.
        !
        ! Note: May want to offer other interpolation methods later.
        !
        ! Inputs:
        !   lines       - Y dimension of latitudes and longitudes and output data
        !   samples     - X dimension of latitudes and longitudes and output data
        !   datetime    - A string in the form YYYY-MM-DDTHH:MM:SSZ which will
        !                 be converted to a Date_Time instance as defined by
        !                 datetime_utils
        !   lons        - An NxM array of longitudes as floats
        !   lats        - An NxM array of latitudes as floats
        !
        ! Outputs:
        !   temps       - An NxM array of temperatures in degrees celsius
        !
        ! Python call signature:
        !   temps = merra(datetime, lons, lats)
        !
        ! This routine is written as a subroutine in order to allow it to be
        ! compiled using f2py.  Doing so allows the subroutine to be imported by
        ! python routines and used as a normal python module.  Calls to compile this
        ! routine are available in ../Makefile.  Compiling this routine will produce
        ! rayleigh.so.  To import from rayleigh.so simply use
        ! the following import in Python:
        !
        !   from rayleigh import rayleigh
        !
        ! IMPORTANT NOTES:
        ! Typically in Fortran we are able to make use of ALLOCATE statements to
        ! handle dynamic array sizes.  When using F2Py, however, this is not
        ! possible.  All ALLOCATEs must be stripped out of the code.  As a
        ! consequence, there are more required inputs than would typically be
        ! expected.  For instance, "lines" and "samples" are required inputs in
        ! order to determine the shape of the various input arrays.  Normally these
        ! values could be inferred and the arrays could be allocated dynamically,
        ! however, when using F2Py, this is not possible and the values must be
        ! known a priori and passed.
        !
        ! Note that lines and samples are not required in the python call, but are
        ! required in the fortran call.  f2py is smart enough to figure these out
        ! on its own for python, but not for fortran.
        !
        !-------------------------------------------------------------------------------------------

        implicit none
        integer, parameter :: bd = 8

        ! INPUTS
        integer(bd), intent(in) :: lines
        integer(bd), intent(in) :: samples
        character(20), intent(in) :: datetime
        real(bd), dimension(lines, samples), intent(in) :: lons
        real(bd), dimension(lines, samples), intent(in) :: lats
        integer(bd), intent(out) :: code

        ! INTERNAL
        integer :: stat
        character(512) :: storage_fname
        character(64) :: storage_pref
        type(Date_Time) :: dt
        real(bd) :: lon_res = 2.0/3.0
        real(bd) :: lat_res = 0.5
        logical, dimension(lines, samples) :: good

        ! RECORDS
        integer :: thread, lock_r, stor_r, data_r

        ! real(bd), dimension(lines, samples) :: lons_pos, lats_pos
        character(512) :: fname
        logical(bd) :: fexist
        integer(bd) :: i, j, recn
        integer(bd), dimension(lines, samples) :: line_inds, sample_inds
        real(4) :: temp

        ! OUTPUTS
        real(bd), dimension(lines, samples), intent(out) :: temps

        !***************************************************************
        ! f2py Signature Information
        !***************************************************************
        !f2py integer(bd), intent(in) :: lines
        !f2py integer(bd), intent(in) :: samples
        !f2py character(20), intent(in) :: datetime
        !f2py real(bd), dimension(lines, samples), intent(in) :: lons
        !f2py real(bd), dimension(lines, samples), intent(in) :: lats
        !f2py real(bd), dimension(lines, samples), intent(out) :: temp
        !f2py integer(bd), intent(out) :: code

        code = 0

        ! Get file unit numbers
        thread = 1
        !$ thread = omp_get_thread_num()
        ! lock_r = 10 * (thread + 1) + 1
        ! stor_r = 10 * (thread + 1) + 2
        ! data_r = 10 * (thread + 1) + 3

        ! Create the datetime instance
        call datetime_from_str(datetime, dt, code)
        if (code .ne. 0) then
            return
        endif

        ! Get storage filename
        storage_pref = 'surftemp-inds'
        call get_storage_fname(storage_pref, lon_res, lat_res, lons, lats, storage_fname, code)
        print *, trim(storage_fname)

        ! Look for a lock file (indicates currently being written)
        open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
              iostat=stat)
        do while (stat /= 0)
            print *, 'File locked ', trim(storage_fname)//'.processing'
            call sleep(1)
            open(lock_r, file=trim(storage_fname)//".processing", status='new', action='write', &
                  iostat=stat)
        enddo

        ! If the storage file exists, then read from it
        inquire(file=storage_fname, exist=fexist)
        if (fexist) then
            ! Delete the tempfile
            close(lock_r, status='delete')
            print *, 'Reading temperatures'
            open(newunit(stor_r), file=storage_fname, status='old', recl=2*bd*size(line_inds), &
                access='direct', action='read')
            read(stor_r, rec=1) line_inds
            read(stor_r, rec=2) sample_inds
            close(stor_r)
        else
            ! Get the line and sample indexes
            print *, 'Calculating temperature inds'
            call get_inds(lon_res, lat_res, lons, lats, line_inds, sample_inds, code)
            if (code .ne. 0) then
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif

            ! Store the indexes
            open(newunit(stor_r), file=storage_fname, status='new', recl=2*bd*size(line_inds), &
                action='write', iostat=stat, access='direct', form='unformatted')
            write(stor_r, rec=1) line_inds
            write(stor_r, rec=2) sample_inds
            close(stor_r)
            close(lock_r, status='delete')
        endif

        ! Create filename
        fname = trim(ancildat_path) // '/merra/' // dt%doy // '_' // &
                dt%hour // '_ts_MERRA.tdf_540_samples_361_lines.bin'
        inquire(file=fname, exist=fexist)
        if (.not. fexist) then
            call NoSuchFileError(fname, code)
        endif

        good = ((line_inds .gt. -999) .and. (sample_inds .gt. -999))
        call print_min_mean_max('Temp Line Inds', line_inds, good)
        call print_min_mean_max('Temp Sample Inds', sample_inds, good)
        call print_min_mean_max('Temp Abs Inds', (line_inds - 1) * 540 + sample_inds, good)
        ! Open and read the data
        open(newunit(data_r), file=fname, recl=4, access='direct', form='unformatted', action='read')
        print *, 'Temp File Unit: ', data_r, ' ', trim(fname)
        do i=1, lines
            do j=1, samples
                if ((line_inds(i, j) .gt. -999) .and. (sample_inds(i, j) .gt.  -999)) then
                    recn = ((line_inds(i, j) - 1) * 540) + sample_inds(i, j)
                    ! print *, rec, line_inds(i, j), sample_inds(i, j), lons_pos(i, j), lats_pos(i, j)
                    read(data_r, rec=recn) temp
                    temps(i, j) = real(temp + 273.15, bd)
                else
                    temps(i, j) = -999.0
                endif
            enddo
        enddo
        print *, 'Done getting temps'
        close(data_r)

    end subroutine surface_temperature

    subroutine surface_emissivity(lines, samples, sensor, channel, month, lons, lats, emiss, code)
        !-------------------------------------------------------------------------------------------
        ! This subroutine will retrieve surface emissivity values for a specific
        ! sensor on the specifed month at the specified latitudes and longitudes.
        !
        ! The emissivity database used here is a dataset of ten-year average surface
        ! emissivity on a 0.05 x 0.05 (longitude x latitude) degree grid.
        ! Interpolation is performed using nearest neighbor.
        !
        ! Note: May want to offer other interpolation methods later.
        !
        ! Inputs:
        !   lines   - Y dimension of latitudes and longitudes and output data
        !   samples - X dimension of latitudes and longitudes and output data
        !   sensor  - Caracter array name of the sensor
        !   channel - Character array name of the channel
        !   month   - Integer month
        !   lons    - An NxM array of longitudes as floats
        !   lats    - An NxM array of latitudes as floats
        !
        ! Outputs:
        !   emiss   - An NxM array of surface emissivities (0.0 - 1.0)
        !
        ! Python call signature:
        !   emiss = surf_emiss(sensor, channel, month, lons, lats)
        !
        ! This routine is written as a subroutine in order to allow it to be
        ! compiled using f2py.  Doing so allows the subroutine to be imported by
        ! python routines and used as a normal python module.  Calls to compile this
        ! routine are available in ../Makefile.  Compiling this routine will produce
        ! rayleigh.so.  To import from rayleigh.so simply use
        ! the following import in Python:
        !
        !   from rayleigh import rayleigh
        !
        ! IMPORTANT NOTES:
        ! Typically in Fortran we are able to make use of ALLOCATE statements to
        ! handle dynamic array sizes.  When using F2Py, however, this is not
        ! possible.  All ALLOCATEs must be stripped out of the code.  As a
        ! consequence, there are more required inputs than would typically be
        ! expected.  For instance, "lines" and "samples" are required inputs in
        ! order to determine the shape of the various input arrays.  Normally these
        ! values could be inferred and the arrays could be allocated dynamically,
        ! however, when using F2Py, this is not possible and the values must be
        ! known a priori and passed.
        !
        ! Note that lines and samples are not required in the python call, but are
        ! required in the fortran call.  f2py is smart enough to figure these out
        ! on its own for python, but not for fortran.
        !
        !-------------------------------------------------------------------------------------------

        implicit none
        integer, parameter :: bd = 8
    
        ! INPUTS
        integer(bd), intent(in) :: lines, samples, month
        character(*), intent(in) :: sensor, channel
        real(bd), dimension(lines, samples), intent(in) :: lons, lats
        integer(bd), intent(inout) :: code

        ! Outputs
        real(bd), dimension(lines, samples), intent(out) :: emiss

        !***************************************************************
        ! f2py Signature Information
        !***************************************************************
        !f2py integer(bd), intent(in) :: lines
        !f2py integer(bd), intent(in) :: samples
        !f2py integer(bd), intent(in) :: month
        !f2py character*(*), intent(in) :: sensor
        !f2py character*(*), intent(in) :: channel
        !f2py real(bd), dimension(lines, samples), intent(in) :: lons
        !f2py real(bd), dimension(lines, samples), intent(in) :: lats
        !f2py real(bd), dimension(lines, samples), intent(out) :: emiss
        !f2py integer(bd), intent(out) :: code

        code = 0

        call f_surface_emissivity(sensor, channel, month, lons, lats, emiss, code)

    end subroutine surface_emissivity

    subroutine f_surface_emissivity(sensor, channel, month, lons, lats, emiss, code)
        !-------------------------------------------------------------------------------------------
        ! This subroutine will retrieve surface emissivity values for a specific
        ! sensor on the specifed month at the specified latitudes and longitudes.
        !
        ! The emissivity database used here is a dataset of ten-year average surface
        ! emissivity on a 0.05 x 0.05 (longitude x latitude) degree grid.
        ! Interpolation is performed using nearest neighbor.
        !
        ! Note: May want to offer other interpolation methods later.
        !
        ! Inputs:
        !   lines   - Y dimension of latitudes and longitudes and output data
        !   samples - X dimension of latitudes and longitudes and output data
        !   sensor  - Caracter array name of the sensor
        !   channel - Character array name of the channel
        !   month   - Integer month
        !   lons    - An NxM array of longitudes as floats
        !   lats    - An NxM array of latitudes as floats
        !
        ! Outputs:
        !   emiss   - An NxM array of surface emissivities (0.0 - 1.0)
        !
        ! Python call signature:
        !   emiss = surf_emiss(sensor, channel, month, lons, lats)
        !
        ! This routine is written as a subroutine in order to allow it to be
        ! compiled using f2py.  Doing so allows the subroutine to be imported by
        ! python routines and used as a normal python module.  Calls to compile this
        ! routine are available in ../Makefile.  Compiling this routine will produce
        ! rayleigh.so.  To import from rayleigh.so simply use
        ! the following import in Python:
        !
        !   from rayleigh import rayleigh
        !
        ! IMPORTANT NOTES:
        ! Typically in Fortran we are able to make use of ALLOCATE statements to
        ! handle dynamic array sizes.  When using F2Py, however, this is not
        ! possible.  All ALLOCATEs must be stripped out of the code.  As a
        ! consequence, there are more required inputs than would typically be
        ! expected.  For instance, "lines" and "samples" are required inputs in
        ! order to determine the shape of the various input arrays.  Normally these
        ! values could be inferred and the arrays could be allocated dynamically,
        ! however, when using F2Py, this is not possible and the values must be
        ! known a priori and passed.
        !
        ! Note that lines and samples are not required in the python call, but are
        ! required in the fortran call.  f2py is smart enough to figure these out
        ! on its own for python, but not for fortran.
        !
        !-------------------------------------------------------------------------------------------

        implicit none
        integer, parameter :: bd = 8
    
        ! INPUTS
        integer(bd), intent(in) :: month
        character(*), intent(in) :: sensor, channel
        real(bd), dimension(:, :), intent(in) :: lons, lats
        integer(bd), intent(inout) :: code

        ! INTERNAL
        integer :: lines, samples, stat
        logical :: fexist
        character(len=64) :: storage_pref
        character(len=512) :: storage_fname
        integer :: sind
        real(bd) :: res = 0.05
        character(2) :: month_str
        character(64) :: channel_str
        character(512) :: fname
        logical(bd) :: found
        integer(bd) :: chan_ind, i, j, recn
        integer(bd), dimension(size(lons, 1), size(lons, 2)) :: line_inds, sample_inds
        real(4) :: emiss_temp
        character(64) :: satellite
        logical, dimension(size(lons, 1), size(lons, 2)) :: good

        ! RECORDS
        integer :: thread, lock_r, stor_r, data_r

        ! Sensor channels
        character(3), dimension(8) :: ahi_chnames = &
            (/ 'B03', 'B05', 'B07', 'B08', 'B11', 'B13', 'B14', 'B15' /)
        character(3), dimension(8) :: ahi_channels = &
            (/ '006', '016', '039', '062', '085', '103', '112', '123' /)
        character(3), dimension(8) :: abi_chnames = &
            (/ 'B02', 'B05', 'B07', 'B08', 'B11', 'B13', 'B14', 'B15' /)
        character(3), dimension(8) :: abi_channels = &
            (/ '006', '016', '039', '062', '085', '103', '112', '123' /)
        character(3), dimension(8) :: seviri_chnames = &
            (/ 'B01', 'B03', 'B04', 'B05', 'B07', 'B09', 'B10', 'B11' /)
        character(3), dimension(8) :: seviri_channels = &
            (/ '006', '016', '039', '062', '087', '108', '120', '134' /)
    
        ! Outputs
        real(bd), dimension(:, :), intent(out) :: emiss

        code = 0

        lines = size(lons, 1)
        samples = size(lons, 2)

        ! Get file unit numbers
        thread = 1
        !$ thread = omp_get_thread_num()
        ! lock_r = 10 * (thread + 1) + 4
        ! stor_r = 10 * (thread + 1) + 5
        ! data_r = 10 * (thread + 1) + 6

        ! Check the inputs
        call test_month(month, code)
        if (code .ne. 0) then
            return
        endif
        write(month_str, '(I2.2)') month

        ! Get prefix for filename
        storage_pref = trim(sensor)//trim(channel)//trim(month_str)

        ! Get storage filename
        call get_storage_fname(storage_pref, res, res, lons, lats, storage_fname, code)
        print *, trim(storage_fname)

        ! Look for a lock file (indicates currently being written)
        open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
              iostat=stat)
        do while (stat /= 0)
            print *, 'File locked ', trim(storage_fname)//'.processing'
            call sleep(1)
            open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
                  iostat=stat)
        enddo

        ! If the storage file exists, then read from it
        inquire(file=storage_fname, exist=fexist)
        if (fexist) then
            ! Delete the tempfile
            close(lock_r, status='delete')
            print *, 'Reading emissivity'
            open(newunit(stor_r), file=storage_fname, status='old', recl=size(emiss)*bd, access='direct', &
                action='read')
            read(stor_r, rec=1) emiss
            close(stor_r)
        else
            ! Get the line and ssample indexes that need to be read
            call get_inds(res, res, lons, lats, line_inds, sample_inds, code)
            if (code .ne. 0) then
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif

            ! Create file name
            do sind = 1, size(supported_sensors)
                ! Find the index of the channel in the channel array
                chan_ind = 0
                found = .false.
                if ((sensor == 'AHI') .or. (sensor == 'ahi')) then
                    satellite = 'HIMAWARI-8'
                    do while (.not. found)
                        chan_ind = chan_ind + 1
                        if (trim(to_upper(channel)) == trim(ahi_chnames(chan_ind))) then
                            found = .true.
                        else if (chan_ind .gt. size(ahi_chnames)) then
                            call ValueError('Channel not found.', code)
                            ! If the tempfile exists, then delete it
                            inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                            if (fexist) then
                                close(lock_r, status='delete')
                            endif
                            return
                        endif
                    enddo
                    channel_str = ahi_channels(chan_ind)
                else if ((sensor == 'ABI') .or. (sensor == 'abi')) then
                    satellite = 'GOESR'
                    do while (.not. found)
                        chan_ind = chan_ind + 1
                        if (trim(to_upper(channel)) == trim(abi_chnames(chan_ind))) then
                            found = .true.
                        else if (chan_ind .gt. size(abi_chnames)) then
                            call ValueError('Channel not found.', code)
                            ! If the tempfile exists, then delete it
                            inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                            if (fexist) then
                                close(lock_r, status='delete')
                            endif
                            return
                        endif
                    enddo
                    channel_str = abi_channels(chan_ind)
                else if ((sensor == 'CLAVRX-ABI') .or. (sensor == 'clavrx-abi')) then
                    satellite = 'GOESR'
                    do while (.not. found)
                        chan_ind = chan_ind + 1
                        if (trim(to_upper(channel)) == trim(abi_chnames(chan_ind))) then
                            found = .true.
                        else if (chan_ind .gt. size(abi_chnames)) then
                            call ValueError('Channel not found.', code)
                            ! If the tempfile exists, then delete it
                            inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                            if (fexist) then
                                close(lock_r, status='delete')
                            endif
                            return
                        endif
                    enddo
                    channel_str = abi_channels(chan_ind)
                else if ((sensor == 'SEVIRI') .or. (sensor == 'seviri')) then
                    satellite = 'MSG'
                    do while (.not. found)
                        chan_ind = chan_ind + 1
                        if (trim(to_upper(channel)) == trim(seviri_chnames(chan_ind))) then
                            found = .true.
                        else if (chan_ind .gt. size(seviri_chnames)) then
                            call ValueError('Channel not found.', code)
                            ! If the tempfile exists, then delete it
                            inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                            if (fexist) then
                                close(lock_r, status='delete')
                            endif
                            return
                        endif
                    enddo
                    channel_str = seviri_channels(chan_ind)
                endif
            enddo
            if (.not. found) then
                call ValueError('Sensor not supported.', trim(sensor), code)
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif

            ! Find the correct filename
            fname = trim(ancildat_path) // '/emiss/' // to_upper(trim(sensor)) // &
                    '/MEAN_' // trim(month_str) // '_EMISS_' // trim(satellite) // '_' // &
                    to_upper(trim(sensor)) // '_' // to_upper(trim(channel_str)) // '.DAT'
            print *, 'Gathering emissivity data from ', trim(fname)
            inquire(file=fname, exist=fexist)
            if (.not. fexist) then
                call NoSuchFileError(trim(fname), code)
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif
        
            ! Open the file for reading
            good = ((line_inds .gt. -999) .and. (sample_inds .gt. -999))
            call print_min_mean_max('Emiss Line Inds', line_inds, good)
            call print_min_mean_max('Emiss Sample Inds', sample_inds, good)
            call print_min_mean_max('Emiss Abs Inds', (line_inds - 1) * 7200 + sample_inds, good)
            open(newunit(data_r), file=fname, recl=4, access='direct', form='unformatted', action='read')
            print *, 'Emiss File Unit: ', data_r, ' ', trim(fname)
            do i=1, lines
                do j=1, samples
                    if ((line_inds(i, j) .gt. -999) .and. (sample_inds(i, j) .gt. -999)) then
                        recn = ((line_inds(i, j) - 1) * 7200) + sample_inds(i, j)
                        read(data_r, rec=recn) emiss_temp
                        emiss(i, j) = real(emiss_temp, bd)
                    else
                        emiss(i, j) = -999.0
                    endif
                enddo
            enddo
            close(data_r)

            ! Write out the data
            print *, 'Writing emissivity ', trim(storage_fname)
            open(newunit(stor_r), file=storage_fname, recl=size(emiss)*bd, access='direct', &
                form='unformatted', action='write')
            write(stor_r, rec=1) emiss
            close(stor_r)
            close(lock_r, status='delete')
        endif

    end subroutine f_surface_emissivity

    subroutine land_sea_mask(lines, samples, lons, lats, mask, code)
        !-------------------------------------------------------------------------------------------
        ! This subroutine will retrieve a land/sea mask for the specified
        ! latitudes and longitudes.  The mask is based on a MODIS land/sea mask
        ! produced in 2001 (should probably be updated) at 30"x30" spatial
        ! resolution.  The mask can take values ranging from 0 to 7 where:
        !
        ! 0 == Shallow ocean
        ! 1 == Land
        ! 2 == Coastline
        ! 3 == Shallow inland water
        ! 4 == Ephemeral water
        ! 5 == Deep inland water
        ! 6 == Moderate ocean
        ! 7 == Deep ocean
        ! 
        ! Inputs:
        !   lines   - Y dimension of latitudes and longitudes and output data
        !   samples - X dimension of latitudes and longitudes and output data
        !   lons    - An NxM array of longitudes as floats
        !   lats    - An NxM array of latitudes as floats
        !
        ! Outputs:
        !   mask    - An NxM array containing land mask values
        !
        ! Python call signature:
        !   mask = land_sea_mask(lons, lats)
        !
        !-------------------------------------------------------------------------------------------

        implicit none
        integer, parameter :: bd = 8
    
        ! INPUTS
        integer(bd), intent(in) :: lines, samples
        real(bd), dimension(lines, samples), intent(in) :: lons, lats

        ! Internal
        integer :: stat
        logical :: fexist
        character(512) :: storage_fname
        character(2) :: storage_pref = 'ls'
        real(bd) :: res = 1.0/120.0 ! 1/2 minute resolution (I think)
        character(512) :: fname = trim(ancildat_path) // '/lw_mask/lw_geo_2001001_v03m.bin'
        integer(bd) :: i, j, recn
        integer(bd), dimension(lines, samples) :: line_inds, sample_inds
        character(1) :: maskchar
        logical, dimension(lines, samples) :: good

        ! RECORDS
        integer :: thread, lock_r, stor_r, data_r

        ! Outputs
        ! Have to read a character, then convert it to a 4 byte integer
        ! It does not appear that there is an 1 byte unsigned integer type
        integer(bd), dimension(lines, samples), intent(out) :: mask
        integer(bd), intent(out) :: code

        !***************************************************************
        ! f2py Signature Information
        !***************************************************************
        !f2py integer(bd), intent(in) :: lines
        !f2py integer(bd), intent(in) :: samples
        !f2py real(bd), dimension(lines, samples), intent(in) :: lons
        !f2py real(bd), dimension(lines, samples), intent(in) :: lats
        !f2py integer(bd), dimension(lines, samples), intent(out) :: mask
        !f2py integer(bd), intent(out) :: code

        ! Get file unit numbers
        thread = 1
        !$ thread = omp_get_thread_num()
        ! lock_r = 10 * (thread + 1) + 7
        ! stor_r = 10 * (thread + 1) + 8
        ! data_r = 10 * (thread + 1) + 9

        ! Get storage filename
        call get_storage_fname(storage_pref, res, res, lons, lats, storage_fname, code)
        print *, trim(storage_fname)

        ! Look for a lock file (indicates currently being written)
        open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
              iostat=stat)
        do while (stat /= 0)
            print *, 'Cannot open file with code: ', stat
            print *, 'File locked ', trim(storage_fname)//'.processing'
            call sleep(1)
            open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
                  iostat=stat)
        enddo

        ! If the storage file exists, then read from it
        inquire(file=storage_fname, exist=fexist)
        if (fexist) then
            ! Delete the tempfile
            close(lock_r, status='delete')
            print *, 'Reading land/sea mask'
            open(newunit(stor_r), file=storage_fname, status='old', access='direct', recl=size(mask)*bd, &
                action='read')
            read(stor_r, rec=1) mask
            close(stor_r)
        else
            ! Get the line and sample indexes that need to be read
            print *, 'Calculating land/sea mask'
            call get_inds(res, res, lons, lats, line_inds, sample_inds, code)
            if (code .ne. 0) then
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif

            ! Find the correct filename
            inquire(file=fname, exist=fexist)
            if (.not. fexist) then
                call NoSuchFileError(fname, code)
            endif
    
            ! Open the file for reading
            good = ((line_inds .gt. -999) .and. (sample_inds .gt. -999))
            call print_min_mean_max('LS Line Inds', line_inds, good)
            call print_min_mean_max('LS Sample Inds', sample_inds, good)
            call print_min_mean_max('LS Abs Inds', (line_inds - 1) * 43200 + sample_inds, good)
            open(newunit(data_r), file=fname, recl=1, access='direct', form='unformatted', action='read')
            print *, 'LS File Unit: ', data_r, ' ', trim(fname)
            do i=1, lines
                do j=1, samples
                    if ((line_inds(i, j) .gt. -999.0) .and. (sample_inds(i, j) .gt. -999.0)) then
                        recn = ((line_inds(i, j) - 1) * 43200) + sample_inds(i, j)
                        ! print *, rec, line_inds(i, j), sample_inds(i, j), lons_pos(i, j), lats_pos(i, j)
                        read(data_r, rec=recn) maskchar
                        mask(i, j) = int(ichar(maskchar), bd)
                    else
                        mask(i, j) = -999
                    endif
                enddo
            enddo
            close(data_r)

            ! Write out the data
            print *, 'Writing land/sea mask ', trim(storage_fname)
            open(newunit(stor_r), file=storage_fname, recl=size(mask)*bd, access='direct', &
                form='unformatted', action='write')
            write(stor_r, rec=1) mask
            close(stor_r)
            close(lock_r, status='delete')
        endif

    end subroutine land_sea_mask

    subroutine elevation(lines, samples, lons, lats, elev, code)
        !-------------------------------------------------------------------------------------------
        ! This subroutine will retrieve mean elevation for each latitude and
        ! longitude.  Elevations are based on the GMTED2010 dataset, at
        ! 30 arc second resoution.  Values are in meters.
        !
        ! Inputs:
        !   lines   - Y dimension of latitudes and longitudes and output data
        !   samples - X dimension of latitudes and longitudes and output data
        !   lons    - An NxM array of longitudes as floats
        !   lats    - An NxM array of latitudes as floats
        !
        ! Outputs:
        !  elev - An NxM array containing elevation values in meters
        !
        ! Python call signature:
        !   elev = elevation(lons, lats)
        !
        !-------------------------------------------------------------------------------------------
        implicit none
        integer, parameter :: bd = 8

        ! INPUTS
        integer(bd), intent(in) :: lines, samples
        real(bd), dimension(lines, samples) :: lons, lats

        ! Internal
        integer :: stat
        logical :: fexist
        character(512) :: storage_fname
        character(4) :: storage_pref = 'elev'
        real(bd) :: res = 1.0/120.0 ! 30 arc second data
        character(512) :: fname = trim(ancildat_path) // '/elevation/elevation.bin'
        integer(bd) :: i, j, recn
        integer(bd), dimension(lines, samples) :: line_inds, sample_inds
        integer(4) :: elev0
        logical, dimension(lines, samples) :: good

        ! Records
        integer :: thread, lock_r, stor_r, data_r

        ! Outputs
        integer(bd), dimension(lines, samples), intent(out) :: elev
        integer(bd), intent(out) :: code

        !***************************************************************
        ! f2py Signature Information
        !***************************************************************
        !f2py integer(bd), intent(in) :: lines
        !f2py integer(bd), intent(in) :: samples
        !f2py real(bd), dimension(lines, samples), intent(in) :: lons
        !f2py real(bd), dimension(lines, samples), intent(in) :: lats
        !f2py integer(bd), dimension(lines, samples), intent(out) :: elev
        !f2py integer(bd), intent(out) :: code

        ! Get file unit numbers
        thread = 1
        !$ thread = omp_get_thread_num()
        ! lock_r = 10 * (thread + 1) + 7
        ! stor_r = 10 * (thread + 1) + 8
        ! data_r = 10 * (thread + 1) + 9

        ! Get storage filename
        call get_storage_fname(storage_pref, res, res, lons, lats, storage_fname, code)
        print *, trim(storage_fname)

        ! Look for a lock file (indicates currently being written)
        open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
              iostat=stat)
        do while (stat /= 0)
            print *, 'File locked ', trim(storage_fname)//'.processing'
            call sleep(1)
            open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
                  iostat=stat)
        enddo

        ! If the storage file exists, then read from it
        inquire(file=storage_fname, exist=fexist)
        if (fexist) then
            ! Delete the tempfile
            close(lock_r, status='delete')
            print *, 'Reading elevation'
            open(newunit(stor_r), file=storage_fname, status='old', access='direct', recl=size(elev)*bd, &
                action='read')
            read(stor_r, rec=1) elev
            close(stor_r)
        else
            ! Get the line and sample indexes that need to be read
            print *, 'Calculating elevation'
            call get_inds(res, res, lons, lats, line_inds, sample_inds, code)
            if (code .ne. 0) then
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif

            ! Find the correct filename
            inquire(file=fname, exist=fexist)
            if (.not. fexist) then
                call NoSuchFileError(fname, code)
            endif

            ! Open the file for reading
            good = ((line_inds .gt. -999) .and. (sample_inds .gt. -999))
            call print_min_mean_max('Elev Line Inds', line_inds, good)
            call print_min_mean_max('Elev Sample Inds', sample_inds, good)
            call print_min_mean_max('Elev Abs Inds', (line_inds - 1) * 43200 + sample_inds, good)
            open(newunit(data_r), file=fname, recl=4, access='direct', form='unformatted', action='read')
            print *, 'Elev File Unit: ', data_r, ' ', trim(fname)
            do i=1, lines
                do j=1, samples
                    if ((line_inds(i, j) .gt. -999.0) .and. (sample_inds(i, j) .gt. -999.0)) then
                        recn = ((line_inds(i, j) - 1) * 43200) + sample_inds(i, j)
                        ! print *, line_inds(i, j), sample_inds(i, j)
                        read(data_r, rec=recn) elev0
                        elev(i, j) = int(elev0, bd)
                    else
                        elev(i, j) = -999
                    endif
                enddo
            enddo
            close(data_r)

            ! Write out the data
            print *, 'Writing elevation', trim(storage_fname)
            open(newunit(stor_r), file=storage_fname, recl=size(elev)*bd, access='direct', &
                form='unformatted', action='write')
            write(stor_r, rec=1) elev
            close(stor_r)
            close(lock_r, status='delete')
        endif
    end subroutine elevation

    subroutine city_lights(lines, samples, lons, lats, rad, code)
        !-------------------------------------------------------------------------------------------
        ! This subroutine will retrieve mean radiance for each latitude and
        ! longitude as derived from the VIIRS Day/Night band.
        ! 15 arc second resoution.  Values are in radiance (unknown units at this time)
        !
        ! Inputs:
        !   lines   - Y dimension of latitudes and longitudes and output data
        !   samples - X dimension of latitudes and longitudes and output data
        !   lons    - An NxM array of longitudes as floats
        !   lats    - An NxM array of latitudes as floats
        !
        ! Outputs:
        !  rad - An NxM array containing radiance values
        !
        ! Python call signature:
        !   rad = city_lights(lons, lats)
        !
        !-------------------------------------------------------------------------------------------
        implicit none
        integer, parameter :: bd = 8

        ! INPUTS
        integer(bd), intent(in) :: lines, samples
        real(bd), dimension(lines, samples) :: lons, lats

        ! Internal
        integer :: stat
        logical :: fexist
        character(512) :: storage_fname
        character(2) :: storage_pref = 'cl'
        real(bd) :: res = 1.0/240.0 ! 15 arc second data
        character(512) :: fname = trim(ancildat_path) // '/city_lights/city_lights.bin'
        integer(bd) :: i, j, recn
        integer(bd), dimension(lines, samples) :: line_inds, sample_inds !, recn
        real(4) :: rad0
        logical, dimension(lines, samples) :: good

        ! Records
        integer :: thread, lock_r, stor_r, data_r

        ! Outputs
        real(bd), dimension(lines, samples), intent(out) :: rad
        integer(bd), intent(out) :: code

        !***************************************************************
        ! f2py Signature Information
        !***************************************************************
        !f2py integer(bd), intent(in) :: lines
        !f2py integer(bd), intent(in) :: samples
        !f2py real(bd), dimension(lines, samples), intent(in) :: lons
        !f2py real(bd), dimension(lines, samples), intent(in) :: lats
        !f2py real(bd), dimension(lines, samples), intent(out) :: rad
        !f2py integer(bd), intent(out) :: code

        ! Get file unit numbers
        thread = 1
        !$ thread = omp_get_thread_num()
        ! lock_r = 10 * (thread + 1) + 7
        ! stor_r = 10 * (thread + 1) + 8
        ! data_r = 10 * (thread + 1) + 9

        ! Get storage filename
        call get_storage_fname(storage_pref, res, res, lons, lats, storage_fname, code)
        print *, trim(storage_fname)

        ! Look for a lock file (indicates currently being written)
        open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
              iostat=stat)
        do while (stat /= 0)
            print *, 'File locked ', trim(storage_fname)//'.processing'
            call sleep(1)
            open(newunit(lock_r), file=trim(storage_fname)//".processing", status='new', action='write', &
                  iostat=stat)
        enddo

        ! If the storage file exists, then read from it
        inquire(file=storage_fname, exist=fexist)
        if (fexist) then
            ! Delete the tempfile
            close(lock_r, status='delete')
            print *, 'Reading city lights'
            open(newunit(stor_r), file=storage_fname, status='old', access='direct', recl=size(rad)*bd, &
                action='read')
            read(stor_r, rec=1) rad
            close(stor_r)
        else
            ! Get the line and sample indexes that need to be read
            print *, 'Calculating city lights'
            call get_inds(res, res, lons, lats, line_inds, sample_inds, code)
            if (code .ne. 0) then
                ! If the tempfile exists, then delete it
                inquire(file=trim(storage_fname)//'.processing', exist=fexist)
                if (fexist) then
                    close(lock_r, status='delete')
                endif
                return
            endif

            ! Find the correct filename
            inquire(file=fname, exist=fexist)
            if (.not. fexist) then
                call NoSuchFileError(fname, code)
            endif

            ! Open the file for reading
            good = ((line_inds .gt. -999) .and. (sample_inds .gt. -999))
            call print_min_mean_max('CL Line Inds', line_inds, good)
            call print_min_mean_max('CL Sample Inds', sample_inds, good)
            call print_min_mean_max('CL Abs Inds', (line_inds - 1) * 86400 + sample_inds, good)
            open(newunit(data_r), file=fname, recl=4, access='direct', form='unformatted', action='read')
            print *, 'CL File Unit: ', data_r, ' ', trim(fname)
            do i=1, lines
                do j=1, samples 
                    if ((line_inds(i, j) .gt. -999.0) .and. (sample_inds(i, j) .gt. -999.0)) then
                        recn = ((line_inds(i, j) - 1) * 86400) + sample_inds(i, j)
                        read(data_r, rec=recn) rad0 
                        rad(i, j) = int(rad0, bd)
                    else
                        rad(i, j) = -999
                    endif
                enddo
            enddo
            close(data_r)

            ! Write out the data
            print *, 'Writing city lights', trim(storage_fname)
            open(newunit(stor_r), file=storage_fname, recl=size(rad)*bd, access='direct', &
                form='unformatted', action='write')
            write(stor_r, rec=1) rad 
            close(stor_r)
            close(lock_r, status='delete')
        endif
    end subroutine city_lights 

end module ancildat
