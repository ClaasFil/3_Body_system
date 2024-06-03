module constants
    implicit none
    real(8), parameter :: G = 6.67430e-11  ! Gravitational constant
end module constants

module object
    implicit none
    type :: Obj
        real(8) :: mass
        real(8), dimension(3) :: position
        real(8), dimension(3) :: velocity
        real(8), dimension(3) :: acceleration
    end type Obj
end module object

module netcdf_writer
    use netcdf
    implicit none
contains
    subroutine create_netcdf(outfile, n, nsteps)
        implicit none
        character(len=*), intent(in) :: outfile
        integer, intent(in) :: n, nsteps
        integer :: ncid, x_dimid, y_dimid, t_dimid, varid, dimids(3)
        integer :: status

        ! Create a new NetCDF file. NC_CLOBBER overwrites any existing file.
        status = nf90_create(trim(outfile), NF90_CLOBBER, ncid)
        if (status /= NF90_NOERR) then
            print *, 'Error creating NetCDF file:', trim(outfile)
            return
        end if

        ! Define the dimensions.
        status = nf90_def_dim(ncid, "time", nsteps, t_dimid)
        status = nf90_def_dim(ncid, "n_objects", n, x_dimid)
        status = nf90_def_dim(ncid, "xyz", 3, y_dimid)

        ! Define a new variable in the root group. The type of the variable is real.
        dimids = (/t_dimid, x_dimid, y_dimid/)
        status = nf90_def_var(ncid, "positions", NF90_DOUBLE, dimids, varid)

        ! End define mode.
        status = nf90_enddef(ncid)

        ! Close the NetCDF file.
        status = nf90_close(ncid)

        if (status /= NF90_NOERR) then
            print *, 'Error defining NetCDF structure:', trim(outfile)
        else
            print *, 'NetCDF file structure defined successfully:', trim(outfile)
        end if
    end subroutine create_netcdf

    subroutine write_to_netcdf(outfile, T, step)
        implicit none
        real(8), dimension(:,:,:), intent(in) :: T
        character(len=*), intent(in) :: outfile
        integer, intent(in) :: step
        integer :: ncid, varid
        integer :: start(3), count(3)
        integer :: status

        ! Open the existing NetCDF file.
        status = nf90_open(trim(outfile), NF90_WRITE, ncid)
        if (status /= NF90_NOERR) then
            print *, 'Error opening NetCDF file:', trim(outfile)
            return
        end if

        ! Get the variable ID.
        status = nf90_inq_varid(ncid, "positions", varid)

        ! Define the start and count arrays for writing data.
        start = (/step, 1, 1/)
        count = (/1, size(T, 2), size(T, 3)/)

        ! Write the data to the NetCDF file.
        status = nf90_put_var(ncid, varid, T, start=start, count=count)

        ! Close the NetCDF file.
        status = nf90_close(ncid)

        if (status /= NF90_NOERR) then
            print *, 'Error writing to NetCDF file:', trim(outfile)
        end if
    end subroutine write_to_netcdf
end module netcdf_writer



module namelist_utilities
    implicit none

    contains

    
    subroutine read_namelist(file_path, n_max, dt, nsteps, outfile, objectfile)
        use, intrinsic :: iso_fortran_env, only: stderr => error_unit
        character(len=*), intent(in)    :: file_path
        integer,          intent(inout) :: n_max
        real(8),          intent(inout) :: dt
        integer,          intent(inout) :: nsteps
        character(len=*), intent(inout) :: outfile, objectfile
        integer :: fu, rc
    
        ! Namelist definition.
        namelist /INPUTS/ n_max, dt, nsteps, outfile, objectfile
    
        ! Check whether file exists.
        inquire (file=file_path, iostat=rc)
        if (rc /= 0) then
            write (stderr, '("Error: input file ", a, " does not exist")') file_path
            return
        end if
    
        ! Open and read Namelist file.
        open (action='read', file=file_path, iostat=rc, newunit=fu)
        read (nml=INPUTS, iostat=rc, unit=fu)
        if (rc /= 0) write (stderr, '("Error: invalid Namelist format")')
    
        close (fu)
    end subroutine read_namelist

    


end module namelist_utilities








program main
    use constants
    use object
    use netcdf_writer
    use namelist_utilities
    implicit none

    character(len=50) :: namelistFile = "data/namelist/1big_1small.nml"

    integer :: n             ! number of bodies will be determent by .xt file
    integer :: n_max =  2  ! maximum number of objects will be set by name list
    real(8) :: dt = 0.00001        ! time step
    real(8) :: distance_magnitude           ! distance between two bodies
    real(8), dimension(3) :: update         ! update to acceleration
    integer :: i, j, k                         ! loop variables
    integer :: nsteps = 10000               ! number of time steps
    type(Obj), allocatable :: objects(:)      ! array of objects
    character(len=50) :: outfile = "data/pos.nc"
    character(len=50) :: objectfile = "data/objects/multiyObj_test.txt"
    real(8), allocatable :: positions(:,:)   ! positions array for NetCDF writing
    integer :: unit
    character(len=100) :: line               ! line read from file


    ! Read parameters from the namelist
    call read_namelist(namelistFile, n_max, dt, nsteps, outfile, objectfile)

    print *, "n_max: ", n_max
    print *, "dt: ", dt
    print *, "nsteps: ", nsteps
    print *, "outfile: ", outfile
    print *, "objectfile: ", objectfile



    ! Open the file to count the number of lines (i.e., objects)
    open(unit=unit, file=objectfile, status='old', action='read')


    ! Count the number of lines
    n = 0
    do while (.true.)
        read(unit, '(A)', iostat=i) line
        if (i /= 0) exit
        n = n + 1
    end do

    ! Subtracting the header line from .txt file
    n = n - 1
    if (n > n_max) then
        print *, "Error: Number of objects exceeds the maximum allowed"
        n = n_max
    end if
    

    close(unit)

    ! Allocate arrays based on the number of objects
    allocate(objects(n))
    allocate(positions(n, 3))
  
    ! Open the file containing initial conditions
    open(unit=unit, file=objectfile, status='old', action='read')
  

    ! Read the initial conditions from the file
    do i = 1, n 
        read(unit, *) objects(i)%mass, objects(i)%position, objects(i)%velocity, objects(i)%acceleration
    end do

    close(unit)

    print *, "Initializing with ", n, " objects"



    ! Create the NetCDF file
    call create_netcdf(outfile, n, nsteps)

    ! Time evolution
    do i = 1, nsteps
        !print *, "----------------- Step: ", i
        ! Compute the acceleration
        do j = 1, n
            ! Initaly acc is ero
            objects(j)%acceleration = 0.0
            ! Iterate over al other bodies
            do k = 1, n
                
                if (j /= k) then
                    !print *, j, k
                    
                    ! Compute the distance between the two bodies
                    !print *, "Distance:", norm2(objects(j)%position - objects(k)%position)
                    distance_magnitude = norm2(objects(j)%position - objects(k)%position)
                    if (distance_magnitude < 1e-1) then
                        distance_magnitude = 1e-1
                    end if
                    ! Compute update to accelleration
                    update = G * objects(k)%mass * (objects(j)%position - objects(k)%position) / distance_magnitude**3
                    ! Update acceleration is set to zero for each iterion but updated die each influencing body
                    objects(j)%acceleration = objects(j)%acceleration - ( update)
                    !print *, " resulting acceleration: ", objects(j)%acceleration
                end if
            end do
        end do

        ! Update the velocity
        do j = 1, n
            !print *,  ( dt * objects(j)%acceleration)
            objects(j)%velocity = objects(j)%velocity + dt * objects(j)%acceleration
            
            
        end do

        ! Update the position
        do j = 1, n
            !print *,  ( objects(j)%velocity)
            !print *, "velocity of object     ", j, " is: ", objects(j)%velocity
            !print *, "old position of object ", j, " is: ", objects(j)%position
            objects(j)%position = objects(j)%position + dt * objects(j)%velocity
            !print *, "new position of object ", j, " is: ", objects(j)%position
            
        end do

        ! Store positions for writing to NetCDF
        do j = 1, n
            positions(j, :) = objects(j)%position
        end do

        ! Write the positions to the NetCDF file
        call write_to_netcdf(outfile, reshape(positions, [1, n, 3]), i)
    end do

    
end program main
  