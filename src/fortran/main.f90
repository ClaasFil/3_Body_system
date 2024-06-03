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










program main
    use constants
    use object
    use netcdf_writer
    implicit none

    integer, parameter :: n = 4             ! number of bodies
    real(8), parameter :: dt = 0.00001        ! time step
    real(8) :: distance_magnitude           ! distance between two bodies
    real(8), dimension(3) :: update         ! update to acceleration
    integer :: i, j, k                         ! loop variables
    integer :: nsteps = 400000               ! number of time steps
    type(Obj), dimension(n) :: objects      ! array of objects
    character(len=50) :: outfile = "data/pos.nc"
    real(8), dimension(n, 3) :: positions   ! positions array for NetCDF writing


    ! Initialize the objects
    objects(1)%mass = 1e11
    objects(1)%position = [0.0, 1.0, 0.0]
    objects(1)%velocity = [1.0, 0.0, 0.0]
    objects(1)%acceleration = [0.0, 0.0, 0.0]

    objects(2)%mass = 1e11
    objects(2)%position = [0.0, -1.0, 0.0]
    objects(2)%velocity = [-1.0, .0, 0.0]
    objects(2)%acceleration = [0.0, 0.0, 0.0]

    objects(3)%mass = 1e11
    objects(3)%position = [0.0, 0.0, 1.0]
    objects(3)%velocity = [0.0, 1.0, 0.0]
    objects(3)%acceleration = [0.0, 0.0, 0.0]

    objects(4)%mass = 1e11
    objects(4)%position = [0.0, 0.0, -1.0]
    objects(4)%velocity = [0.0, -1.0, 0.0]
    objects(4)%acceleration = [0.0, 0.0, 0.0]





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
  