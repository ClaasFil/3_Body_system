module constants
    implicit none
    real, parameter :: G = 6.67430e-11 
end module constants



module body
    implicit none
    type :: Body
      real(8) :: mass
      real(8), dimension(3) :: position
      real(8), dimension(3) :: velocity
      real(8), dimension(3) :: acceleration
    end type Body
end module body








program main
    use constants
    use body
    implicit none

    integer, parameter :: n = 3             ! number of bodies
    real(8), parameter :: dt = 0.001        ! time step
    integer :: i, j                         ! loop variables
    integer :: nsteps = 1000                ! number of time steps
    type(Body), dimension(n) :: bodies      ! array of bodies


    ! Initialize the bodies
    bodies(1)%mass = 1.0
    bodies(1)%position = [0.0, 0.0, 0.0]
    bodies(1)%velocity = [0.0, 0.0, 0.0]
    bodies(1)%acceleration = [0.0, 0.0, 0.0]

    bodies(2)%mass = 1.0
    bodies(2)%position = [1.0, 0.0, 0.0]
    bodies(2)%velocity = [0.0, 0.0, 0.0]
    bodies(2)%acceleration = [0.0, 0.0, 0.0]

    bodies(3)%mass = 1.0
    bodies(3)%position = [0.0, 1.0, 0.0]
    bodies(3)%velocity = [0.0, 0.0, 0.0]
    bodies(3)%acceleration = [0.0, 0.0, 0.0]

    ! Time evolution
    do i = 1, nsteps
        ! Compute the acceleration
        do j = 1, n
            bodies(j)%acceleration = 0.0
            do k = 1, n
                if (j /= k) then
                    bodies(j)%acceleration = bodies(j)%acceleration - G * bodies(k)%mass * (bodies(j)%position - bodies(k)%position) / norm(bodies(j)%position - bodies(k)%position)**3
                end if
            end do
        end do

        ! Update the velocity
        do j = 1, n
            bodies(j)%velocity = bodies(j)%velocity + dt * bodies(j)%acceleration
        end do

        ! Update the position
        do j = 1, n
            bodies(j)%position = bodies(j)%position + dt * bodies(j)%velocity
            ! print positions
            write 
        end do
    end do

    
end program main
  