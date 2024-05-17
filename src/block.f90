module m_tetromino
use appgraphics
implicit none

    type piece
        integer::tetromino
        integer::rotation
    end type Piece

    type point
        integer::x
        integer::y
    end type Point

    !
    ! Tetromino rotations
    !
    ! ####
    type(point), target :: points1(4,0:1) = reshape((/point(0,1), point(1,1), point(2,1), point(3,1),&
                                                    point(1,0), point(1,1), point(1,2), point(1,3)/), (/4,2/))

    ! ##
    ! ##
    type(point), target :: points2(4,0:0) = reshape((/point(0,0), point(0,1), point(1,0), point(1,1)/),(/4,1/))

    ! #
    ! ##
    ! #
    type(point), target :: points3(4,0:3) = reshape((/point(1,0), point(0,1), point(1,1), point(2,1),&
                                                    point(1,0), point(1,1), point(2,1), point(1,2),&
                                                    point(0,1), point(1,1), point(2,1), point(1,2),&
                                                    point(1,0), point(0,1), point(1,1), point(1,2)/), (/4,4/))

    ! ##
    !  ##
    type(point), target :: points4(4,0:1) = reshape((/point(0,0), point(1,0), point(1,1), point(2,1),&
                                                    point(1,0), point(0,1), point(1,1), point(0,2)/), (/4,2/))

    !  ##
    ! ##
    type(point), target :: points5(4,0:1) = reshape((/Point(1,0),Point(2,0), Point(0,1), Point(1,1),&
                                                    point(0,0), point(0,1), point(1,1), point(1,2)/), (/4,2/))

    ! ###
    ! #
    type(point), target :: points6(4,0:3) = reshape((/point(2,0), point(0,1), point(1,1), point(2,1),&
                                                    point(0,0), point(0,1), point(0,2), point(1,2),&
                                                    point(0,0), point(1,0), point(2,0), point(0,1),&
                                                    point(0,0), point(1,0), point(1,1), point(1,2)/), (/4,4/))

    ! #
    ! ###
    type(point), target :: points7(4,0:3) = reshape((/point(0,0), point(1,0), point(2,0), point(2,1),&
                                                    point(1,0), point(1,1), point(0,2), point(1,2),&
                                                    point(0,0), point(0,1), point(1,1), point(2,1),&
                                                    point(0,0), point(1,0), point(0,1), point(0,2)/), (/4,4/))


    ! Scores for piece rotations
    integer, target :: score1(0:1) = (/5,8/)
    integer, target :: score2(0:0) = (/6/)
    integer, target :: score3(0:3) = (/5,5,6,5/)
    integer, target :: score4(0:1) = (/6,7/)
    integer, target :: score5(0:1) = (/6,7/)
    integer, target :: score6(0:3) = (/6,7,6,7/)
    integer, target :: score7(0:3) = (/6,7,6,7/)


    type tetromino
        type(point), pointer :: coords(:,:)
        integer::color
        integer, pointer :: score(:)
    end type tetromino


    type tetrominos
        type(tetromino) :: shapes(7)

        contains
            procedure::init => init_tetrominos
            procedure::random => random_tetromino

    end type tetrominos


    contains


    subroutine init_tetrominos(self)
    implicit none

        class(tetrominos), intent(inout) :: self

        self%shapes(1)%coords => points1
        self%shapes(1)%color = creatergb(0,255,255)
        self%shapes(1)%score => score1

        self%shapes(2)%coords => points2
        self%shapes(2)%color = creatergb(255,255,0)
        self%shapes(2)%score => score2

        self%shapes(3)%coords => points3
        self%shapes(3)%color = creatergb(255,0,255)
        self%shapes(3)%score => score3

        self%shapes(4)%coords => points4
        self%shapes(4)%color = creatergb(255,0,0)
        self%shapes(4)%score => score4

        self%shapes(5)%coords => points5
        self%shapes(5)%color = creatergb(0,255,0)
        self%shapes(5)%score => score5

        self%shapes(6)%coords => points6
        self%shapes(6)%color = creatergb(255,127,0)
        self%shapes(6)%score => score6

        self%shapes(7)%coords => points7
        self%shapes(7)%color = creatergb(0,0,255)
        self%shapes(7)%score => score7
    end subroutine init_tetrominos


    subroutine random_tetromino(self, p)
    implicit none
        class(tetrominos), intent(in) :: self
        type(Piece), intent(out) :: p

        real(kind=4)::rndnum(2)

        call random_number(rndnum)
        p%tetromino = floor(rndnum(1)*7)+1
        p%rotation = floor(rndnum(2)*(ubound(self%shapes(p%tetromino)%coords,dim=2)+1))

    end subroutine random_tetromino


end module m_tetromino
