module m_globals
use m_tetromino
implicit none

    real (kind=4), parameter :: delays(0:5) = (/0.30,0.25,0.20,0.15,0.10,0.08/)
    integer, parameter :: startx = 3, starty = -4

    type(tetrominos) :: shapes
    real (kind=4) :: movedelay
    integer :: score, rows, level

end module m_globals
