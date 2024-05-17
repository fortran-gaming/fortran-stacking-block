module m_grid
use appgraphics
implicit none

    integer, parameter :: mincol = 0,  maxcol = 9, minrow = -4, maxrow = 21

    integer::grid(mincol:maxcol, minrow:maxrow)


    contains

    subroutine init_grid
    implicit none

        grid = 0
    end subroutine init_grid


    logical function canmove(p, x, y)
    use m_globals
    use m_tetromino
    implicit none

        type(piece), intent(in) :: p
        integer, intent(in) :: x, y

        integer :: i

        do i=lbound(shapes%shapes(p%tetromino)%coords, dim=1),ubound(shapes%shapes(p%tetromino)%coords, dim=1),1
            if (shapes%shapes(p%tetromino)%coords(i,p%rotation)%x + x < mincol .or. &
                    shapes%shapes(p%tetromino)%coords(i,p%rotation)%x + x > &
                    maxcol .or. shapes%shapes(p%tetromino)%coords(i,p%rotation)%y + y > maxrow) then
                canmove = .false.
                return
            else if (grid(shapes%shapes(p%tetromino)%coords(i,p%rotation)%x + x, &
                        shapes%shapes(p%tetromino)%coords(i,p%rotation)%y+ y) /= 0) then
                canmove = .false.
                return
            end if
        end do

        canmove = .true.

    end function canmove


    subroutine del_rows(y, p)
    use m_globals
    use m_tetromino
    implicit none

        type(piece), intent(in) :: p
        integer, intent(in) :: y
        integer :: i, j

        do_rows: do i=y, 0,-1
            do j=lbound(grid, dim=1), ubound(grid, dim=1), 1
                if (grid(j,i) == 0) cycle do_rows
            end do
            do j=i,0,-1
                grid(:,j) = grid(:,j-1)
            end do
            rows = rows + 1
            level = rows/10
            if (level >= ubound(delays, dim=1)) then
                level = ubound(delays, dim=1)
            end if
            movedelay = delays(level)
            score = score + shapes%shapes(p%tetromino)%score(p%rotation)
        end do do_rows
    end subroutine del_rows


    subroutine store_tetromino(p,x,y)
    use m_globals
    use m_tetromino
    implicit none

        type(piece), intent(in) :: p
        integer, intent(in) :: x, y

        integer::i

        do i=lbound(shapes%shapes(p%tetromino)%coords, dim=1), ubound(shapes%shapes(p%tetromino)%coords, dim=1),1
            grid(shapes%shapes(p%tetromino)%coords(i,p%rotation)%x + x, &
                shapes%shapes(p%tetromino)%coords(i,p%rotation)%y + y) = shapes%shapes(p%tetromino)%color
            call del_rows(shapes%shapes(p%tetromino)%coords(i,p%rotation)%y + y, p)
        end do

    end subroutine store_tetromino


end module m_grid
