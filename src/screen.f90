module m_screen
implicit none

    integer, parameter :: blocksize = 20

    contains

    ! Initializes the screen by creating our window
    function init_screen(width, height)
    use appgraphics
    implicit none

        integer::init_screen
        integer, intent(in)::width, height

        ! Note that we've enabled double-buffering of the screen for performance
        init_screen = initwindow(width, height, title="Tetris", dbflag=.TRUE., closeflag=.TRUE.)

        call setmatchthemetextstyle()

    end function init_screen


    subroutine draw_grid()
    use m_grid
    use appgraphics
    implicit none

        integer :: row, col

        call setcolor(BLACK)

        do row=0,ubound(grid, dim=2),1
            do col=0,ubound(grid, dim=1),1
                if (grid(col,row) /= 0) then
                    call setfillstyle(SOLID_FILL, grid(col,row))
                    call bar(col*blocksize,row*blocksize,col*blocksize+blocksize,row*blocksize+blocksize)
                    call rectangle(col*blocksize,row*blocksize,col*blocksize+blocksize,row*blocksize+blocksize)
                endif
            end do
        end do

    end subroutine draw_grid


    subroutine draw_tetromino(p,x,y)
    use appgraphics
    use m_globals
    use m_tetromino
    implicit none

        type(piece), intent(in) :: p
        integer, intent(in) :: x, y

        integer :: i

        call setcolor(BLACK)

        do i=lbound(shapes%shapes(p%tetromino)%coords, dim=1),ubound(shapes%shapes(p%tetromino)%coords, dim=1),1
            call setfillstyle(SOLID_FILL,shapes%shapes(p%tetromino)%color)
            call bar((shapes%shapes(p%tetromino)%coords(i,p%rotation)%x+x)*blocksize, &
                    (shapes%shapes(p%tetromino)%coords(i,p%rotation)%y+y)*blocksize, &
                    (shapes%shapes(p%tetromino)%coords(i,p%rotation)%x+x)*blocksize+blocksize, &
                    (shapes%shapes(p%tetromino)%coords(i,p%rotation)%y+y)*blocksize+blocksize)
            call rectangle((shapes%shapes(p%tetromino)%coords(i,p%rotation)%x+x)*blocksize, &
                    (shapes%shapes(p%tetromino)%coords(i,p%rotation)%y+y)*blocksize, &
                    (shapes%shapes(p%tetromino)%coords(i,p%rotation)%x+x)*blocksize+blocksize, &
                    (shapes%shapes(p%tetromino)%coords(i,p%rotation)%y+y)*blocksize+blocksize)
        end do

    end subroutine draw_tetromino


    subroutine draw_nextpiece(np)
    use m_globals
    use m_tetromino
    implicit none

        type(piece), intent(in) :: np

        integer :: minx, miny, maxx, maxy
        integer :: i, x, y

        minx = 4
        maxx = 0
        miny = 4
        maxy = 0

        do i=lbound(shapes%shapes(np%tetromino)%coords, dim=1), ubound(shapes%shapes(np%tetromino)%coords, dim=1), 1
            if (shapes%shapes(np%tetromino)%coords(i,np%rotation)%x > maxx) then
                maxx = shapes%shapes(np%tetromino)%coords(i,np%rotation)%x
            end if

            if (shapes%shapes(np%tetromino)%coords(i,np%rotation)%x < minx) then
                minx = shapes%shapes(np%tetromino)%coords(i,np%rotation)%x
            end if

            if (shapes%shapes(np%tetromino)%coords(i,np%rotation)%y > maxy) then
                maxy = shapes%shapes(np%tetromino)%coords(i,np%rotation)%y
            end if

            if (shapes%shapes(np%tetromino)%coords(i,np%rotation)%y < miny) then
                miny = shapes%shapes(np%tetromino)%coords(i,np%rotation)%y
            end if
        end do

        x = (4-(maxx-minx))*blocksize/2-minx*blocksize
        y = (4-(maxy-miny))*blocksize/2-miny*blocksize

        do i=lbound(shapes%shapes(np%tetromino)%coords, dim=1),ubound(shapes%shapes(np%tetromino)%coords, dim=1),1
            call setfillstyle(SOLID_FILL,shapes%shapes(np%tetromino)%color)
            call bar((shapes%shapes(np%tetromino)%coords(i,np%rotation)%x)*blocksize+x, &
                    (shapes%shapes(np%tetromino)%coords(i,np%rotation)%y)*blocksize+y, &
                    (shapes%shapes(np%tetromino)%coords(i,np%rotation)%x)*blocksize+blocksize+x, &
                    (shapes%shapes(np%tetromino)%coords(i,np%rotation)%y)*blocksize+blocksize+y)
            call rectangle((shapes%shapes(np%tetromino)%coords(i,np%rotation)%x)*blocksize+x, &
                    (shapes%shapes(np%tetromino)%coords(i,np%rotation)%y)*blocksize+y, &
                    (shapes%shapes(np%tetromino)%coords(i,np%rotation)%x)*blocksize+blocksize+x, &
                    (shapes%shapes(np%tetromino)%coords(i,np%rotation)%y)*blocksize+blocksize+y)
        end do



    end subroutine draw_nextpiece


end module m_screen
