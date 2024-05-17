program main
use appgraphics, only: startidle, loop, closewindow
use m_globals
use m_screen
use m_tetromino
use m_grid
implicit none

    integer :: root_menu, file_menu, game_menu, about_menu
    integer :: item_temp

    character(20) :: crows, clevel, cscore

    logical :: playing = .true., gameover = .false.

    type(piece) :: p, np
    integer :: x, y
    integer :: oldrot
    integer :: key
    logical :: drop = .false.

    integer :: screen

    real(kind=4) :: t1,t2

    call random_init(.false., .true.)

    call shapes%init()

    screen = init_screen(312, 444)

    root_menu = addmenu("", MENU_FOR_WINDOW)
    file_menu = addmenu("File", root_menu)
    game_menu = addmenu("Game", root_menu)
    about_menu = addmenu("Help", root_menu)

    item_temp = addmenuitem("Quit", file_menu, quitgame)
    item_temp = addmenuitem("New", game_menu, newgame)
    item_temp = addmenuitem("About...", about_menu, aboutgame)

    call newgame()

    t1 = secnds(0.0)

    do while(playing)
        if (kbhit()) then
            key = getch()

            if (key==KEY_UP) then
                oldrot = p%rotation
                p%rotation = modulo(p%rotation + 1, ubound(shapes%shapes(p%tetromino)%coords,dim=2) + 1)
                if (.not.canmove(p,x,y)) p%rotation = oldrot
            endif

            if (key==KEY_LEFT) then
                if(canmove(p,x-1,y)) x = x - 1
            endif

            if (key==KEY_RIGHT) then
                if(canmove(p,x+1,y)) x = x + 1
            endif

            if (key==KEY_DOWN .or. key==iachar(' ')) drop = .true.
        endif

        if (drop) then
            do while (canmove(p,x,y+1))
                y = y + 1
            end do

            call store_tetromino(p,x,y)

            if (y < 0 ) then
                gameover = .true.
            else
                x = startx
                y = starty
                p = np
                call shapes%random(np)
            end if

            drop = .false.
            t1 = secnds(0.0)
        else
            t2 = secnds(0.0)
            if(t2-t1 >= movedelay) then
                if(.not.canmove(p,x,y+1)) then
                    call store_tetromino(p,x,y)

                    if (y < 0 ) then
                        gameover = .true.
                    else
                        x = startx
                        y = starty
                        p = np
                        call shapes%random(np)
                    end if
                else
                    y = y + 1
                endif
                t1 = t2
            endif
        endif

        call setbkcolor(DARKGRAY)
        call clearviewport()

        call setviewport(4,0,204,440,.true.)
        call setbkcolor(LIGHTGRAY)
        call clearviewport
        call draw_grid()
        call draw_tetromino(p,x,y)
        call rectangle(0,0,199,439)
        call resetviewport()

        call setviewport(208,4,308,104,.true.)
        call setbkcolor(LIGHTGRAY)
        call clearviewport
        call draw_nextpiece(np)
        call rectangle(0,0,99,99)
        call resetviewport()

        call setviewport(208,112,308, 184, .true.)
        call setbkcolor(LIGHTGRAY)
        call clearviewport()
        call settextstyle(SERIF_FONT, HORIZ_DIR, 20)
        write(cscore, "('Score: ', I4)") score
        call settextxy(4,4)
        call outtext(cscore)
        write(clevel, "('Level: ', I4)") level
        call settextxy(4,24)
        call outtext(clevel)
        write(crows, "('Rows: ', I4)") rows
        call settextxy(4,44)
        call outtext(crows)
        call rectangle(0,0,99,71)
        call resetviewport()

        if (gameover) then
            call settextstyle(SERIF_FONT, HORIZ_DIR, 40)
            call settextxy(20,200)
            call outtext("Game Over")
        end if

        call swapbuffers()

        if (gameover) then
            do while(gameover .and. playing)
                if (kbhit()) then
                    key = getch()
                end if
                call startidle(10)
            end do
        else
            call startidle(10)
        end if
    end do

    call closewindow(screen)


    contains


        subroutine quitgame()
        implicit none

            playing = .false.
            call stopidle()
        end subroutine quitgame


        subroutine newgame()
        implicit none

            level = 0
            score = 0
            rows = 0
            movedelay = delays(level)
            x = startx
            y = starty
            gameover = .false.
            call init_grid()
            call shapes%random(p)
            call shapes%random(np)
        end subroutine newgame


        subroutine aboutgame()
        use iso_c_binding
        implicit none

            character(2)::ff
            character(512)::msg

            ! To create multiple lines in our message box, we need to have
            ! a \r\n for windows.  For brevity, we can create that variable
            ! here to hold the two characters.
            ff = C_CARRIAGE_RETURN//C_NEW_LINE

            msg = repeat(' ', 512)
            msg = "Tetris"//ff//"A simple game sample using AppGraphics."//ff//ff//&
                "Please feel free to modify and use this code"//ff//&
                "in any way you wish."

            call dlgmessage(DIALOG_INFO, msg)

        end subroutine aboutgame


end program main
