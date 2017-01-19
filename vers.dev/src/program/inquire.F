	implicit none
        character file*20
        logical exist
1       print *,'file name'
        read(*,'(a)') file
        if (file.eq.' ') stop
        inquire (file=file,exist=exist)
        print *,'file ',file,' exist=',exist
        go to 1
        end
