      program path2kml
      implicit none
      character ch*256, string*256, inp*256, out*256, msg*3,color(0:5)*8
     &         ,name*256, string0*256
      integer iargc, narg,i,j, iinp/10/, iout/11/, length, ln, branch, branch0
      real x,y,z,  numget
      logical exist
        
      inp = ' '
      out = 'path.kml'
      color=( /'FF0000FF','FF00FF00','FFFF0000','FFFF00FF','FFFFFF00','FF00FFFF'/)
      name=' '
      ln=0

      narg=iargc()
      if (narg.eq.0) then
      print *
      print *,'path2kml program'
        print *,'-i input data file '
        print *,' '
        print *,'-o output kml file'
        print *,' '
        print *,'-color line colors'
        print *,' default ',color
        read *
        stop
        endif

        do i = 1,narg
        call getarg (i,ch)
        name(ln+1:) = ch
        ln = length(name)+1
        name(ln:ln) = ' '

          if (ch.eq.'-i') then
          call getarg (i+1,inp)

          else if (ch.eq.'-o') then
          call getarg (i+1,out)

          else if (ch.eq.'-color') then
          j = 0
            do while (ch(1:1).ne.'-'.and.j+i.lt.narg)
            j = j+1
            call getarg (i+j,color(j))
            enddo
          endif
        enddo
c-------------------------------------------------------------------------------

        if (inp.ne.' ') then
        inquire (file=inp,exist=exist)
          if (.not.exist) then
          print *,'inp ',inp(1:length(inp)), ' not exist'
          call exit(1)
          endif

        open(iinp,file=inp,action='read')

        else
        print *,'input file not defined'
        call exit(1)
        endif
c-------------------------------------------------------------------------------
 
        if (out.ne.' ') then
        open(iout,file=out,action='write')

        else
        print *,'output file not defined'
        call exit(1)
        endif
c-------------------------------------------------------------------------------
        write (iout,'(a)') '<?xml version="1.0" encoding="UTF-8"?>'
        write (iout,'(a)') '<kml xmlns="http://www.opengis.net/kml/2.2" '
        write (iout,'(a)') 'xmlns:gx="http://www.google.com/kml/ext/2.2" '
        write (iout,'(a)') 'xmlns:kml="http://www.opengis.net/kml/2.2" '
        write (iout,'(a)') 'xmlns:atom="http://www.w3.org/2005/Atom">'

        write (iout,'(a)') ' <Document>'
        write(iout,'(a)') '<name>'
        write(iout,'(a)') out(1:length(out))
        write(iout,'(a)') '</name>'
        write(iout,'(a)') '<description> '
        write(iout,'(a)') ' <![CDATA[ '
        write(iout,'(a)') name(1:length(name))
        write(iout,'(a)') ' ]]> '
        write(iout,'(a)') ' </description>'
        write (iout,'(a)') '  <open>0</open>'
c-------------------------------------------------------------------------------
        read (iinp,'(a)') string
        read (iinp,'(a)') string
        read (string,*) x,y,z,branch

        do while (.true.)

        branch0=branch

        write(iout,'(a)') '<Placemark>'
        write(iout,'(a)') '<visibility>1</visibility>'
        write(iout,'(a)') '<name>'
        write(iout,'(i2)') branch
        write(iout,'(a)') '</name>'
        write(iout,'(a)') '<LookAt>'

        write(iout,'(a)') '<longitude>'
        write(iout,*) x
        write(iout,'(a)') '</longitude>'
        write(iout,'(a)') '<latitude>'
        write(iout,*) y
        write(iout,'(a)') '</latitude>'
        write(iout,'(a)') '<altitude>0</altitude>'
        write(iout,'(a)') '<range>20000</range>'
        write(iout,'(a)') '<tilt>0</tilt>'
        write(iout,'(a)') '<heading>-0.05183987880918447</heading>'
        write(iout,'(a)') '<altitudeMode>relativeToGround</altitudeMode>'
        write(iout,'(a)') '<gx:altitudeMode>relativeToSeaFloor</gx:altitudeMode>'
        write(iout,'(a)') '</LookAt>'

        write(iout,'(a)') '<Style><LineStyle><color>'
        write(iout,'(a)') color(branch)
        write(iout,'(a)') '</color><width>3</width></LineStyle></Style>'
        write(iout,'(a)') '<LineString>'
        write(iout,'(a)') '<altitudeMode>absolute</altitudeMode>'
        write(iout,'(a)') '<tessellate>1</tessellate>'
        write(iout,'(a)') '<coordinates>'

        write(iout,'(f12.6,",",f12.6,",",f12.6)') x,y,z

*       read (iinp,'(a)') string
        do while (.true.)
        read (iinp,'(a)',end=1000) string
        read (string,*) x,y,z,branch
        write(iout,'(f12.6,",",f12.6,",",f12.6)') x,y,z
          if (branch.ne.branch0) go to 1000
        enddo
        
1000    continue
        write(iout,'(a)') '</coordinates>'
        write(iout,'(a)') '</LineString>'

        write(iout,'(a)') '</Placemark>'
        
       
        read (iinp,'(a)',end=2000) string
        enddo

2000    continue


        write (iout,'(a)') ' </Document>'
        write (iout,'(a)') ' </kml>'

        close (iinp)
        close (iout)
        stop
        end
