        Module interface_mod
        implicit none
        real*4 xrange, yrange, pixsize, titsize, legsize
     &        , legtextsize, xleg
     &        , yleg0, pansize, bulletsize,dxmap,dymap, scale
     &        ,dytit, ximage, yimage, vstep
     &        ,xmin,xmax,ymin,ymax, xor, yor
     &        ,xval,yval, dc
        character title*132, legtext*132
c       -----------------------------------------
        integer nmaps,ileg
        parameter (nmaps=100)
        real*4 vmin(nmaps),vmax(nmaps),Smin(nmaps),mult(nmaps)
        integer nvalue(nmaps),nstepcol(nmaps)
        logical*4 leglog(nmaps)
        character palette(nmaps)*18,palout(nmaps)*2,fmt(nmaps)*20 
        character rgbfile(nmaps)*132
c       -----------------------------------------
        real*4 rs(0:10000),gs(0:10000),bs(0:10000),v1(0:10000),v2(0:10000)
        integer*4 nrgb, ncolor,firstscal/0/,  maxcol/841/
     &               ,firstbool /842/, firstnom /850/, background/899/
     &               ,magenta /900/, blue/901/, cyan/902/
     &               ,green /903/, yellow/904/, red/905/,orange/906/
     &               ,gray/907/, black/908/, white/909/,reddark/910/
     &               ,magentadark/911/,whitedark/912/,bluedark/913/
     &               ,greendark/914/,orangedark/915/,transparent/916/
c       -----------------------------------------------------------
        integer*4 nfields, nnomleg,nfields_max
        parameter (nfields=27,nfields_max=200)
        character nomlegend(0:nfields_max)*20
        integer*4   numlegend(0:nfields_max), colleg(0:nfields_max)
        logical*4 legend(0:nfields_max),legend1(0:nfields_max)
c       ------------------------------
        integer*4 npal
        parameter (npal=17)
        integer*4 ipal(npal)
        character cpal(npal)*1, cpalext(npal)*12
c       -----------------------------------------------------------
        logical*4 bullet, bulletcircle, check, Sbullet, outwin, grid, resize
        real*4     mv /z'ffffffff'/
        integer*4  mv4
        integer*1  mv1
        character*1 null
        integer*1  inull /z'00'/
        equivalence (null,inull)
        end module interface_mod
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        program map2image
        use interface_mod
        implicit none
c-------------------------------------------------------------
c interface with maps
      integer*4  npcr, np, ncol, nrow, nmask
 
        real*4, allocatable :: r4(:), val(:)
 
        integer*1, allocatable :: mask(:)
        integer*4  typ, ninpmap
c--------------------------------------------
* gdal additional variables
        integer*4 nband, iband,error, nbyte
        character driver*64, datatype*64, inpfmt*64, outfmt*64
        real*8 sizey
c--------------------------------------------
c local variables
        integer*4 iout/10/, iargc, narg
     &          ,i,j,k,l,m,it0, time, lm, length,getpid, nmv
     &        , numval
        real*8 xor0,yor0,cellsize
        real*4 xor1,yor1,cellsize1, pixsize1
        real*4 x1,y1,x2,y2

        real*4 fred,fgreen,fblue
        logical*4 rmps, exist, existmask,eqmv
        
        character inpmap(nmaps)*132, ch*132, map*132
     &          ,outgif*132, outps*132, maskmap*132
#if defined WIN32
     &          ,bslash*1 /'\'/
#else
     &          ,bslash*1 /'/'/
#endif
        real*4 numget, x, y
        character msg*3, yesno*3
        character transparency*80, geometry*80, command*1024
        
c-------------------------------------
        logical*4 dolegend(nmaps)
        logical*4 doborder(nmaps)
c-------------------------------------
        ipal = (/magenta,blue,cyan,green,yellow,red,black,white,gray
     &         ,orange,reddark,magentadark,whitedark,bluedark
     &         ,greendark,orangedark,transparent/)
        cpal = (/'m','b','c','g','y','r','N','w','H'
     &         ,'o','R','M','W','B','G','O','T'/)
        cpalext = (/'magenta     ','blue        ','cyan        '
     &             ,'green       ','yellow      ','red         '
     &             ,'black       ','white       ','gray        '
     &             ,'orange      ','reddark     ','magentadark '
     &             ,'whitedark   ','bluedark    ','greendark   '
     &             ,'orangedark  ','transparent '/)
c-----------------------------------------------------------
        call getmv(mv)
        call getmv1(mv1)
        call getmv4(mv4)

        print *,'mv=',mv,' mv1=',mv1,' mv4=',mv4
 
        narg = iargc()
        it0 = time()
c-----------------------------------------------------------
        ximage = 1200
        yimage = 800
        pixsize =  -1
        title = ' '

*       titsize = 20.
*       legsize = 16.
*       bulletsize = 4.
        titsize = 15.
        legsize = 12.
        bulletsize = 3.
        legtextsize = -1

        bulletcircle = .true.
        rgbfile = 'none'
        outgif = ' '
        outps = ' '
        inpmap = ' '
        maskmap = ' '
        grid = .false.
        bullet = .true.
        Sbullet = .false.
        check = .false.
        scale = 1
        resize = .false.
c-----------------------------------------------------------
        palette='mbcgyr'
        palout='MR'
        nstepcol = 0
        fmt = 'g10.4'
        leglog = .false.
        nvalue = 5
        vmin = 0
        vmax = 0
        smin = 0
        mult = 1.
        dolegend = .true.
        doborder = .false.
        ileg = 0
c-----------------------------------------------------------
        nnomleg = nfields*2
        legend(0:nfields_max) = .false.
        legend1(0:nfields_max) = .true.
          do i = 0,nfields_max
          numval = i
          numlegend(numval) = numval
          colleg(numval) = mod(i,nfields)
          write (nomlegend(numval),'(i3)') i
          enddo
c-----------------------------------------------------------
        if (narg.eq.0) then
 
        print *,' '
        print *,'map2image help'
        print *,' '
        print *,'convert PCRaster maps into an image file'
        print *,' '
        print *,'Method: create an encapsulated postscript file '
        print *,'and convert to image using "convert" command '
        print *,'see  http://www.imagemagick.org '
        print *,'Note: the map cell size can be different'
        print *,' '
        print *,'-i input map[s]'
        print *,'   no DEFAULT, requested '
        print *,'  only one scalar legend'
        print *,'-map input map '
        print *,'  individual scalar legends are defined for each map' 
        print *,'-doLegend yes/no yes/no ... '
        print *,'   do legend for input maps'
        print *,'   DEFAULT is yes for all maps'
        print *,'-doBorder yes/no yes/no ... '
        print *,'   do border iaround maps '
        print *,'   DEFAULT is no for all maps'
        print *,'-o output image file'
        print *,'   no DEFAULT, requested '
        print *,'   NOTE: the format of the image file is determined'
     &         ,' by its extension'
        print *,'-m maskmap'
        print *,'   map that identify the area used to produce the '
     &         ,'image'
        print *,'   CAUTION: must have the same window of the first '
     &         ,'input map'
        print *,'   no DEFAUULT, not requested'
        print *,'-outps output ps file'
        print *,'   no DEFAULT, NOT requested '
        print *,'   NOTE: if not specified, a temporary file in /tmp '
        print *,'         is created and deleted at the process end'
        print *,'   CAUTION: the extension must be .ps'

        print *,'   Legend for scalar maps'
        print *,'-Slegend vmin vmax nvalue'
        print *,'   Legend for scalar maps'
        print *,'   min. map value, max. map value, no. of values'
        print *,'   NOTE: see Nlegend for nominal maps;'
        print *,'  DEFAULT are the min, max values of the first scalar '
     &         ,'map'
        print *,'         nvalue=',nvalue(1)
        print *,'-Smin smin '
        print *,'   minimum scalar value in the isocolor map'
        print *,'  DEFAULT is ',smin(1)
        print *,'-palette ',cpal
        print *,'   Palette for scalar maps, values between vmin vmax'
        print *,'   define your palette according to the following '
     &         ,'definition:'
        call set_rgb
        print '(a6,3a6)','color','r','g','b'
          do i =1,npal
          call getcolor (cpal(i),fred,fgreen,fblue)
          print '(a6,3i6,3x,a12)',cpal(i),int(fred*255),int(fgreen*255),int(fblue*255),cpalext(i)
          enddo
        print *,'   DEFAULT is ',palette(1)
        print *,'-nstepcol no of step colors between defined rgb colors'
        print *,'   DEFAULT is ',maxcol-1-firstscal,' / no. or '
     &         ,'rgb colors'
        print *,'-palout ',cpal
        print *,'   Palette for scalar maps, values out of vmin vmax'
        print *,'   See palette definition'
        print *,'   DEFAULT is ',palout(1)
        print *,'-fmt fmt (format of legend numbers)'
        print *,'   DEFAULT is ',fmt(1)
        print *,'-mult mult'
        print *,'   multiplier of scalr values'
        print *,'  DEFAULT is ',mult(1)

        print *,'-Ncolor num color'
        print *,'   Color for nominal maps'
        print *,'   DEFAULT color are numbers from 0 to ',nfields-1
        print *,'-Nlegend num legend'
        print *,'   Legend for nominal maps'
        print *,'   NOTE: see Slegend for scalar  maps;'
        print *,'   num is replaced by legend'
        print *,'   DEFAULT legend are numbers from 0 to ',nfields-1
        print *,'-NoLegend num '
        print *,'   no Legend for nominal maps: '
        print *,'   Note: if no num are entered, no legend is produced for all the values'
        print *,'-leglog yes/no'
        print *,'   logarithmic (yes) or linear (no) legend scale'
        print *,'   DEFAULT is ',yesno(leglog)
        print *,'-resize yes/no'
        print *,'   resize with convert command -geometry 1000x800 '
        print *,'   suggested for nested images'
        print *,'   DEFAULT is ',yesno(resize)
        print *,'-grid yes/no'
        print *,'   grid for scalar maps'
        print *,'   DEFAULT is ',yesno(grid)
        print *,'-pixsize pixel size (dots per cell)'
        print *,'   DEFAULT is max ((ximage-dxmap)/ncols,(yimage-dymap)/nrows)'
        print *,'   where ximage=',ximage,' yimage=',yimage
        print *,'   NOTE: pixsize is applied to the first map;'
        print *,'         for the next maps is redefined '
     &         ,          'according to the cell size '
        print *,'         For ppt ximge=1040 yimage=720'
        print *,'         For ppt ximge=1040 yimage=720'
        print *,'-mapshift dx,dy'
        print *,'   map shift in graphic units'
        print *,'   DEFAULT is ',dxmap,dymap
        print *,'-title title'
        print *,'   no DEFAULT, NOT requested '
        print *,'-titsize title size'
        print *,'   DEFAULT is ',titsize
        print *,'-legsize legend size'
        print *,'   DEFAULT is ',legsize
        print *,'   NOTE: the dimension of the color legend is '
     &         ,'proportional '
        print *,'         to this value'
        print *,'-legtextsize legend text size'
        print *,'   DEFAULT is legsize'
        print *,'   NOTE: this is the dimension of the input map name: '
        print *,'         if zero the map name is not written'
        print *,'-bullet yes/no'
        print *,'   make a bullet (yes) or a square (no) in case of '
     &         ,' nominal or boolean map'
        print *,'   DEFAULT is ',yesno(bullet)
        print *,'-Sbullet yes/no'
        print *,'   make a bullet (yes) or a square (no) in case of '
     &         ,' scalar map'
        print *,'   DEFAULT is ',yesno(Sbullet)
        print *,'-bulletcircle yes/no'
        print *,'   make (yes) or not make (no) a circle around '
     &         ,'the bullet'
        print *,'   DEFAULT is ',yesno(bulletcircle)
        print *,'-bulletsize bullet size for boolean or nominal maps'
        print *,'   DEFAULT is ',bulletsize
        print *,'-rgb rgb file name'
        print *,'   Palette for scalar maps, values between vmin vmax'
        print *,'   define your palette by '
        print *,'   text file with r g b values'
        print *,'   NOTE: rgb file replace palette defined '
     &         ,'by "-palette" '
        print *,'   no DEFAULT'
        print *,'-chek yes/no'
        print *,'   check rgb color'
        print *,'   DEFAULT is ',yesno(check)
#if defined WIN32
        read *
#endif
        stop
        endif
c-----------------------------------------------------------------
        do i = 1,narg
        call getarg (i,ch)
c-----------------------------------------------------------------
        if (ch.eq.'-i') then
        ileg=1
          do j = 1,nmaps
          call getarg (i+j,ch)
            if (ch(1:1).eq.'-'.or.ch.eq.' ') exit
          inpmap(j) = ch
          ninpmap = j
          enddo
c-----------------------------------------------
        else if (ch.eq.'-o') then
        call getarg (i+1,outgif)
c-----------------------------------------------
        else if (ch.eq.'-m') then
        call getarg (i+1,maskmap)
c-----------------------------------------------
        else if (ch.eq.'-outps') then
        call getarg (i+1,outps)


c-----------------------------------------------
        else if (ch.eq.'-map') then
        ileg = ileg+1
        ninpmap=ileg
        call getarg (i+1,inpmap(ileg))
c-----------------------------------------------
        else if (ch.eq.'-Slegend') then
        call getarg (i+1,ch)
            if (ch.eq.' ') then
            print *,'Error in legend parameter'
            call exit(1)
            endif
          vmin(ileg)= numget (ch,msg)
c
        call getarg (i+2,ch)
            if (ch.eq.' ') then
            print *,'Error in legend parameter'
            call exit(1)
            endif
          vmax(ileg)= numget (ch,msg)
c
        call getarg (i+3,ch)
            if (ch(1:1).eq.'-'.or.ch.eq.' ') then
            print *,'Error in legend parameter'
            call exit(1)
            endif
          nvalue(ileg)= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-fmt') then
        call getarg (i+1,fmt(ileg))
c-----------------------------------------------
        else if (ch.eq.'-Smin') then
        call getarg (i+1,ch)
          smin(ileg)= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-palette') then
        call getarg (i+1,palette(ileg))
c------------------------------------------------------------------
        else if (ch.eq.'-nstepcol') then
        call getarg (i+1,ch)
          nstepcol(ileg)= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-palout') then
        call getarg (i+1,palout(ileg))
c-----------------------------------------------
        else if (ch.eq.'-mult') then
        call getarg (i+1,ch)
          mult(ileg)= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-leglog') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        leglog(ileg) = .true.
        else if (ch.eq.'no') then
        leglog(ileg) = .false.
        else
        leglog(ileg) = .true.
        endif

c-----------------------------------------------------------------
        else if (ch.eq.'-Nlegend') then
            do m = 0,nfields_max
            nomlegend(m) = ' '
            enddo
          m = i+1
          j = 0
        do while (m.le.narg-1)
            call getarg (m,ch)
            m = m+1
        if (ch.eq.' '.or.ch(1:1).eq.'-') goto 100
        numval = numget(ch,msg)
              if (msg.ne.' ') then
              print *,'msg=',msg
              exit
              endif
            j = j+1
        numlegend(j) = numval
            call getarg (m,ch)
            m = m+1
        if (ch.eq.' ') then
              print *,'Error in Nlegend parameters'
              call exit(1)
              endif
        nomlegend(numval) = ch
*           print *,'j=',j,' numlegend=',numval,' nom=',nomlegend(numval)
        enddo

        
100     nnomleg = j
*         print *,'nnomleg=',nnomleg,'i=',i,' narg=',narg,' m=',m
c-----------------------------------------------------------------
        else if (ch.eq.'-Ncolor') then
          m = i
        do j = 1,nfields_max
            m = m+1
            call getarg (m,ch)
        if (ch.eq.' '.or.ch(1:1).eq.'-') goto 200

        numval = numget(ch,msg)
            m = m+1
            call getarg (m,ch)
        if (ch.eq.' '.or.ch(1:1).eq.'-') then
              print *,'Error in Ncolor parameters'
              call exit(1)
              endif
        colleg(numval) = numget(ch,msg)
        enddo
200     continue
c-----------------------------------------------------------------
        else if (ch.eq.'-NoLegend') then
          m = i
        do j = 1,nfields_max
            m = m+1
            call getarg (m,ch)
        if (ch.eq.' '.or.ch(1:1).eq.'-') goto 300
        numval = numget(ch,msg)
            legend1(numval) = .false.
        enddo
300     continue
          if (m.eq.i+1) legend1 = .false.
c------------------------------------------------------------------
        else if (ch.eq.'-grid') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        grid = .true.
        else if (ch.eq.'no') then
        grid = .false.
        else
        grid = .true.
        endif
c------------------------------------------------------------------
        else if (ch.eq.'-resize') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        resize = .true.
        else if (ch.eq.'no') then
        resize = .false.
        else
        resize = .true.
        endif
c------------------------------------------------------------------
        else if (ch.eq.'-bullet') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        bullet = .true.
        else if (ch.eq.'no') then
        bullet = .false.
        else
        bullet = .true.
        endif
c------------------------------------------------------------------
        else if (ch.eq.'-Sbullet') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        Sbullet = .true.
        else if (ch.eq.'no') then
        Sbullet = .false.
        else
        Sbullet = .true.
        endif
c------------------------------------------------------------------
        else if (ch.eq.'-bulletcircle') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        bulletcircle = .true.
        else if (ch.eq.'no') then
        bulletcircle = .false.
        else
        bulletcircle = .true.
        endif
c-----------------------------------------------
        else if (ch.eq.'-mapshift') then
        call getarg (i+1,ch)
          dxmap= numget (ch,msg)
            if (msg.ne.' ') then
            print *,'Error in mapshift parameter'
            call exit(1)
            endif
 
        call getarg (i+2,ch)
          dymap= numget (ch,msg)
            if (msg.ne.' ') then
            print *,'Error in mapshift parameter'
            call exit(1)
            endif
c------------------------------------------------------------------
        else if (ch.eq.'-pixsize') then
        call getarg (i+1,ch)
          pixsize= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-scale') then
        call getarg (i+1,ch)
          scale= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-titsize') then
        call getarg (i+1,ch)
          titsize= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-bulletsize') then
        call getarg (i+1,ch)
          bulletsize= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-legsize') then
        call getarg (i+1,ch)
          legsize= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-legtextsize') then
        call getarg (i+1,ch)
          legtextsize= numget (ch,msg)
c------------------------------------------------------------------
        else if (ch.eq.'-check') then
        call getarg (i+1,ch)
        if (ch.eq.'yes') then
        check = .true.
        else if (ch.eq.'no') then
        check = .false.
        else
        check = .true.
        endif
c------------------------------------------------------------------
        else if (ch.eq.'-title') then
        do j = 1,132
        call getarg (i+j,ch)
        if (ch(1:1).eq.'-'.or.ch.eq.' ') goto 400
        title = title(1:length(title)+1)//ch
        enddo
400     continue
c------------------------------------------------------------------
        endif
        enddo
c------------------------------------------------------------------
        do i = 1,narg
        call getarg (i,ch)
c-----------------------------------------------------------------
        if (ch.eq.'-doLegend') then
          dolegend = .false.
        do j = 1,ninpmap
            call getarg (i+j,ch)
        if (ch(1:1).eq.'-'.or.ch.eq.' ') then
              exit
              endif
        if (ch.eq.'yes') then
        doLegend(j) = .true.
        else if (ch.eq.'no') then
        doLegend(j) = .false.
        else
        doLegend(j) = .true.
        endif
        enddo
c-----------------------------------------------------------------
        else if (ch.eq.'-doBorder') then
        doBorder = .false.
           do j = 1,ninpmap
           call getarg (i+j,ch)
             if (ch(1:1).eq.'-'.or.ch.eq.' ') then
             exit
             endif
             if (ch.eq.'yes') then
             doBorder(j) = .true.
             else if (ch.eq.'no') then
             doBorder(j) = .false.
             else
             doBorder(j) = .true.
             endif
           enddo
c------------------------------------------------------------------
        else if (ch.eq.'-rgb') then
          do j = 1,ninpmap
          call getarg (i+j,ch)
            if (ch(1:1).eq.'-'.or.ch.eq.' ') then
            exit
            endif
          rgbfile(j)=ch
          enddo
c------------------------------------------------------------------
          endif
        enddo
c------------------------------------------------------------------
        if (legtextsize.lt.0.) legtextsize = legsize
c------------------------------------------------------------------
        if (outps.eq.' ') then
#ifdef WIN32
        write (outps,'(a,i8,a)') 'tmp',getpid(),'.eps'
#else
        write (outps,'(a,i8,a)') '/tmp/map2image',getpid(),'.eps'
#endif
        call fillzero (outps,length(outps))
        rmps = .true.
        else
        rmps = .false.
        endif
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        call set_rgb
c------------------------------------------------------------------
        if (maskmap.ne.' ') then
        lm = length(maskmap)
        inquire (file=maskmap,exist=exist)
          if (.not.exist) then
          print *,'mask map ',maskmap(1:lm),' not exist'
          call exit(1)
          endif
        map = maskmap(1:lm)//null
*        call OPENMAPTYPE2 (map,ncol,nrow,typ,nbyte,xor0,yor0,cellsize,error)
        call OPENGDAL (map,ncol,nrow,typ,xor0,yor0,cellsize,sizey,driver,nband,error)
        iband = nband
        call GETGDALATTR (iband,nbyte,datatype)
        if (typ.eq.0) then
          if (datatype.eq.'Byte') then
          typ=6 ! ldd
          else if (datatype.eq.'Int32') then
          typ=2 ! nominal
          else if (datatype.eq.'Float32') then
          typ=4 ! scalar
          endif
        else if (typ.eq.1.or.typ.eq.6) then
        nbyte = 1
        else if (typ.gt.1.and.typ.lt.6) then
        nbyte = 4
        endif

        npcr = ncol*nrow

        allocate (mask(npcr),r4(npcr))
        mask = 1
        call getr4(r4,ncol,nrow,npcr,typ,nbyte)
          do l = 1,npcr
            if (eqmv(r4(l))) mask(l) = 0
          enddo
        existmask = .true.
        deallocate (r4)

        else
        lm = length(inpmap(1))
        map = inpmap(1)(1:lm)//null
*        call OPENMAPTYPE2 (map,ncol,nrow,typ,nbyte,xor0,yor0,cellsize,error)
        call OPENGDAL (map,ncol,nrow,typ,xor0,yor0,cellsize,sizey,driver,nband,error)
*        call closemap()
        call closegdal()

        npcr = ncol*nrow
        allocate (mask(npcr))
        mask = 1
        existmask = .false.
        endif
c----------------------------------------------
        if (pixsize.le.0.) then
        pixsize = min((ximage-dxmap)/ncol,(yimage-dymap)/nrow)
        print *,'ximage=',ximage,' yimage=',yimage,' ncol=',ncol,' nrow=',nrow
        endif
c----------------------------------------------
#ifdef DOIT
        titsize = pixsize*titsize
        legsize = pixsize*legsize
        legtextsize = pixsize*legtextsize
        bulletsize = pixsize*bulletsize
        dxmap = pixsize*dxmap
        dymap = pixsize*dymap
#endif
c----------------------------------------------
        xor1 = xor0
        yor1 = yor0
        cellsize1 = cellsize
        pixsize1 = pixsize
        xor = xor0*pixsize/cellsize
        yor = yor0*pixsize/cellsize
c       ---------------------------------------
        dc = pixsize1*0.25
            do i = 1,narg
        call getarg (i,ch)
        if (ch.eq.'-lddsize') then
        call getarg (i+1,ch)
            dc= numget (ch,msg)
            endif
          enddo
c       ---------------------------------------
        l = 0
        xmin = 1e30
        ymin = 1e30
        xmax = -1e30
        ymax = -1e30
          do j = 1,nrow
            do i = 1,ncol
            l = l+1
              if (mask(l).eq.1) then
              x = xor + pixsize*(i-0.5)
              y = yor - pixsize*(j-0.5)
              xmin = min(x,xmin)
              ymin = min(y,ymin)
              xmax = max(x,xmax)
              ymax = max(y,ymax)
              endif
            enddo
          enddo
        xmin = xmin-pixsize/2
        ymin = ymin-pixsize/2
        xmax = xmax+pixsize/2
        ymax = ymax+pixsize/2
c       ---------------------------------------
        xrange = xmax-xmin
        yrange = ymax-ymin
c       ---------------------------------------
        xleg = legsize/2
        yleg0 = dymap + yrange
          if (title.eq.' ') then
          dytit = 0
          else
          dytit = 1.5*titsize
          endif
        ximage = dxmap+xrange
        yimage = dymap+yrange
          if (count(dolegend).gt.0) yimage=yimage+dytit
 
        call psstart (10, outps, 0.,0.,ximage,yimage,.true.,'map2image',scale)
          if (title.ne.' ') then
          call pschgfont (1,titsize,0.,0.)
*         call psstring (xleg+dxmap,yimage-titsize,title,length(title),black)
          call psstring (xleg,yimage-titsize*1.25,title,length(title),black)
          endif
        print *
        print *,'xmin=',xmin,' xmax=',xmax
        print *,'ymin=',ymin,' ymax=',ymax
        print *,'ximage=',ximage,' yimage=',yimage,' graphic units'
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
*         transparency=" -transparent white -compress Zip "
         transparency=" -transparent 'rgb(250,250,250)' -compress Zip "
         call psfillrect (0.,0.,ximage,yimage,transparent)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c loop over all maps
c------------------------------------------------------------------
        if (ileg.lt.ninpmap) then
        palette(2:ninpmap)=palette(1)
        palout(2:ninpmap)=palout(1)
        nstepcol(2:ninpmap) = nstepcol(1)
        fmt(2:ninpmap) = fmt(1)
        leglog(2:ninpmap) = leglog(1)
        nvalue(2:ninpmap) = nvalue(1)
        vmin(2:ninpmap) = vmin(1)
        vmax(2:ninpmap) = vmax(1)
        smin(2:ninpmap) = smin(1)
        mult(2:ninpmap) = mult(1)
        endif
c------------------------------------------------------------------
        do m = 1,ninpmap
        ileg = m
c------------------------------------------------------------------
        do i = 132,1,-1
          if (inpmap(m)(i:i).eq.bslash) goto 500
        enddo
500     continue
        legtext=inpmap(m)(i+1:)
c------------------------------------------------------------------
        if (inpmap(m).eq.' ') then
        print *,'Input map not defined'
        call exit(1)
        endif
 
        lm = length(inpmap(m))
        map = inpmap(m)(1:lm)//null
*        call OPENMAPTYPE2 (map,ncol,nrow,typ,nbyte,xor0,yor0,cellsize,error)
        call OPENGDAL (map,ncol,nrow,typ,xor0,yor0,cellsize,sizey,driver,nband,error)
        iband = nband
        call GETGDALATTR (iband,nbyte,datatype)
        if (typ.eq.0) then
          if (datatype.eq.'Byte') then
          typ=6 ! ldd
          else if (datatype.eq.'Int32') then
          typ=2 ! nominal
          else if (datatype.eq.'Float32') then
          typ=4 ! scalar
          endif
        else if (typ.eq.1.or.typ.eq.6) then
        nbyte = 1
        else if (typ.gt.1.and.typ.lt.6) then
        nbyte = 4
        endif


        npcr = ncol*nrow
c-----------------------------------------------
        pixsize = pixsize1*cellsize/cellsize1
        xor = xor0*pixsize/cellsize
        yor = yor0*pixsize/cellsize
c-----------------------------------------------
/*
          do j = 1,nrow
            do i = 1,ncol
            x = xor + pixsize*(i-1)
            y = yor - pixsize*j
            call psfillrect (x,y,x+pixsize,y+pixsize,transparent)
            enddo
          enddo
*/
c-----------------------------------------------
        print *
        print *,'------------------------------------------------'
        print *,'map ',map(1:lm),' datatype=',datatype(1:length(datatype)),' typ=',typ,' nbyte=',nbyte
        print *,'pixsize1=', pixsize1,' cellsize1=',cellsize1
        print *,'pixsize=',pixsize,' cellsize=',cellsize
        print *,'xor=',xor,' yor=',yor
        print *,'ncol=',ncol,' nrow=',nrow,' npcr=',npcr
c------------------------------------------------------------------
        allocate (r4(npcr))
        call getr4(r4,ncol,nrow,npcr,typ,nbyte)
        print *,' done getr4'
c------------------------------------------------------------------
        if (typ.eq.1) then
        call boolean2ps (r4,ncol,nrow,doLegend(m))
 
        else if (typ.eq.6) then
        call ldd2ps (r4,ncol,nrow,doLegend(m))
 
        else if (typ.eq.4.or.typ.eq.5) then
          if (rgbfile(ileg).eq.'none') then
          call scalar2ps (r4,ncol,nrow,doLegend(m))
          else
          call scalar2ps1 (r4,ncol,nrow)
          endif 
        else
        call nominal2ps (r4,ncol,nrow,doLegend(m))
        endif
        if (doBorder(m)) then
        print *,'do border'
*       call pschgline (pixsize1,0)
*        call pschgline (max(pixsize,pixsize1/4),0)
        call pschgline (1.,0)

        x1 = xor; y1=yor; x2=xor+ncol*pixsize; y2=yor
        call psline (x1+dxmap-xmin,y1+dymap-ymin,x2+dxmap-xmin,y2+dymap-ymin,black)

        x1 = x2; y1=y2; y2=yor-pixsize*nrow
        call psline (x1+dxmap-xmin,y1+dymap-ymin,x2+dxmap-xmin,y2+dymap-ymin,black)

        x1 = x2; y1=y2; x2=xor
        call psline (x1+dxmap-xmin,y1+dymap-ymin,x2+dxmap-xmin,y2+dymap-ymin,black)

        x1 = x2; y1=y2; x2=xor; y2=yor
        call psline (x1+dxmap-xmin,y1+dymap-ymin,x2+dxmap-xmin,y2+dymap-ymin,black)

        endif

        deallocate (r4)
        enddo
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        call psend (.true.)
      print *,'----------------------------------------------------'
        if (outgif.ne.' ') then
          if (resize) then
          geometry=' '
          ncol=min(ximage,1000.); nrow=min(yimage,800.)
          write (geometry,'(i6,a1,i6)') ncol,'x',nrow
          j = 0
            do i = 1,13
              if (geometry(i:i).ne.' ') then
              j=j+1
              geometry(j:j)=geometry(i:i)
              endif
            enddo
          geometry=' -geometry '//geometry(:j) 
          else
          geometry=' '
          endif
        command='convert '//transparency(1:length(transparency)+1)
     &                         //geometry(1:length(geometry)+1)
     &                         //outps(1:length(outps)+1)
     &                         //outgif(1:length(outgif)+1)
        write(*,'(a)') command(1:length(command))
        call system(command(1:length(command)))
          if (rmps) call system ('rm '//outps)
        print *,'outgif=',outgif(1:length(outgif))
        endif
      print *,'----------------------------------------------------'
      print *
      print *,'time spent [min] = ', (time()-it0)/60.
 
        stop
        end

c-----------------------------------------------
        subroutine getr4 (r4,ncol,nrow,npcr,typ,nbyte)
c-----------------------------------------------
        use interface_mod
        implicit none
        integer*4 ncol,nrow,npcr
        real*4 r4(npcr)
        logical*4 eqmv
 
        integer*4, allocatable :: int4(:)
        integer*2, allocatable :: int2(:)
        integer*1, allocatable :: int1(:)
        integer*4  typ, ninpmap,i,nmv, nbyte, iband /1/
c       ------------------------------------------------
        allocate (int4(npcr),int2(npcr),int1(npcr))

        nmv = 0
 
c boolean and ldd
        if (typ.eq.1.or.typ.eq.6) then
*        call GETVALMAP (int1)
        call GETGDALVAL1 (iband,int1)
        r4 = int1
        do i = 1,npcr
        if (int1(i).eq.mv1) then
            r4(i) = mv
            nmv = nmv +1
            endif
        enddo
 
c nominal, ordinal
        else if (typ.eq.2.or.typ.eq.3) then
          if (nbyte.eq.1) then
*          call GETVALMAP (int1)
          call GETGDALVAL1 (iband,int1)
            do i = 1,npcr
              if (int1(i).eq.mv1) then
              r4(i) = mv
              nmv = nmv +1
              else
              r4(i) = int1(i)
              endif
            enddo
 
          else if (nbyte.eq.4) then
*          call GETVALMAP (int4)
          call GETGDALVAL4 (iband,int4)
            do i = 1,npcr
              if (int4(i).eq.mv4) then
              r4(i) = mv
              nmv = nmv +1
              else
              r4(i) = int4(i)
              endif
            enddo

          else
          print *,'typ=',typ,' nbyte=',nbyte
          stop "program error"
          endif
 

c scalar, direction
 
        else if (typ.eq.4.or.typ.eq.5) then
*        call GETVALMAP (r4)
        call GETGDALVAL (iband,r4)
          do i = 1,npcr
            if (.not.eqmv(r4(i))) r4(i)=r4(i)*mult(ileg)
          enddo
        
        else
        print *,'type ',typ,' not implemented'
        call CLOSEGDAL()
        endif	

        deallocate (int4,int2,int1)
        return
        end
c------------------------------------------------------
        subroutine boolean2ps (val,ncol,nrow,dolegend)
        use interface_mod
        implicit none
        integer*4 ncol,nrow
        real*4 val(ncol,nrow)
        logical*1, allocatable :: mask(:,:)
        logical*4 dolegend, eqmv
c
        real*4 x,y
        integer*4 i, j, l, m, icol
c--------------------------------------------------
        allocate (mask(ncol,nrow))
 
 
        do j = 1,nrow
          do i = 1,ncol
          mask(i,j) = .not.eqmv(val(i,j))
          enddo
        enddo
c--------------------------------------------------
        print *
        print *,'ncol=',ncol,' nrow=',nrow
        print *,'xrange=',xrange,' yrange=',yrange,' legsize=',legsize
        print *,'titsize=',titsize,' pixsize=',pixsize
c--------------------------------------------------
        call pschgline (min(0.5,pixsize/4.),0)
c--------------------------------------------------
        do j = 1,nrow
          do i = 1,ncol
            if (mask(i,j)) then
            icol = firstbool + mod(int(val(i,j)),2)
            call fillcolor(val(i,j),i,j,icol,bullet,.false.)
            endif
          enddo
        enddo

        call psflush
 
        if (dolegend) call boolean_legend

        call psflush
 
        deallocate (mask)
 
        return
        end
c------------------------------------------------------
        subroutine nominal2ps (val,ncol,nrow,dolegend)
        use interface_mod
        implicit none
        integer*4 ncol,nrow
        real*4 val(ncol,nrow)
        logical*1, allocatable :: mask(:,:)
        logical*4 dolegend, eqmv
c
        real*4 x,y
        integer*4 i, j, icol, length, numval
        character text*20
c--------------------------------------------------
        allocate (mask(ncol,nrow))
 
 
        do j = 1,nrow
          do i = 1,ncol
          mask(i,j) = .not.eqmv(val(i,j))
          enddo
        enddo
c--------------------------------------------------
        print *
        print *,'ncol=',ncol,' nrow=',nrow
        print *,'xrange=',xrange,' yrange=',yrange,' legsize=',legsize
        print *,'titsize=',titsize,' pixsize=',pixsize
c--------------------------------------------------
        call pschgline (0.5,0)
        call pschgfont (1,legsize,0.,0.)
c--------------------------------------------------
        do j = 1,nrow
          do i = 1,ncol
          numval=val(i,j)
            if (mask(i,j)) then
            if (nomlegend(numval).ne.' ') then
            icol = firstnom + colleg(numval)
            call fillcolor(val(i,j),i,j,icol,bullet,.false.)
            legend(numval) = .not.outwin
              if (.not.legend1(numval).and..not.outwin) then
              legend(numval) = .false.
              text = nomlegend(numval)
              print *,'numval=',numval,' color=',colleg(numval),' text=',text
              x = xval-legsize*length(text)/3
*               if (mod(numval,2).eq.1) then
                y = yval+bulletsize*2
*               else
*               y = yval-bulletsize*2-legsize
*               endif
              call psstring (x,y,text,length(text),icol)
              endif
            endif
            endif
          enddo
        enddo

        call psflush
 
        if (nnomleg.gt.0.and.dolegend.and.count(legend).gt.0) call nominal_legend

        call psflush
 
        deallocate (mask)
 
        return
        end
c------------------------------------------------------
        subroutine ldd2ps (val,ncol,nrow,dolegend)
        use interface_mod
        implicit none
        integer*4 ncol,nrow
        real*4 val(ncol,nrow)
        logical*1, allocatable :: mask(:,:)
        logical*4 dolegend, eqmv
c
        real*4 x1,y1,x2,y2
        integer*4 i, j, m, i2,j2
        integer*4 imap(3,9)
        data imap /
     &  1, -1, 1,
     &  2,  0, 1,
     &  3,  1, 1,
     &  4, -1, 0,
     &  5,  0, 0,
     &  6,  1, 0,
     &  7, -1,-1,
     &  8,  0,-1,
     &  9,  1,-1/
c--------------------------------------------------
        allocate (mask(ncol,nrow))
 
 
        do j = 1,nrow
          do i = 1,ncol
          mask(i,j) = .not.eqmv(val(i,j))
          enddo
        enddo
c--------------------------------------------------
        print *
        print *,'ncol=',ncol,' nrow=',nrow,' mask values=',count(mask)
        print *,'xrange=',xrange,' yrange=',yrange
        print *,'dc=',dc
c--------------------------------------------------
        call forcepscolor (black)
 
        do j = 1,nrow
          do i = 1,ncol
          x1 = xor + pixsize*(i-0.5)
          y1 = yor - pixsize*(j-0.5)
*          print *,'val=',val(i,j),' mask=',mask(i,j)
            if (mask(i,j).and.x1.gt.xmin.and.x1.lt.xmax
     &                   .and.y1.gt.ymin.and.y1.lt.ymax) then
            x1 = x1 + dxmap - xmin
            y1 = y1 + dymap - ymin
              if (val(i,j).eq.5) then
              call psfillrect (x1-dc,y1-dc,x1+dc,y1+dc,black)
              else
              i2 = i+imap(2,int(val(i,j)))
              j2 = j+imap(3,int(val(i,j)))
              x2 = dxmap - xmin + xor + pixsize*(i2-0.5)
              y2 = dymap - ymin + yor - pixsize*(j2-0.5)
              call pschgline (max(0.8,dc/2),0)
              call psline (x1,y1,x2,y2,black)
              endif
            endif
          enddo
        call psflush
        enddo

        call psflush
 
        deallocate (mask)
        print *,'done ldd2ps'
        return
        end
c------------------------------------------------------
        subroutine scalar2ps (val,ncol,nrow,dolegend)
        use interface_mod
        implicit none
        integer*4 ncol,nrow
        real*4 val(ncol,nrow)
        logical*1, allocatable :: mask(:,:)
        logical*4 dolegend, testval, eqmv
c
        real*4 ratio
        integer*4 i, j, m, colval, icol
c--------------------------------------------------
        allocate (mask(ncol,nrow))
 
        do j = 1,nrow
          do i = 1,ncol
          mask(i,j) = .not.eqmv(val(i,j))
          enddo
        enddo
c--------------------------------------------------
c vmin and vmax in case of undefined values
        if (vmin(ileg).eq.0..and.vmax(ileg).eq.0.) then
        vmin (ileg)= 1e10
        vmax(ileg) = -1e10
        do j = 1,nrow
          do i = 1,ncol
            if (mask(i,j)) then
            vmin(ileg) = min(vmin(ileg),val(i,j))
            vmax(ileg) = max(vmax(ileg),val(i,j))
            endif
          enddo
        enddo
          if (leglog(ileg)) then
          ratio=(vmax(ileg)/vmin(ileg))**(1./nvalue(ileg))
          vmin(ileg) = vmin(ileg)*ratio
          vmax(ileg) = vmax(ileg)/ratio
          else
          ratio=(vmax(ileg)-vmin(ileg))/nvalue(ileg)
          vmin(ileg) = vmin(ileg)+ratio
          vmax(ileg) = vmax(ileg)-ratio
          endif
        endif
c------------------------------------------------------------------
        if (leglog(ileg)) then
          if (vmin(ileg).le.0.) then
          print *,'For log scale vmin must be > 0, not ',vmin
          call exit(1)
          endif
          if (vmax(ileg).le.0.) then
          print *,'For log scale vmax must be > 0, not ',vmax
          call exit(1)
          endif
        endif
c--------------------------------------------------
        print *
        print *,'vmin=',vmin(ileg),' vmax=',vmax(ileg)
        print *,'ncol=',ncol,' nrow=',nrow
        print *,'xrange=',xrange,' yrange=',yrange,' legsize=',legsize
        print *,'titsize=',titsize,' pixsize=',pixsize
c--------------------------------------------------
        call set_rgb_scalar
c--------------------------------------------------
c define vstep  (see colval)
          if (leglog(ileg)) then
            vstep = ncolor/log(vmax(ileg)/vmin(ileg))
          else
            vstep = ncolor/(vmax(ileg)-vmin(ileg))
          endif
c--------------------------------------------------
        call pschgline (min(0.5,bulletsize/4.),0)
c--------------------------------------------------
c make color map
        do j = 1,nrow
          do i = 1,ncol
            if (leglog(ileg)) then
            testval = mask(i,j).and.val(i,j).gt.smin(ileg)
            else
            testval = mask(i,j).and.abs(val(i,j)).gt.smin(ileg)
            endif
            if (testval) then
            icol = colval(val(i,j))
            call fillcolor (val(i,j),i,j,icol,Sbullet,grid)
            endif
          enddo

        call psflush
        enddo

        call psflush
 
       if (dolegend) call scalar_legend

        call psflush
 
        deallocate (mask)
 
        return
        end
c------------------------------------------------------
        subroutine scalar2ps1 (val,ncol,nrow)
        use interface_mod
        implicit none
        integer*4 ncol,nrow, length
        real*4 val(ncol,nrow)
        logical*1, allocatable :: mask(:,:)
        logical*4 dolegend, testval, exist, eqmv
c
        real*4 ratio
        integer*4 i, j, m, colval1, icol
c--------------------------------------------------
        allocate (mask(ncol,nrow))
 
        do j = 1,nrow
          do i = 1,ncol
          mask(i,j) = .not.eqmv(val(i,j))
          enddo
        enddo
c--------------------------------------------------
        call pschgline (min(0.5,bulletsize/4.),0)
c------------------------------------
c read rgb file if specified
        inquire (file=rgbfile(ileg),exist=exist)
          if (exist) then
          open (11,file=rgbfile(ileg))
          read (11,*)
          i = 0
            do while (.true.)
            i = i + 1
            read (11,*,end=100) v1(i),v2(i),rs(i),gs(i),bs(i)
            call pssetcol (i,rs(i),gs(i),bs(i))
            enddo
100       nrgb = i-1
          close (11)
          print *,'found ',nrgb,' colors' 
          else
          print *,'rgb file ',rgbfile(ileg)(1:length(rgbfile(ileg))),' not exist'
          call exit
          endif
c--------------------------------------------------
c make color map
        do j = 1,nrow
          do i = 1,ncol
          testval = mask(i,j).and.abs(val(i,j)).gt.smin(ileg)
            if (testval) then
            icol = colval1(val(i,j))
            call fillcolor (val(i,j),i,j,icol,Sbullet,grid)
            endif
          enddo

        call psflush
        enddo

        call psflush
 
        deallocate (mask)
 
        return
        end
c------------------------------------------------------
        function  colval1 (val)
        use interface_mod
        implicit none
        real*4 val
c i: column index
c j: row index
        integer*4 colval1, i, iold
        data iold /1/ 
        save :: iold
 
c define color
       colval1 =  iold
        if (val.gt.v2(iold)) then
          do i=iold+1,nrgb
            if (v2(i).gt.val) then
            iold=i
            colval1=iold
            return
            endif
          enddo
        else if (val.lt.v1(iold)) then
          do i=iold-1,1,-1
            if (v1(i).lt.val) then
            iold=i
            colval1=iold
            return
            endif
          enddo
        else
        colval1=iold
        endif 

        end
c------------------------------------------------------
        subroutine scalar_legend
        use interface_mod
        implicit none
 
        real*4 yleg,xpan,ypan, val,dval, yleg1, bigpan,rgbpan, xrangeleg
     &        ,sizeout
        character text*30,format*132
        integer*4 i,j,icol, length
        logical*4 first /.true./
c--------------------------------------------------
        call pscomment ('Start scalar legend')
c--------------------------------------------------
        xpan = pansize*2
        sizeout = legsize/nstepcol(ileg)
        bigpan = pansize*min(10,nvalue(ileg))+2*sizeout
        rgbpan = (bigpan-2*sizeout)/nrgb
        ypan = rgbpan/nstepcol(ileg)
 
        yleg1 = yleg0
        yleg0 = yleg0 - bigpan -legsize*2
        yleg = yleg0
 
        xrangeleg = xpan+legsize*5
        print *
        print *,'range legend=',xrangeleg
        call psfillrect (xleg,yleg-legsize/2,xleg+xrangeleg,yleg1,white)
 
        call pschgline (1.,0)
        call psrect (xleg,yleg,xleg+xpan,yleg+bigpan,black)
        call psflush
 
c legend color
 
            icol = firstscal
            call psfillrect (xleg,yleg
     &            ,xleg+xpan,yleg+sizeout,icol)
            yleg = yleg+sizeout
 
          do i = 1,nrgb
 
            do j = 1,nstepcol(ileg)
            icol = (i-1)*nstepcol(ileg) + j
            call psfillrect (xleg,yleg+(j-1)*ypan
     &            ,xleg+xpan,yleg+j*ypan,icol)
            enddo
 
           yleg = yleg + rgbpan
 
          enddo
 
            icol = maxcol
            call psfillrect (xleg,yleg
     &            ,xleg+xpan,yleg+sizeout,icol)
            yleg = yleg+sizeout
 
c legend text
 
        if (legtextsize.gt.0) then
        call pschgfont (1,legtextsize,0.,0.)
        call psstring (xleg,yleg+legsize,legtext,length(legtext),black)
        endif
 
        call pschgfont (1,legsize,0.,0.)

c legend numbers
 
          yleg = yleg0+sizeout
 
          if (leglog(ileg)) then
          dval = exp(log(vmax(ileg)/vmin(ileg))/(nvalue(ileg)-1))
          else
          dval = (vmax(ileg)-vmin(ileg))/(nvalue(ileg)-1)
          endif
 
          val = vmin(ileg)
 
          do i = 1, nvalue(ileg)
 
          format = '(' //fmt(ileg)// ')'
          write (text,format) val
 
          call psstring (xleg+xpan+legsize,yleg-legsize/3.
     &                  ,text,length(text),black)
 
          yleg = yleg + (rgbpan*nrgb)/(nvalue(ileg)-1)
 
          if (leglog(ileg)) then
          val = val*dval
          else
          val = val+dval
          endif
 
          enddo

        call psflush
 
 
        yleg0 = yleg0 - legsize
 
        call pscomment ('End scalar legend')
        return
        end
c------------------------------------------------------
        subroutine nominal_legend
        use interface_mod
        implicit none
 
        real*4 yleg,xpan,yleg1, bigpan, xrangeleg
        character text*30
        integer*4 i,j,icol, length, numval
        logical*4 first /.true./
c------------------------------------
        if (first) then
        first = .false.
 
        else
        return
        endif
c--------------------------------------------------
        call pscomment ('Start nominal_legend')
c--------------------------------------------------
        xpan = pansize*2
        bigpan = pansize*count(legend.and.legend1)
 
        yleg1 = yleg0
        yleg0 = yleg0 - bigpan -legsize*2
        yleg = yleg0
 
 
        xrangeleg = xpan+legsize*5
        print *
        print *,'range legend=',xrangeleg
        call psfillrect (xleg,yleg-legsize/2,xleg+xrangeleg,yleg1,white)
        call pschgline (1.,0)
        call psrect (xleg,yleg,xleg+xpan,yleg+bigpan,black)
        call psflush
 
        call pschgfont (1,legsize,0.,0.)
 
c legend color & number

          do i = 1,nnomleg
          j = nnomleg - i  + 1
          numval = numlegend(j)
          icol = firstnom + colleg(numval)
*         print *,'j=',j,' num=',numval,'nom=',nomlegend(numval)
*    &        ,' icol=',colleg(numval), ' legend=',legend(numval),' legend1=',legend1(numval)
          enddo
 
          do i = 1,nnomleg
          j = nnomleg - i  + 1
          numval = numlegend(j)
            if (legend(numval).and.legend1(numval)) then
            icol = firstnom + colleg(numval)
            call psfillrect (xleg,yleg,xleg+xpan,yleg+pansize,icol)
            text = nomlegend(numval)
            call psstring (xleg+xpan+legsize,yleg
     &                    ,text,length(text),black)
            yleg = yleg + pansize
            endif
          enddo
 
c legend text
 
        if (legtextsize.gt.0) then
        call pschgfont (1,legtextsize,0.,0.)
        call psstring (xleg,yleg+legsize,legtext,length(legtext),black)
        endif

        call psflush
 
        call pscomment ('End nominal_legend')
        yleg0 = yleg0 - legsize
 
        return
        end
c------------------------------------------------------
        subroutine boolean_legend
        use interface_mod
        implicit none
 
        real*4 yleg,xpan,yleg1, bigpan, xrangeleg
        character text(2)*5 /'False','True'/
        integer*4 i,j,icol, length
        logical*4 first /.true./
c------------------------------------
        if (first) then
        first = .false.
 
        else
        return
        endif
c--------------------------------------------------
        call pscomment ('Start boolean_legend')
c--------------------------------------------------
        xpan = pansize*2
        bigpan = pansize*2
 
        yleg1 = yleg0
        yleg0 = yleg0 - bigpan -legsize*2
        yleg = yleg0
 
 
        xrangeleg = xpan+legsize*5
        print *
        print *,'range legend=',xrangeleg
        call psfillrect (xleg,yleg-legsize/2,xleg+xrangeleg,yleg1,white)

        call pschgline (1.,0)
        call psrect (xleg,yleg,xleg+xpan,yleg+bigpan,black)
        call psflush
 
        call pschgfont (1,legsize,0.,0.)
 
c legend color & number
 
          do i = 1,2
          call psfillrect (xleg,yleg
     &            ,xleg+xpan,yleg+pansize,i+firstbool-1)
          call psstring (xleg+xpan+legsize,yleg
     &                  ,text(i),5,black)
           yleg = yleg + pansize
          enddo
 
c legend text
 
        if (legtextsize.gt.0) then
        call pschgfont (1,legtextsize,0.,0.)
        call psstring (xleg,yleg+legsize,legtext,length(legtext),black)
        endif

        call psflush
 
        call pscomment ('End boolean_legend')
 
        yleg0 = yleg0 - legsize
 
        return
        end
c------------------------------------------------------
        subroutine set_rgb
        use interface_mod
        implicit none
        integer*4 i,j,m,length
c-------------------------------------
c background (same of transparent)
        call pssetcol (background, 250./255. ,250./255. ,250./255.)
c Black
        call pssetcol (black,0.01,0.01,0.01)
c White
        call pssetcol (white, 0.99 , 0.99 , 0.99 )
c Gray dark
        call pssetcol (gray,0.5,0.5,0.5)
c
c rainbow for scalar legend
        call pssetcol (magenta,1.,0.,1.)
        call pssetcol (blue,   0.,0.,1.)
        call pssetcol (cyan,   0.,1.,1.)
        call pssetcol (green,  0.,1.,0.)
        call pssetcol (yellow, 1.,1.,0.)
        call pssetcol (red,    1.,0.,0.)
*       call pssetcol (orange,    1.,153./255.,0.)
        call pssetcol (orange,    1.,170./255.,0.)
        call pssetcol (orangedark,    185./255. ,85./255.,0.)
*       call pssetcol (reddark, 192./255. , 0., 0.) ! quasi red
        call pssetcol (reddark, 156./255. , 0., 0.) ! quasi red
        call pssetcol (magentadark, 157./255. , 0./255. , 157./255. ) ! magenta dark
        call pssetcol (whitedark, 228./255., 228./255.,228./255.) ! white dark
        call pssetcol (bluedark,   0.,0.,0.75)
        call pssetcol (greendark, 98./255., 150./225., 98./255.)
        call pssetcol (transparent, 250./255. ,250./255. ,250./255.)
 
c colors for nominal maps
 
        i=0; call pssetcol(firstnom+i, 0.95, 0.85, 0.) ! quasi yellow
        i=i+1; call pssetcol(firstnom+i, 0.85, 0., 0.) ! quasi red
        i=i+1; call pssetcol(firstnom+i, 0., 0.75, 0.) ! quasi green
        i=i+1; call pssetcol(firstnom+i, 0., 227./255, 222./255.) ! quasi cyan
        i=i+1; call pssetcol(firstnom+i, 0., 0., 0.8) ! quasi blue
        i=i+1; call pssetcol(firstnom+i, 200./255. , 70./255. , 200./255. ) ! quasi magenta
        i=i+1; call pssetcol(firstnom+i, 98./255., 150./255., 98./255.) ! green dark
        i=i+1; call pssetcol(firstnom+i, 180./255.,107./255., 0.) ! brown
        i=i+1; call pssetcol(firstnom+i, 157./255., 0./255., 157./255.) ! magenta dark
        i=i+1; call pssetcol(firstnom+i, 1., 0.75, 1.) ! magenta ligth

        i=i+1; call pssetcol(firstnom+i, 156./255., 0., 0.) ! reddark
*       i=i+1; call pssetcol(firstnom+i, 1., 153./255., 0.) ! orange
        i=i+1; call pssetcol(firstnom+i, 1., 170./255., 0.) ! orange
        i=i+1; call pssetcol(firstnom+i, 185./255., 85./255., 0.) ! orangedark
        i=i+1; call pssetcol(firstnom+i, 1., 1., 0.) ! yellow
        i=i+1; call pssetcol(firstnom+i, 0.7, 0.7, 0.7) ! 
        i=i+1; call pssetcol(firstnom+i, 0.65, 0.65, 0.65) ! 
        i=i+1; call pssetcol(firstnom+i, 0.6, 0.6, 0.6) ! 
        i=i+1; call pssetcol(firstnom+i, 0.55, 0.55, 0.55) ! 
        i=i+1; call pssetcol(firstnom+i, 0.5, 0.5, 0.5) ! 
        i=i+1; call pssetcol(firstnom+i, 0.45, 0.45, 0.45) ! 
        i=i+1; call pssetcol(firstnom+i, 0.4, 0.4, 0.4) ! 
        i=i+1; call pssetcol(firstnom+i, 0.35, 0.35, 0.35) ! 
        i=i+1; call pssetcol(firstnom+i, 0.3, 0.3, 0.3) ! 
        i=i+1; call pssetcol(firstnom+i, 0.25, 0.25, 0.25) ! 
        i=i+1; call pssetcol(firstnom+i, 0.2, 0.2, 0.2) ! 
        i=i+1; call pssetcol(firstnom+i, 0.15, 0.15, 0.15) ! 
        i=i+1; call pssetcol(firstnom+i, 0.1, 0.1, 0.1) ! 
        i=i+1; call pssetcol(firstnom+i, 0.01, 0.01, 0.01) ! black
#ifdef DOIT
        i=i+1; call pssetcol(firstnom+i, 1., 0., 1.) ! magenta
        i=i+1; call pssetcol(firstnom+i, 0., 0., 1.) ! blue
        i=i+1; call pssetcol(firstnom+i, 0., 1., 1.) ! cyan
        i=i+1; call pssetcol(firstnom+i, 0., 1., 0.) ! green
        i=i+1; call pssetcol(firstnom+i, 1., 0., 0.) ! red
#endif
 
c colors for boolean maps
        call pssetcol(firstbool+0,0.85, 0., 0.) ! quasi red
        call pssetcol(firstbool+1,0., 0.85, 0.) ! quasi green
 
        pansize = legsize
 
        return
        end
c-----------------------------------------------------
        subroutine getcolor (color,fred,fgreen,fblue)
        use interface_mod
        implicit none
        character color*1
        integer*4 icol, j
        real*4 fred,fgreen,fblue
 
c assigne a color index to a color letter
 
          do j=1,npal
            if (color.eq.cpal(j)) then
            call psgetcol (ipal(j),fred,fgreen,fblue)
            go to 100
            endif
          enddo
 
          print *,'color ',color,' not recognized: set to black'
          call psgetcol (black,fred,fgreen,fblue)
 
100     continue
        return
        end
c------------------------------------------------------
        subroutine set_rgb_scalar
        use interface_mod
        implicit none
        real*4 fred, fgreen, fblue, faclim /1./
        integer*4 i,j,m,length, nrgb2, nstep2
        logical*4 exist
        logical*4 first /.true./

c-------------------------------------
c default rgb for scalar map
        print *
        print *,'palette=',palette(ileg)
        nrgb = length(palette(ileg))
          do i = 1,nrgb
          call getcolor (palette(ileg)(i:i),rs(i),gs(i),bs(i))
          enddo
c-------------------------------------
c set rgb out of the legend
        print *
        print *,'palout=',palout(ileg)
        call getcolor (palout(ileg)(1:1),rs(0),gs(0),bs(0))
        call getcolor (palout(ileg)(2:2),rs(nrgb+1),gs(nrgb+1),bs(nrgb+1))
c-------------------------------------
        print *
        print *,'r   g   b'
        do i = 0,nrgb+1
        print '(3f4.1)',rs(i),gs(i),bs(i)
        enddo
c-------------------------------------------------------------------
        call set_rainbow
c---------------------------------------------
        return
        end
c------------------------------------------------------
        subroutine set_rainbow
        use interface_mod
        implicit none
        real*4 cstep, fred, fgreen, fblue
        integer*4 i,j
        real*4, allocatable :: x(:),xx(:)
c------------------------------------
        if (nstepcol(ileg).eq.0) then
        nstepcol(ileg) = (maxcol-firstscal-1)/(nrgb)
        else
        nstepcol(ileg) = min(nstepcol(ileg),(maxcol-firstscal-1)/(nrgb))
        endif
        ncolor = nrgb*nstepcol(ileg)
c-------------------------------------
        allocate (x(nrgb),xx(ncolor))
 
          do i = 1,ncolor
          xx(i) = (i-0.5)/ncolor
          enddo
 
         x(1) = xx(1)
          do i = 2,nrgb-1
          x(i) = (i-0.5)/nrgb
          enddo
         x(nrgb) = xx(ncolor)
c------------------------------------
         j = 1
          do i = 1,ncolor
            labrgb: do
              if (x(j+1).ge.xx(i)) then
              cstep = (xx(i)-x(j))/(x(j+1)-x(j))
              fred = rs(j) + cstep*(rs(j+1)-rs(j))
              fgreen = gs(j) + cstep*(gs(j+1)-gs(j))
              fblue = bs(j) + cstep*(bs(j+1)-bs(j))
              call pssetcol (i+firstscal,fred,fgreen,fblue)
              exit labrgb
              endif
            j = j+1
            enddo labrgb
          enddo
c--------------------------------------------------------------------
        call pssetcol (firstscal,rs(0),gs(0),bs(0))
        call pssetcol (maxcol,rs(nrgb+1),gs(nrgb+1),bs(nrgb+1))
c--------------------------------------------------------------------
        print *
        print *,'nrgb=',nrgb,' nstepcol=',nstepcol(ileg),' maxcol=',maxcol
     &        ,' nvalue=',nvalue(ileg)
c---------------------------------------------
        if (check) then
          do i = firstscal,maxcol
          call psgetcol (i,fred,fgreen,fblue)
          print '(i4,3f8.3)',i,fred,fgreen,fblue
          enddo
        endif
c------------------------------------
        deallocate (x,xx)
c------------------------------------
        return
        end
c------------------------------------------------------
        subroutine  fillcolor (val,i,j,icol,doBullet,doGrid)
        use interface_mod
        implicit none
        real*4 val
c i: column index
c j: row index
        integer*4 i,j,m, icol
        logical*4 doBullet, doGrid
 
 
c define x,y position (center)
 
        outwin = .true.
        xval = xor + pixsize*(i-0.5)
        yval = yor - pixsize*(j-0.5)
 
        if    (xval.lt.xmin.or.xval.gt.xmax
     &     .or.yval.lt.ymin.or.yval.gt.ymax) return

        outwin = .false.
 
        xval = xval + dxmap - xmin
        yval = yval + dymap - ymin
 
c fill circle
              if (doBullet) then
              call psfillcirc (xval,yval,bulletsize,bulletsize,icol)
                if (bulletcircle) then
                call pscirc (xval,yval,bulletsize,bulletsize,black)
                endif
c fill square
              else
              call psfillrect (xval-pixsize/2,yval-pixsize/2
     &                        ,xval+pixsize/2,yval+pixsize/2,icol)
                if (doGrid) then
                call psrect (xval-pixsize/2,yval-pixsize/2
     &                        ,xval+pixsize/2,yval+pixsize/2,black)
                endif
              endif
 
        return
        end
c------------------------------------------------------
        function  colval (val)
        use interface_mod
        implicit none
        real*4 val
c i: column index
c j: row index
        integer*4 colval
 
        real*4 frac
 
c define color
 
        if (val.le.vmin(ileg)) then
        colval = firstscal
 
        else if (val.ge.vmax(ileg)) then
        colval = maxcol
 
        else
          if (leglog(ileg)) then
            colval = log(val/vmin(ileg))*vstep + firstscal  + 1
          else
            colval = (val-vmin(ileg))*vstep + firstscal + 1
          endif
 
        endif
 
        return
        end