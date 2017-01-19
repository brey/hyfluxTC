
c============================================================================
        subroutine srtm2map
c============================================================================
        use parameter_data
        use map_data
        use map_cover_data
        use window_data
	implicit none
c---------------------------------------------------------------------
        integer(i4) i,j,l

c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
       cellsize = min(dlong0,dlat0)
        nvx = nlong0 + 1
        nvy = nlat0 + 1
        xstart = longmin0 -dlong0/2
        ystart = latmax0 + dlat0/2
        nval = nvx*nvy
       allocate (val(nval))
c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        call getsrtm  (verify,cover,intsrtm,iout,srtmpath,'.Bathymetry.srtm', workdir(1:lw)
     &                       ,xstart,ystart,cellsize,nvx,nvy,val
     &           , sizey, mapfmt, mapext, nband,error)

        demNodemap = workdir(1:lw)//bs//'demNode'//mapext
        call OUTGDAL (demNodemap,nvx,nvy,4
     &                  ,xstart,ystart,cellsize
     &                  ,val, nval
     &           , sizey, mapfmt, nband,error)

        deallocate (val)

       return
       end

