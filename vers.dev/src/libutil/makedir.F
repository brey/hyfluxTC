      function makedir(dir)
#if defined WIN32
      use dflib
#endif
      character dir*(*)
*      integer system
      integer makedir,length,ld
      ld=length(dir)
      
#if defined WIN32
      logical result
      result = makedirqq (dir)
      makedir = 0
        if (.not.result) makedir = -1
#else
      call system ('if [ ! -d ' //dir(1:ld)// ' ] ; then mkdir ' //dir(1:ld)// ' ; fi &> /dev/null ')
      makedir = 0      
#endif
      return
      end
