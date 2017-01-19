#include <modules.F>
*================================================================================
      program map2watch
*================================================================================
      use parameter_data
      use statistics_data
      use window_data
      use geometry_data
      use hydro_data
      use slope_data
      use boundary_data
      implicit none
c------------------------------------
      logical exist
c------------------------------------
      call cputime (cpu0)
      call elapstime (elaps0)
c------------------------------------
	code = 'map2watch'
c------------------------------------
      CALL SET_DEFAULTS
      CALL GET_PARAMETERS (.true.)
      CALL DERIVED_PARAMETERS
      call show_parameters

c------------------------------
      write (iout,*) '========================================================'
      write (iout,*) ' Start initWatch'
      write (iout,*) '========================================================'
      call initWatch (
        if (watch) then
        write (iout,*) ' Done initWatch: watch=',watch
	else
        write (iout,*) 'watchfile=',watchfile(1:length(watchfile))
        write (iout,*) 'watchdir=',watchdir(1:length(watchdir))
        endif

c------------------------------------
      call cputime (cpu1)
      call elapstime (elaps1)
      write (iout,*) 'cpu time =', cpu1-cpu0,' sec'
      write (iout,*) 'elaps time =', elaps1-elaps0,' sec'
c------------------------------------
	call closeParam(0)
c------------------------------------
      stop
      end
