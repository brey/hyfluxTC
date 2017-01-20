	subroutine open_nimrod (cfilename, verify, num_cols, num_rows, data_resolution)
!
!***111111111111111111111111111111111111111111111111111111111111111111111111
!
! TYPE: subroutine
!
! LANGUAGE: FORTRAN
!
! ORIGIN: read_nimrod_file  program
!
!
!***222222222222222222222222222222222222222222222222222222222222222222222222
!
! PURPOSE: read a Nimrod formatted file
!
! Created 16.02.2004 by G. Franchello (GF)
!
!--------------------------------------------------------------------------
	IMPLICIT NONE
!
!**
! Local parameter variables
!
	INTEGER, PARAMETER :: pihead = 31	! Length of integer general section of Nimrod header
	INTEGER, PARAMETER :: piheadsp = 51	! Length of integer specific section of Nimrod header
	INTEGER, PARAMETER :: prhead = 28	! Length of real general section of Nimrod header
	INTEGER, PARAMETER :: prheadsp = 45  	! Length of real specific section of Nimrod header
	INTEGER, PARAMETER :: imdi = -32767	! Nimrod integer missing data indicator
!
	REAL, PARAMETER :: rmdi = -32767.0	! Nimrod real missing data indicator
!
! Local workspace variables
!
	INTEGER :: num_cols					     ! Number of columns in data array
	INTEGER :: num_rows					     ! Number of rows in data array
	INTEGER :: data_type					 ! Data type (real, integer etc.)
	INTEGER :: num_bytes				     ! Number of bytes per data element (int*2 = 2 etc.)
	INTEGER :: field_code_number			 ! field code number
	INTEGER :: ifilelen					     ! Number of characters in filename
	INTEGER :: ierror					     ! Error code
	INTEGER :: iunit					     ! Logical unit number for accessing files
	INTEGER :: iorder					     ! Location of data origin (0 = Top LH corner etc.)
	INTEGER (KIND=2), DIMENSION(pihead) :: iheader_general     ! Integer general part of Nimrod header
	INTEGER (KIND=2), DIMENSION(piheadsp) :: iheader_specific  ! Integer specific part of Nimrod header
 
 
	INTEGER (KIND=2), DIMENSION(:,:), ALLOCATABLE :: i2data    ! Integer*2 data array
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: i4data             ! Integer*4 data array
!
	REAL :: data_resolution				     ! Data pixel size
	REAL :: scaling_factor				     ! Data scaling factor
	REAL :: data_offset					     ! Data offset
	REAL, DIMENSION(prhead) :: rheader_general		     ! Real general part of Nimrod header
	REAL, DIMENSION(prheadsp) :: rheader_specific 	     ! Real specific part of Nimrod header
	REAL, DIMENSION(:,:), ALLOCATABLE :: rdata    	     ! Real data array
!
	CHARACTER (LEN=3)   :: cstatus			     ! Status of file to open, 'NEW' or 'OLD'
	CHARACTER (LEN=56)  :: cheader		             ! Character header part of Nimrod header
!	CHARACTER (LEN=200) :: cfilename			     ! Input Filename
	CHARACTER  cfilename*(*)			     ! Input Filename
 
!
! External routines called
!
	EXTERNAL :: get_free_lun		! Retrieve next available logical unit number
	EXTERNAL :: nimrod_open		! Opens a Nimrod formatted file
	EXTERNAL :: nimrod_header		! Reads the header from a previously opened Nimrod file
	EXTERNAL :: nimrod_rread		! Reads the header and real data section from a previously opened Nimrod file
	EXTERNAL :: nimrod_iread		! Reads the header and integer*2 data section from a previously opened Nimrod file
	EXTERNAL :: nimrod_i4read		! Reads the header and integer*4 data section from a previously opened Nimrod file
!------------------------------------------------------------------------------
! local variables included by GF
	logical verify, exist, verify1
	integer nc,nr, length, lm, i, j
	integer imdi2 /-1/
	real*4 mv /z'ffffffff'/
	real*4 buff(num_cols, num_rows)   ! buffer to store data
!**
!----------------------------------------------------------------------------
	save data_type, num_bytes, iunit, iorder, scaling_factor
!----------------------------------------------------------------------------
!
!* 1. Initialise arrays, generate filename of file we want to open and get
!     free logical unit no.
!
	iheader_general = imdi
	iheader_specific = imdi
	i2data = imdi
	i4data = imdi
	rheader_general = rmdi
	rheader_specific = rmdi
	rdata = rmdi
	cheader = ''
!-----------------------------------------------
! see if the files are specified
! cfilename
	if (cfilename.eq.' ') then
	print *,'you have not specified the input file'
	call exit(1)
	endif
 
	lm = length(cfilename)
	inquire (file=cfilename, exist=exist)
	  if (.not.exist) Then
	  print *,'The file ',cfilename(1:lm),' does not exist'
	  call exit(1)
	  endif
 
	ifilelen = lm
 
!-----------------------------------------------
! Set file status
!
	cstatus = 'OLD'
!
! Get free logical unit number to open file
!
	CALL GET_FREE_LUN ( iunit,    ierror )
!
! Check error code to make sure we've retrieved a unit number
!
	IF ( ierror .NE. 0 ) THEN
	print*,'ERROR retrieving logical unit number, exiting program'
	GOTO 999
	ENDIF
!
!**
!----------------------------------------------------------------------------
!
!* 2. Open file and read header
!
!
! Call nimrod_open.f to open file so we can read the header
!
	CALL NIMROD_OPEN ( iunit,
     &	     cfilename(1:ifilelen),
     & 	     cstatus,
     &	     ierror )
!
! Check error code and exit program if error has occured
!
	IF ( ierror .NE. 0 ) THEN
	print*,'ERROR opening file ',cfilename(1:ifilelen),' on unit number',iunit
		print*,'exiting program'
	GOTO 999
	ENDIF
!
! Call nimrod_header.f to read header
!
	CALL NIMROD_HEADER ( iunit,
     &	       iheader_general,
     &   	       rheader_general,
     &    	       cheader,	
     &	       rheader_specific,
     &   	       iheader_specific,
     &      	       ierror )
!
! Check error code and exit program if error has occured
!
	IF ( ierror .NE. 0 ) THEN
	print*,'ERROR reading header of file ',cfilename(1:ifilelen),' on unit number',iunit
	print*,'exiting program'
	GOTO 999
	ENDIF
!
! Rewind file so that it can be read when we read the header and data together
!
	REWIND (iunit)
!
!**
!----------------------------------------------------------------------------
!
!* 3. Retrieve values we need from header to determine what the data is and
!     allocate memory to the correct data array
!
	data_type = iheader_general(12)
	num_bytes = iheader_general(13)
	num_cols = iheader_general(17)
	num_rows = iheader_general(16)
	iorder = iheader_general(24)
	field_code_number = iheader_general(19)
 
	data_resolution = rheader_general(4)
	scaling_factor = rheader_general(8)
	data_offset = rheader_general(9)
 
!----------------------------------------------------
	if (verify) then
	print *,'data_type=',data_type
	print *,'num_bytes=',num_bytes
	print *,'num_cols=',num_cols
	print *,'num_rows=',num_rows
	print *,'iorder=',iorder
	print *,'field_code_number=',field_code_number
	print *,'data_resolution=',data_resolution
	print *,'scaling_factor=',scaling_factor
	print *,'data_offset=',data_offset
 
	print *,' '
	print *,'int. head: general'
  	  do i = 1,pihead
	    if (iheader_general(i).ne.imdi) print *,i,iheader_general(i)
	  enddo
 
	print *,' '
	print *,'Real head: general'
	  do i = 1,prhead
	    if (rheader_general(i).ne.rmdi) print *,i,rheader_general(i)
	  enddo
 
	print *,' '
	print *,'Real head: specific'
	  do i = 1,prheadsp
	    if (rheader_specific(i).ne.rmdi) print *,i,rheader_specific(i)
	  enddo
 
	print *,' '
	print *,'charac. header'
	print *,cheader
 
	print *,' '
	print *,'int. head: specific'
	  do i = 1,piheadsp
	    if (iheader_specific(i).ne.imdi) print *,i,iheader_specific(i)
	  enddo
	endif
 
	return
!----------------------------------------------------
!----------------------------------------------------
	ENTRY read_nimrod (verify1,buff,num_cols, num_rows)
!----------------------------------------------------
!----------------------------------------------------
!
	 SELECT CASE(data_type)
	  CASE(0)			! Data is of type real
      ALLOCATE ( rdata(num_cols, num_rows),	
     &		 STAT = ierror )
 
!
!     Check error code and exit program if error has occured
!
      IF ( ierror .NE. 0 ) THEN
        print*,'ERROR allocating memory to data array, exiting program'
        GOTO 999
      ENDIF
!        	
	    CASE(1)			! Data is of type integer
!
!     Check how many bytes per data element and allocate memory accordingly
!
      IF ( num_bytes .EQ. 2 ) THEN 	! Int*2 data
!	
        ALLOCATE ( i2data(num_cols, num_rows), STAT = ierror )
!
!       Check error code and exit program if error has occured
!
        IF ( ierror .NE. 0 ) THEN
          print*,'ERROR allocating memory to data array, exiting program'
          GOTO 999
        ENDIF
!
      ELSEIF ( num_bytes .EQ. 4 ) THEN  ! Int*4 data
!
        ALLOCATE ( i4data(num_cols, num_rows), STAT = ierror )	
!
!       Check error code and exit program if error has occured
!
        IF ( ierror .NE. 0 ) THEN
          print*,'ERROR allocating memory to data array, exiting program'
          GOTO 999
        ENDIF
!
      ELSE
	print*,'ERROR: Unknown number of bytes',num_bytes,'for integer data type'
      ENDIF
!        	
	  CASE DEFAULT		! Unknown data type      	 	
!
      print*,'ERROR: Unknown data type number',data_type,'found. Cannot allocate memory.'
      GOTO 999
!
	END SELECT
!
!**
!----------------------------------------------------------------------------
!
!* 4. Read data depending on data type
!
	SELECT CASE(data_type)
	  CASE(0)			! Data is of type real
      CALL NIMROD_RREAD ( iunit,
     & 			  iorder,	
     &   		  num_cols,		
     &		  num_rows,		
     &		  iheader_general,
     &			  rheader_general,
     &   		  cheader,			
     &			  rheader_specific,	
     &  			  iheader_specific,
     &	      		  rdata,		
     &	   		  ierror )
!
!     Check error code and exit program if error has occured
!
      IF ( ierror .NE. 0 ) THEN
        print*,'ERROR reading real data, exiting program'
        GOTO 999
      ENDIF	
!        	
	   CASE(1)			! Data is of type integer
!
!     Check how many bytes per data element and allocate memory accordingly
!
      IF ( num_bytes .EQ. 2 ) THEN 	! Int*2 data
!
        CALL NIMROD_IREAD ( iunit,	
     & 			    iorder,			
     &	   		    num_cols,			
     &			    num_rows,			
     &			    iheader_general,	
     &			    rheader_general,
     &	    cheader,			
     &		    rheader_specific,	
     & 			    iheader_specific,
     &      		    i2data,			
     &   		    ierror )
!
!       Check error code and exit program if error has occured
!
        IF ( ierror .NE. 0 ) THEN
          print*,'ERROR reading int*2 data, exiting program'
          GOTO 999
        ENDIF
!
      ELSEIF ( num_bytes .EQ. 4 ) THEN  ! Int*4 data
!
        CALL NIMROD_I4READ ( iunit,					
     & 			    iorder,					
     &   		    num_cols,					
     &			    num_rows,					
     &			    iheader_general,				
     & 			    rheader_general,				
     &	   		    cheader,					
     &			    rheader_specific,				
     &  			    iheader_specific,				
     &	      		    i4data,					
     &	   		    ierror )
!
!       Check error code and exit program if error has occured
!
        IF ( ierror .NE. 0 ) THEN
          print*,'ERROR reading int*2 data, exiting program'
          GOTO 999
        ENDIF
!
      ELSE
	print*,'ERROR: Unknown number of bytes',num_bytes,'for integer data type'
      ENDIF
!        	
	  CASE DEFAULT		! Unknown data type      	 	
!
      print*,'ERROR: Unknown data type number',data_type,'found. Cannot read data.'
!
	 END SELECT
!
!**
!----------------------------------------------------------------------------
!
!* 5. Close the file, print a line of the data retrieved and deallocate the
!     memory
!
	  CLOSE (   UNIT = iunit,						
     & 	   IOSTAT = ierror )
!
!  Check error code and warn if error has occured
!
	  IF ( ierror .NE. 0 ) THEN
	  print*,'WARNING: Unable to close file ',cfilename(1:ifilelen),' on unit',iunit
	   ENDIF
!----------------------------------------------------------------
	  SELECT CASE(data_type)
!----------------------------------------------------------------
	    CASE(0)			! Data is of type real
!----------------------------------------------------------------
	if (verify1) then
      print*,'rdata(55,200:300)'
      print*,rdata(55,200:300)
	endif
!-----------------------------------------
! move data to buffer
	do i = 1,num_cols
		do j = 1, num_rows
	      if (rdata(i,j).ne.rmdi) then
		  buff(i,j) = rdata(i,j)*scaling_factor
		  else
		  buff(i,j) = mv
		  endif
		enddo
	enddo
!--------------------------------------
!
!     Deallocate memory
!
      DEALLOCATE ( rdata,						
     & 		   STAT = ierror )
!
!     Check error code and warn if error has occured
!
      IF ( ierror .NE. 0 ) THEN
        print*,'WARNING: Unable to deallocate memory from rdata array'
      ENDIF	
       	
!----------------------------------------------------------------
	   CASE(1)			! Data is of type integer
!----------------------------------------------------------------
!
!     Check how many bytes per data element and allocate memory accordingly
!
!----------------------------------------------------------------
      IF ( num_bytes .EQ. 2 ) THEN 	! Int*2 data
!----------------------------------------------------------------
!
	if (verify1) then
        print*,'i2data(55,200:300)'
        print*,i2data(55,200:300)
	endif
!-----------------------------------------
! move data to buffer
	do i = 1,num_cols
		do j = 1, num_rows
	      if (i2data(i,j).ne.imdi2) then
		  buff(i,j) = i2data(i,j)*scaling_factor
		  else
		  buff(i,j) = mv
		  endif
		enddo
	enddo
!--------------------------------------
!
!       Deallocate memory
!
        DEALLOCATE ( i2data,      		     STAT = ierror )
!
!       Check error code and warn if error has occured
!
        IF ( ierror .NE. 0 ) THEN
          print*,'WARNING: Unable to deallocate memory from i2data array'
        ENDIF
!
!----------------------------------------------------------------
      ELSEIF ( num_bytes .EQ. 4 ) THEN  ! Int*4 data
!----------------------------------------------------------------
!
	if (verify1) then
        print*,'i4data(55,200:300)'
        print*,i4data(55,200:300)
	endif
!-----------------------------------------
! move data to buffer
	do i = 1,num_cols
		do j = 1, num_rows
	      if (i4data(i,j).ne.imdi) then
		  buff(i,j) = i4data(i,j)*scaling_factor
		  else
		  buff(i,j) = mv
		  endif
		enddo
	enddo
!--------------------------------------
!
!       Deallocate memory
!
        DEALLOCATE ( i4data,  		     STAT = ierror )
!
!       Check error code and warn if error has occured
!
        IF ( ierror .NE. 0 ) THEN
          print*,'WARNING: Unable to deallocate memory from i4data array'
        ENDIF
!
      ELSE
	print*,'ERROR: Unknown number of bytes',num_bytes,'for integer data type'
      ENDIF
!        	
	    CASE DEFAULT		! Unknown data type      	 	
!
      print*,'ERROR: Unknown data type number',data_type,'found. Cannot read data.'
!
	  END SELECT
999	CONTINUE
	 END
