!
!  $Header:$
!

program make_lunar_file

   use dnb_programs, only: lunar_reflectance
   
   use readh5dataset, only: &
      H5ReadDataset
      
   use hdf5, only: &
      HID_T &
      ,H5T_NATIVE_REAL &
      , HSIZE_T &
      , H5F_ACC_TRUNC_F &
      , h5dclose_f &
      , h5dcreate_f &
      , h5dwrite_f &
      , h5fclose_f &
      , h5fcreate_f &
      , h5open_f &
      , h5sclose_f &
      , h5screate_simple_f
   
   implicit none
   
  
   character(len=250) :: dir_file
   character(len=250) :: data_path
   
   character(len=150)::GDNBO_File, SVDNB_File,svdnb_file_short
   real, pointer :: nfititer(:,:)
   real, pointer :: lun_az(:,:)
   real, pointer :: lun_zen(:,:)
   real, pointer :: rad_chdnb(:,:)
   real, pointer :: sol_zen(:,:)
   real, pointer :: sat_az(:,:)
   real, pointer :: lon(:,:) 
   real, pointer :: lat(:,:)
   real, allocatable::ref_chdnb_lunar(:,:)
   real, pointer :: moon_illum_frac(:)
   real, pointer :: moon_phase_angle(:)
   
   integer::year,doy,start_time,end_time,orbit,month,day
   character(128) :: arg_string
   character(128) :: file
   integer :: file_unit, ioerr
   logical :: file_exists
   character(128)::outfile
   character(38) :: orbit_identifier
   integer(HID_T) :: file_h5, group,space, dset, dcpl  ! Handles
   integer :: hdferr
   integer(HSIZE_T), DIMENSION(1:2) :: dims 
  
   file_unit = 10

   open(unit=file_unit, file='dnb_input', status ='old' &
        , action='read' , iostat=ioerr)
   read(file_unit, fmt="(a)" ) data_path
   read(file_unit, * ) svdnb_file_short
   read(file_unit, * ) gdnbo_file
   close(file_unit)
  
   print*,'start'
   data_path=trim(data_path)//'/'
   SVDNB_file = trim(data_path)//trim(svdnb_file_short)
   GDNBO_File = trim(data_path)//trim(gdnbo_file)
  
   dir_file =  trim(GDNBO_File)

   inquire(file=dir_file,exist=file_exists)
   
   if (.not. file_exists ) then
      print*,'GDNBO file not found ',dir_file
      return
   end if 
   
   
   inquire(file=svdnb_file,exist=file_exists)
   
   if (.not. file_exists ) then
      print*,'SVDNBO file not found ',dir_file
      return
   end if 
   
   
   
   call H5ReadDataset(trim(svdnb_file),'All_Data/VIIRS-DNB-SDR_All/Radiance',rad_chdnb)
 
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/SolarZenithAngle',sol_zen)
    
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/LunarAzimuthAngle' , lun_az)
  
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/LunarZenithAngle' , lun_zen)
  
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/SatelliteAzimuthAngle',sat_az)
  
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/MoonPhaseAngle' , moon_phase_angle) 
  
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/MoonIllumFraction' , moon_illum_frac)
  
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/Longitude' , lon) 
   
   call H5ReadDataset(dir_file,'All_Data/VIIRS-DNB-GEO_All/Latitude' , lat)
   

   call Read_viirs_date_time(trim(svdnb_file_short),year,month,day,doy,start_time,end_time,orbit,orbit_identifier)
   

   call  lunar_reflectance(rad_chdnb , sol_zen, lun_zen, year, month, day, start_time &
          & , moon_phase_angle(1),  ref_chdnb_lunar)
  
   print*,'maximal value lunar reflectance = ', maxval(ref_chdnb_lunar)
  
   print*,'if value equals 0 then this scene is daytime....'
   print*
   
   dims = shape(ref_chdnb_lunar)

   outfile = 'results/'//trim('LUNAR_REFLECTANCE_')//trim(svdnb_file_short(7:37))//'.h5'
  
   call h5open_f(hdferr)
   call h5fcreate_f(outfile, H5F_ACC_TRUNC_F, file_h5, hdferr)
   call h5screate_simple_f(2, dims, space, hdferr)
  
   call h5dcreate_f(file_h5, "ref_chdnb_lunar", H5T_NATIVE_REAL, space, dset, hdferr)
   call h5dwrite_f(dset, H5T_NATIVE_REAL, ref_chdnb_lunar, dims, hdferr)
   call h5dcreate_f(file_h5, "longitude", H5T_NATIVE_REAL, space, dset, hdferr)
   call h5dwrite_f(dset, H5T_NATIVE_REAL, lon, dims, hdferr)
   call h5dcreate_f(file_h5, "latitude", H5T_NATIVE_REAL, space, dset, hdferr)
   call h5dwrite_f(dset, H5T_NATIVE_REAL, lat, dims, hdferr)
  
   call h5dclose_f(dset , hdferr)
   call h5sclose_f(space, hdferr)
   call h5fclose_f(file_h5 , hdferr)

contains
  
   subroutine read_viirs_date_time(Infile,year,month,day,doy,start_time,end_time,orbit, orbit_identifier)
   ! Get the date & time from the file's name

      character(len=*), intent(in) :: infile   
      integer, intent(out) :: year
      integer, intent(out) :: doy    !day of year
      integer, intent(out) :: start_time  !millisec
      integer, intent(out) :: end_time    !millisec
      integer, intent(out) :: orbit
	   character(38), intent(out) :: orbit_identifier
      integer :: ileap
      integer :: month
      integer :: day
      integer :: start_hour
      integer :: start_minute
      integer :: start_sec
      integer :: end_hour
      integer :: end_minute
      integer :: end_sec
      integer :: first_day_month(12)
  
      !         1	    2	      3	        4	  5	    6	      7	        8
      !12345678901234567890123456789012345678901234567890123456789012345678901234567890
      !GMODO_npp_d20100906_t2110510_e2112156_b00012_c20110707160532497848_noaa_ops.h5

      ! --- Read data from the file name
      read(Infile(12:15), fmt="(I4)") year
      read(Infile(16:17), fmt="(I2)") month
      read(Infile(18:19), fmt="(I2)") day
      read(Infile(22:23), fmt="(I2)") start_hour
      read(Infile(24:25), fmt="(I2)") start_minute
      read(Infile(26:27), fmt="(I2)") start_sec
      read(Infile(31:32), fmt="(I2)") end_hour
      read(Infile(33:34), fmt="(I2)") end_minute
      read(Infile(35:36), fmt="(I2)") end_sec
      read(Infile(40:44), fmt="(I5)") orbit
	   orbit_identifier = infile(7:44)
 
      ! --- Calculate the date of year
      ileap = 0
      ! warninng , this will cause an error in 185 years  ( but I don't care at all )
      if (year /4 .eq. year/4.) ileap = 1
      if ( year == 2100 ) ileap = 0
      
      first_day_month = (/ 1 , 31 , 59  , 90  , 120  &
                         , 151 , 181, 212 , 243 , 273 , 304 , 334 /)
      first_day_month ( 3: ) =  first_day_month ( 3: ) + ileap
      doy = first_day_month(month) + day

      ! --- Calculate start END time
      start_time = ((start_hour * 60 + start_minute) * 60 + start_sec) * 1000
      end_time = ((end_hour * 60 + end_minute) * 60 + end_sec) * 1000

   end subroutine READ_VIIRS_DATE_TIME

end program make_lunar_file
