!>
!   $Header:$
!  Steve's Lunar Irradiance Algorithm
!
real function lunar_irradiance_dnb ( &
      year &
   , month &
   , day_of_month &
   , msec_of_day  &
   , ancil_data_dir )
   
   implicit none
   integer ( kind = 4 )  :: year
   integer ( kind = 4 )  :: month
   integer ( kind= 4 )   :: day_of_month
   integer ( kind = 4 )  :: msec_of_day
   character ( len = * ) :: ancil_data_dir
!INTF_END    
  ! constants
   double precision , parameter :: MEAN_EARTH_SUN_DIST = 149598022.6071
   double precision , parameter :: MEAN_EARTH_MOON_DIST = 384400.0
   real , parameter :: EARTH_RADIUS_KM = 6378.140
   real , parameter:: SRF_INTEG = 0.32560294 ! integral of the DNB sensor response function (micron)
   
   integer, parameter :: DOUBLE_SIZE = 8
   integer, parameter :: FLOAT_SIZE = 4
   real , parameter:: PI = ACOS(-1.)
   real , parameter::  DTOR = PI / 180.
   character(len=128) :: lunar_irrad_file 
   character(len=128) :: distance_table_file
   
   ! NEW 3/26/2014 based on Gauss curve-fits to Obs/Mod ratio data between -120(wax) and 120(wane) degrees      
   ! updated values 05/05/2015 
   DOUBLE PRECISION ::  lpds ! -120 wax, +120 wane
 
   double precision :: WAXP1 = 5.2228373e-12
   double precision :: WAXP2 = 2.2410515e-9
   double precision :: WAXP3 = 3.7994231e-7
   double precision :: WAXP4 = 3.1637454e-5
   double precision :: WAXP5 = 1.3079265e-3
   double precision :: WAXP6 = 2.3328238e-2
   double precision :: WAXP7 = 1.1448359

   double precision :: WANP1 = 6.3455594e-12
   double precision :: WANP2 = -2.6095939e-9
   double precision :: WANP3 = 4.2557303e-7
   double precision :: WANP4 = -3.4087847e-5
   double precision :: WANP5 = 1.3562948e-3
   double precision :: WANP6 = -2.5037150e-2
   double precision :: WANP7 = 1.1450824
      
   integer(kind=4), parameter :: NUM_IRRAD_TABVALS = 181
   integer(kind=4), parameter :: NUM_DIST_TABVALS = 184080
   
   real :: lunar_irrad_lut(2,NUM_IRRAD_TABVALS)
   double precision :: dist_phase_lut ( 4, NUM_DIST_TABVALS )
   double precision :: phase_array ( NUM_IRRAD_TABVALS)
   
   real :: hour_fraction
   real :: minute
   real :: hour
   integer :: idx_lut
   double precision :: yyyymmddhh
   
   integer :: idx_lut_irrad
   real :: phase_fraction
   real :: phase_albedo_correction_factor
   
   double precision :: lunar_phase_angle_geo
   double precision :: cos_phase_angle
   double precision :: curr_earth_sun_dist
   double precision :: curr_earth_moon_dist 
   double precision :: curr_mean_irrad
   double precision :: cos_weighted_irrad
   
   double precision :: denorm1 
   double precision :: denorm2
   double precision :: denorm3
   double precision :: denorm_factor
      
   logical :: is_waning
   
   distance_table_file=trim(ancil_data_dir)//'DIST_2010-2030_double.bin'
   open (unit=1,file=trim(distance_table_file),status="old",action="read",&
         & access="direct",form="unformatted",recl= DOUBLE_SIZE * 4 * NUM_DIST_TABVALS )
      read (unit=1,rec=1) dist_phase_lut
   close (1)
    
    ! - transform to time index for tables
   minute = mod ( msec_of_day /1000./60., 60. )
   hour_fraction = minute / 60.
   hour = floor ( msec_of_day / 1000./60./60. )
   
   yyyymmddhh = year * 1000000+month * 10000+ day_of_month * 100 
   yyyymmddhh = yyyymmddhh + hour
   idx_lut = index_in_vector ( dist_phase_lut (1,:), NUM_DIST_TABVALS , yyyymmddhh )
   idx_lut = max ( 1 , min( NUM_DIST_TABVALS , idx_lut ) )
   
   lunar_phase_angle_geo    = dist_phase_lut(2,idx_lut)  &
                        & +  hour_fraction &
                        & * (dist_phase_lut(2,idx_lut + 1) &
                        & - dist_phase_lut(2,idx_lut))
                        
   curr_earth_sun_dist  = dist_phase_lut(3,idx_lut)  &
                        & + hour_fraction &
                        & * (dist_phase_lut(3,idx_lut + 1) &
                        & - dist_phase_lut(3,idx_lut))
                        
   curr_earth_moon_dist = dist_phase_lut(4,idx_lut)  &
                        & + hour_fraction &
                        & * (dist_phase_lut(4,idx_lut + 1) &
                        & - dist_phase_lut(4,idx_lut))
                        
   lunar_irrad_file = trim(ancil_data_dir)//'lunar_irrad_Mean_DNB.bin'
   
   open (unit=1,file=trim(lunar_irrad_file),status="old",action="read",&
      & access="direct",form="unformatted",recl = FLOAT_SIZE * 2 * NUM_IRRAD_TABVALS)
         read (unit=1,rec=1) lunar_irrad_lut
   close (1)                     
   
   
   ! b) interpolate lunar_irrad_lut() to get current mean-geometry lunar irradiance pre-convolved to dnb srf
   !   use topo phase angle 
   phase_fraction  = lunar_phase_angle_geo - int(lunar_phase_angle_geo)
   phase_array     = lunar_irrad_lut(1,:)
   idx_lut_irrad   = index_in_vector(phase_array,num_irrad_tabvals,lunar_phase_angle_geo)
   curr_mean_irrad = lunar_irrad_lut(2,idx_lut_irrad) + &
                     &   phase_fraction*(lunar_irrad_lut(2,idx_lut_irrad+1) &
                           -lunar_irrad_lut(2,idx_lut_irrad))
   
   
   cos_phase_angle = cos(lunar_phase_angle_geo * DTOR)
   denorm1 = mean_earth_sun_dist ** 2 + mean_earth_moon_dist ** 2 + &
           2.0 * mean_earth_moon_dist * mean_earth_sun_dist * cos_phase_angle
   denorm2 = curr_earth_sun_dist ** 2 + curr_earth_moon_dist ** 2 + &
           2.0 * curr_earth_moon_dist * curr_earth_sun_dist * cos_phase_angle
   denorm3 = ((mean_earth_moon_dist - earth_radius_km ) / (curr_earth_moon_dist - earth_radius_km)) ** 2.0
   denorm_factor = (denorm1 / denorm2 ) * denorm3
   
                      
   lunar_irradiance_dnb =  curr_mean_irrad * denorm_factor * ( SRF_INTEG * 1.0e-03)
   
   
   lpds = lunar_phase_angle_geo
   is_waning = dist_phase_LUT(2,idx_lut ) > dist_phase_LUT(2,idx_lut + 1)      
   if ( is_waning ) lpds = lpds * ( -1 )
            
   if ( abs ( lpds ) > 120.0 ) then
      phase_albedo_correction_factor = 1.
   else if ( lpds < 0 ) then
      phase_albedo_correction_factor = waxp1*lpds**6 + waxp2*lpds**5 + waxp3*lpds**4 + &
                                          waxp4*lpds**3  +waxp5*lpds**2 + waxp6*lpds + waxp7   
   else 
      phase_albedo_correction_factor = wanp1*lpds**6 + wanp2*lpds**5 + wanp3*lpds**4 + &
                                          wanp4*lpds**3  +wanp5*lpds**2 + wanp6*lpds + wanp7
   end if
      
   lunar_irradiance_dnb = lunar_irradiance_dnb * phase_albedo_correction_factor
   
   
   contains
  
      integer function index_in_vector(xx,n,x)
  
         integer, intent(in)::n
         double precision, intent(in)::x
         double precision,dimension(:),intent(in)::xx
         integer:: i,jl,jm,ju
         jl = 0
         ju=n+1

         do i=1,2*n
            if (ju-jl <= 1) then
               exit
            end if
            jm=(ju+jl)/2
            if ( (xx(n) >= xx(1)) .eqv. (x >= xx(jm)) ) then
               jl=jm
            else
               ju=jm
            end if
         end do

         if (x == xx(1)) then
            index_in_vector =1
         else if (x == xx(n)) then
            index_in_vector = n-1
         else
             index_in_vector = jl
         end if
  
      end function index_in_vector   
end function lunar_irradiance_dnb


