!
!  $Header:$
!

module dnb_programs

   private
   public::lunar_reflectance

contains

   subroutine lunar_reflectance( &
           rad_dnb_input &
         , solzen &
         , lunzen &
         , start_year &
         , month &
         , day_of_month &
         , start_time &
         , moon_phase_angle &
         , ref_dnb_lunar )

      implicit none
      
      ! - input
      real ,   intent(in) :: rad_dnb_input(:,:)
      real ,   intent(in) :: solzen(:,:)
      real ,   intent(in) :: lunzen(:,:)
      integer, intent(in) :: start_year
      integer, intent(in) :: month 
      integer, intent(in) :: day_of_month
      integer, intent(in) :: start_time
      real,    intent(in) :: moon_phase_angle 
           
      ! - output
      real, intent(out),allocatable :: ref_dnb_lunar (:,:)
      
      real, allocatable :: cos_weighted_irrad (:,:)
          
      real, parameter :: ASTRO_DARK_THRESH = 109.0 ! Sun 19 degrees or more below horizon
      real, parameter :: MIN_LUNAR_IRRAD_DNB = 1.0e-5 ! W/m^2 = 1.0e-09 W/cm^2, threshold for doing calcs
           
      real , parameter :: PI = ACOS(-1.)
      real , parameter ::  DTOR = PI / 180.
            
      real :: lunar_irradiance_dnb
     
      !  - executable ...
               
      allocate ( cos_weighted_irrad(ubound(rad_dnb_input,1),ubound(rad_dnb_input,2) )  )   
         
      cos_weighted_irrad = cos ( lunzen * DTOR )  &
            *    lunar_irradiance_dnb (start_year, month,day_of_month,start_time &
                     ,'/DATA/Ancil_Data/clavrx_ancil_data/static/dnb_ancils/')
      ref_dnb_lunar =     100 * ( PI * rad_dnb_input  * 1.0e+04) / cos_weighted_irrad
          
      where ( (rad_dnb_input  * 1.0e+04) > 1.0 )
         ref_dnb_lunar = 0.0 
      end where
      
      where ( cos_weighted_irrad <= MIN_LUNAR_IRRAD_DNB )
         ref_dnb_lunar = 0.0 
      end where
    
      where ( solzen < ASTRO_DARK_THRESH )
         ref_dnb_lunar = -999.
      end where  
            
      ref_dnb_lunar = min (ref_dnb_lunar , 150. )
      deallocate ( cos_weighted_irrad)
     

    end subroutine lunar_reflectance
   
   
end module dnb_programs
