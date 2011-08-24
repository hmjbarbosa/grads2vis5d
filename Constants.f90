MODULE CONSTANTS
    IMPLICIT NONE

    PUBLIC
    
    ! Sizes
    INTEGER, PARAMETER :: max_var=200
    INTEGER, PARAMETER :: max_time=400

    ! Physical Parameters
    REAL, PARAMETER :: H = 7.2 ! Lapse ratio (K/km)
    REAL, PARAMETER :: Po = 1012.15 ! Reference pressure (hPa)

    REAL, PARAMETER :: R = 287.0 ! Gas constant (J/Kg/K)
    REAL, PARAMETER :: g = 9.8 ! Acceleration due to gravity (m/s2)

END MODULE CONSTANTS
