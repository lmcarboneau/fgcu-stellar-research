FUNCTION radec_2_ksieta, ra_in, de_in, ra0_in, de0_in

 ; ------------------------------------------------------------------------------------------
 ;
 ; All input values must be in degrees
 ;
 ; ra  = Alpha for reference star(s)
 ; de  = Delta for reference star(s)
 ; ra0 = Alpha for field center
 ; de0 = Delta for field center
 ;
 ; ------------------------------------------------------------------------------------------

 conv    = !dpi / 180.0D
 ra      = double( ra_in  * conv )
 de      = double( de_in  * conv )
 ra0     = double( ra0_in * conv )
 de0     = double( de0_in * conv )

 ; Now, we can calculate ksi and eta according to the prescriptions
 ; given in Green, chapter 13

 naevner = sin( de0 ) * sin( de ) + cos( de0 ) * cos( de ) * cos( ra - ra0 )
 ksi     = cos( de ) * sin( ra - ra0 ) / naevner
 eta     = [ cos( de0 ) * sin( de ) - sin( de0 )* cos( de ) * cos( ra - ra0 ) ] / naevner

 if n_elements( ksi ) eq 1 then Return, [ksi, eta ]
 if n_elements( ksi ) gt 1 then Return, [ transpose( ksi ), transpose( eta ) ]

END ; ----------------------------------------
