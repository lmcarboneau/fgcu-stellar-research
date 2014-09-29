FUNCTION ksieta_2_radec, ksi_in, eta_in, ra0_in, de0_in

     ; ra0_in must be in degrees 
     ; de0_in must be in degrees 
     conv  = !dpi / 180.0D

     ; Convert to radians
     ra0   = double( conv * ra0_in )
     de0   = double( conv * de0_in )
     ksi   = double( ksi_in )
     eta   = double( eta_in )

     ; Calculate RA and DEC
     denom = cos( de0 ) - eta * sin( de0 )
     raa   = ( ra0 + atan( ksi / denom ) ) 

     dee   = cos( [raa-ra0] ) * [ sin( de0 )+ eta * cos( de0 ) ] / denom  
     dee   = atan( dee )

     raa   = raa / conv
     dee   = dee / conv

     if n_elements( raa ) eq 1 then return, [ raa, dee ]
     if n_elements( raa ) gt 1 then return, [ transpose( raa ), transpose( dee ) ]

END
