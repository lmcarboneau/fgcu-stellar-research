PRO wire_getpar, fj,p,forb,forb04,w99,w00,dens_cen04,dens_cen99

; Orbital freq. of wire (Procyon data) + conversion constants
fj=1e6/86400D & p = 3.6352700*fj  
forb = 173.62563  
forb04 = 177.546


; Resolution element: wire_power_resolution.pro
w99 = 3.5270790 & w00 = 5.3577757D

; Resolution in power spectra for Alpha Cen observations:
dens_cen04 = 1.4747115D
dens_cen99 = 1.0125118D

end
