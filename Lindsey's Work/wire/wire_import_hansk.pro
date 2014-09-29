PRO wire_import_hansk, hk

; =========================================================
hk = replicate( {f99: 0., a99: 0., d99: 0., $
                 fs99:0., as99:0., ds99:0., $
                 f00: 0., a00: 0., d00: 0., $
                 fs00:0., as00:0., ds00:0. }, 100000)
; =========================================================

close,1

; =========================================================
; From wire_power_resolution.pro:
; =========================================================
dens_pro99 = 3.5270944D
dens_pro00 = 5.3577757D

dens_cen04 = 1.4747115D
dens_cen99 = 1.0125118D
; =========================================================

; =========================================================
fil99 = '/ai40/bruntt/wire/procyon/procyon_data/spec99.dat'
spawnrob,'wc ' + fil99, a
b = strsplit(a,' ',/extract) & n = long(a(0))

d = fltarr(4,n)
openr,1,fil99 & readf,1,d & close,1

hk(0:n-1).f99 = reform( d(1,*) )
hk(0:n-1).a99 = reform( d(2,*) )
hk(0:n-1).d99 = reform( (d(2,*))^2.0 / dens_pro99 )
; =========================================================

; THIS IS THE SMOOTH DENSITY CURVE FROM HANS KJELDSEN 6th AUG 2004
; =========================================================
fil99 = '/ai40/bruntt/wire/procyon/procyon_data/spec99.smooth'
spawnrob,'wc ' + fil99, a
b = strsplit(a,' ',/extract) & n = long(a(0))

d = fltarr(2,n)
openr,1,fil99 & readf,1,d & close,1

hk(0:n-1).fs99 = reform( d(0,*) )
hk(0:n-1).ds99 = reform( d(1,*) )
hk(0:n-1).as99 = sqrt( hk(0:n-1).ds99 * dens_pro99 )
; =========================================================



; =========================================================
fil00 = '/ai40/bruntt/wire/procyon/procyon_data/spec00.dat'
spawnrob,'wc ' + fil00, a
b = strsplit(a,' ',/extract) & n = long(a(0))

d = fltarr(4,n)
openr,1,fil00 & readf,1,d & close,1

hk(0:n-1).f00 = reform( d(1,*) )
hk(0:n-1).a00 = reform( d(2,*) )
hk(0:n-1).d00 = reform( (d(2,*))^2.0 / dens_pro00 )
; =========================================================

; THIS IS THE SMOOTH DENSITY CURVE FROM HANS KJELDSEN 6th AUG 2004
; =========================================================
fil00 = '/ai40/bruntt/wire/procyon/procyon_data/spec00.smooth'
spawnrob,'wc ' + fil00, a
b = strsplit(a,' ',/extract) & n = long(a(0))

d = fltarr(2,n)
openr,1,fil00 & readf,1,d & close,1

hk(0:n-1).fs00 = reform( d(0,*) )
hk(0:n-1).ds00 = reform( d(1,*) )
hk(0:n-1).as00 = sqrt( hk(0:n-1).ds00 * dens_pro00 )
; =========================================================


END
