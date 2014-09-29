; Get logg of b stars determined from Teff, mass (from evolution
; tracks) and Hipparcos parallaxes + BC from Bessell et al. 1998


plot_indiv_err = 0B ; plot error bars for each star?

 restore,'~/wire/wire_essential/wire_sec_info.idl'
 wire_ramirez, wireobj, wireobj2

readcol,'/home/bruntt/papers/wire/bstars/bmass.dat',$
 hd,feh,teff,bc,logg,mass,emass

n = n_elements(hd)
logg_pi = fltarr(n)
err_pi  = fltarr(n)

err_phot = fltarr(n)

err_teff = teff * 0.05 ; error on effective temperatures = 6% ?
err_v  = 0.02         ; error on V magnitude ?
err_bc = bc * 0.05    ; error on bolometric correction = 6% ?
emass = emass * 0.5

for i=0,n-1 do begin
 w = where(hd(i) eq wireobj2.hd,c)

 logg_pi(i) = 4. * alog10(teff(i)/5778.) + alog10(mass(i)/1.0) + $
              2. * alog10(wireobj2(w).par/1e3) + $
              0.4 * (wireobj2(w).v + bc(i) + 0.26) + 4.44

 err_pi(i) = (4. * (1./alog(10.)) * err_teff(i) / teff(i))^2. + $
                  ((1./alog(10.)) * emass(i)/mass(i))^2. + $
             (2. * (1./alog(10.)) * wireobj2(w).epar/wireobj2(w).par)^2. + $
             (0.4 * err_v)^2. + (0.4 * err_bc(i))^2. + (0.03)^2.
; variance of logg_pi (sqrt taken later ...) 

 err_phot(i) = wireobj2(w).elogg ; from wire_ramirez.pro

endfor

err_pi = sqrt(err_pi) ; error on logg_pi

offy = 0.03

plotsym,0,/fill
plot,logg_pi,logg,psym=2,xr=[2.5,4.5],yr=[2.5,4.5],/nodata,$
 xtit='!17log !3g!Iphot!N', ytit='!17log !3g!I!4p!3!N'
oplot,[2,5],[2,5],thick=2,color=200
oplot,logg,logg_pi,psym=8,symsi=1.2
for i=0,n-1 do xyouts,logg(i),logg_pi(i)+offy,$
 strcompress(string(hd(i),format='(I8)'),/remove_all),charsi=0.9,charthick=1.0,$
 alignment=0.5

if plot_indiv_err then begin
 for i=0,n-1 do $
  oplot,logg(i)+[-1,1.]*0.,logg_pi(i)+[-1.,1]*err_pi(i)

 for i=0,n-1 do $
  oplot,logg(i)+[-1,1.]*err_phot(i),logg_pi(i)+[-1.,1]*0.
endif


resistant_mean, err_phot, 3, typ_phot, sd,nr
resistant_mean, err_pi,   3, typ_pi,   sd,nr

oplot,4.+[-1.,1]*typ_phot, 3.0 + [-1,1.] * 0.,thick=2
oplot,4.+[-1.,1]*0., 3.0 + [-1,1.] * typ_pi,thick=2

print,' %%% Avg error on logg for phot/pi: ',typ_phot, typ_pi

; logg_pi = 4.*teff + mass + 2*log_pi + 0.4*(V+BC+0.26)+4.44

END
