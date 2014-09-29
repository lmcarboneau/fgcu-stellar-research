; Plot ampl. spectra as they are progressively cleaned:
; Run this after: 
;; .r wire_carrier_procyon.pro
;; tuse = t-t0 & ruse = r - total(weights*r) & wuse = weights
;; wire_clean, tuse, ruse, wuse, 1., 180., 25., fc, dc

siglimit = 4.0

f1 = 1. ; freq range in micro Hz
f2 = 2000. 
yy = 0.7 ; y-range max. for power spectrum

namp = n_elements(fc) ; number of ampl spectra


; Remove the (nn) = 30 most significant peaks and calc. noise in
; a sliding box. This is noise in the POWER SPECTRUM!
nsec = 50. & nn = 5.
wire_calc_ampl_noise, fc(nn).freq*fac*fac2, (fc(nn).amp/1e6)^2.0, $
 f1, f2, nsec, noise

; Calculate significant level of each detected peak ...
noise2 = noise & noise2(1,*) = smooth(noise(1,*),4) 
noise_at_freq = interpol(noise2(1,*),noise2(0,*),fc.f )
sigma_at_freq = (fc.a^2.0) / noise_at_freq ; ampl in power / noise in power

plot,fc(0).freq*fac*fac2,(fc(0).amp/1e6)^2.0,ysty=1,xr=[f1,f2],xsty=1,yr=[0,yy]
oplot,noise2(0,*),noise2(1,*)*siglimit,col=col.red,psym=-6
oplot,fc(nn).freq*fac*fac2,(fc(nn).amp/1e6)^2.0,col=col.red,line=3
oplot,fc.f,noise_at_freq*siglimit,psym=2,col=col.sky,symsi=3
oplot,fc.f,fc.a^2.0,psym=6,col=col.cyan,symsi=2,thick=2

; Pick only freqs > 400 microHz and the modes that are significant!
wp = where(fc.f gt 0. and sigma_at_freq gt siglimit,cp)
fr = findgen(cp) + 1. ; ID = name of peak

for i=0,cp-1 do $
 plots,fc(wp(i)).f * [1.,1], [fc(wp(i)).a, yy], line=2,col=col.sky

print,'Freq. [microHz]','Ampl [m^2/s^2]','S/N',$
  format='(A20, A20, A6)'
for i=0,cp-1 do $
 print,fc(wp(i)).f, fc(wp(i)).a, sigma_at_freq(wp(i)), $
  format='(F20.2, F20.3, F6.1)'



; ==========================================================================

f1a = 0.
f2a = 2000.

for i=0,cp-1 do begin

plot,fc(wp(i)).freq*fac*fac2,(fc(wp(i)).amp/1e6)^2.0,ysty=1,xr=[f1a,f2a],xsty=1,yr=[0,yy]
for j=0,cp-1-1 do $
 oplot,fc(wp(j)).f*[1.,1], [yy*.4,yy],col=col.sky,thick=1,line=2

oplot,fc(wp(i)).f*[1.,1], [yy*.4,yy],col=col.sky,thick=2

for j=0,cp-1 do $
 xyouts,fc(wp(j)).f+5., yy,col=col.sky,orientation=270, charsi=1.0, $
 strcompress(string(fr(j)),/remove_all) + ' ' + $
 strcompress(string(fc(wp(j)).f,format='(F6.1)'),/remove_all)

oplot,noise(0,*),smooth(noise(1,*)*siglimit,5),col=col.red,psym=-6

wait, 0.7

endfor


end
