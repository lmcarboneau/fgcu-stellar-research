np = 4 ; use the four nearest LC points --> apply resistant mean of ptp scatter
; np = 2 ; use neighbour LC points only 

addyear = '1999'
; addyear = '2000'

addyear = 'sim'

if addyear eq 'sim' then readcol,$
 '/home/bruntt/wire/procyon/simul/peakdist2000/it010_wire_lc_Procyon_2000_s0_HD61421_F5IV-V_sublowgt750.0_aG0.70_aP14.0_dT1.00_WN99.0.dat',t,d,wei,format='D,D,D' else $
 readcol,$
  '~/wire/procyon/power/wire_lc_Procyon_'+addyear+'_s0_HD61421_F5IV-V_sublow.dat',$
  t,d,wei,$
  format='D,D,D'

s = sort(t)
t = t(s) & d = d(s) & wei = wei(s)
g = uniq(t)
t = t(g) & d = d(g) & wei = wei(g)

 if n_elements(g) ne n_elements(t) then stop

n = n_elements(t)

dt = median(t(1:n-1) - t(0:n-2))

plot,t,d,psym=3

m = max(wei)
w = where(wei/m lt max(wei/m) * 0.95,c)
if c ge 2 then oplot,t(w),d(w),psym=6,symsi=.5

rms = robust_sigma(d)
scat = fltarr(n)


if np eq 2 then begin
 for i=1,n-2 do begin
  scat(i) = (abs(d(i)-d(i-1)) + abs(d(i)-d(i+1))) * 0.5
 endfor
; End points in LC
 scat(0) = scat(1)
 scat(n-1) = scat(n-2)
endif

if np eq 4 then begin
 for i=2,n-3 do begin
  x = [d(i)-d(i-2),d(i)-d(i-1),d(i)-d(i+1),d(i)-d(i+2)]
  deltat = [t(i)-t(i-2),t(i)-t(i-1),t(i)-t(i+1),t(i)-t(i+2)]
  wt = where(abs(deltat) lt dt * 3.,ct)

;  help,ct

;  if ct ge 2 then $
;  resistant_mean, abs(x(wt)), 3, me, sd, nr else $
;   me = -1.
;  scat(i) = me

  if ct ge 2 then $
   me = avg(abs(x(wt))) else $
   me = -1.
  scat(i) = me


 endfor
; End points in LC
 scat(0:1) = avg(scat(2:5))
 scat(n-2:n-1) = avg(scat(n-5:n-3))
endif



w2 = where(scat lt -.5,c2)
w3 = where(scat gt 0.,c3)
if c2 ge 2 then scat(w2) = interpol(scat(w3),t(w3),t(w2))


scat2 = sqrt( scat/(sqrt(np-1.))^2. + (rms*0.3)^2.)
; scat2 = scat2 / avg(scat2)

wts = 1./scat2^2.

!P.multi=[0,1,2]

wts2 = ddsmooth(wts,stiff=50)
wts3 = wts2 / total(wts2)

; Hvad svarer ddsmooth til med normal smooth:
plot,wts2
oplot,smooth(wts,150,/edge),col=col.red
oplot,smooth(wts,200,/edge),col=col.sky

stop

plot_io,t,wts,psym=1,symsi=.7 ; ,xr=[-.8,-.5]*3.
oplot,t,wts2,col=col.sky

plot,t,d,psym=1,symsi=.7,yr=[-1,1]*.6*1e-3,xr=[0,1]-4.
oplot,t,((wts2/avg(wts2))-1.)*1e-3,col=col.sky

!P.multi=0

plot,d*1e3,wts3,psym=3,xr=[-1,1]*0.6,yr=[0,1.5e-4]*2.

if addyear eq '1999' then begin
 w2 = where(t gt .15 and t lt .2,c) & help,c
 w  = where(t gt .45 and t lt .55,c) & help,c
endif

if addyear eq '2000' or addyear eq 'sim' then begin
 w2 = where(t gt -3.3 and t lt -3.22,c) & help,c
 w  = where(t gt -3.68 and t lt -3.62,c) & help,c
endif

; plot,t,d,psym=3 & oplot,t(w),d(w),psym=4,col=col.sky & oplot,t(w2),d(w2),psym=6,col=col.red

oplot,d(w2)*1e3,wts3(w2),psym=4,col=col.red
oplot, d(w)*1e3, wts3(w),psym=4,col=col.sky

xyouts,.3,avg(wts3(w2)),$
 strcompress(string(robust_sigma(d(w2))*1e3,format='(F9.3)'),/remove_all)  + $
 ' / ' + $
 strcompress(string(avg(wts3(w2))*1e6,format='(F9.3)'),/remove_all),col=col.red

xyouts,.3,avg(wts3(w)),$
 strcompress(string(robust_sigma(d(w))*1e3,format='(F9.3)'),/remove_all)  + $
 ' / ' + $
 strcompress(string(avg(wts3(w))*1e6,format='(F9.3)'),/remove_all),col=col.sky

print,'Scat2: ', avg(scat2(w)), avg(scat2(w2))


if addyear eq '1999' or addyear eq '2000' then begin

openw,1,$
 '~/wire/procyon/power/wire_lc_Procyon_'+addyear+'_s0_HD61421_F5IV-V_sublow_scatwei.dat'
for i=0,n_elements(wts3)-1 do $
 printf,1,t(i),d(i),wts3(i),format='(D15.7,D15.7,D15.7)'
close,1

openw,1,$
 '~/wire/procyon/power/wire_lc_Procyon_'+addyear+'_s0_HD61421_F5IV-V_sublow_equalwei.dat'
for i=0,n_elements(wts3)-1 do $
 printf,1,t(i),d(i),1./n_elements(wts3),format='(D15.7,D15.7,D15.7)'
close,1

endif else begin

openw,1,$
 '~/wire/procyon/power/Procyon_2000_s0_HD61421_F5IV-V_sublowgt750.0_aG0.70_aP14.0_dT1.00_WN99_scatwei.dat'
for i=0,n_elements(wts3)-1 do $
 printf,1,t(i),d(i),wts3(i),format='(D15.7,D15.7,D15.7)'
close,1

openw,1,$
 '~/wire/procyon/power/Procyon_2000_s0_HD61421_F5IV-V_sublowgt750.0_aG0.70_aP14.0_dT1.00_WN99_equalwei.dat'
for i=0,n_elements(wts3)-1 do $
 printf,1,t(i),d(i),1./n_elements(wts3),format='(D15.7,D15.7,D15.7)'
close,1

wire_stetson, d, wei2, 0 ; Stetson Weights

openw,1,$
 '~/wire/procyon/power/Procyon_2000_s0_HD61421_F5IV-V_sublowgt750.0_aG0.70_aP14.0_dT1.00_WN99_stetson.dat'
for i=0,n_elements(wts3)-1 do $
 printf,1,t(i),d(i),wei2(i),format='(D15.7,D15.7,D15.7)'
close,1


endelse



end
