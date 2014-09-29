; Perform autocorrelation for Procyon observations from WIRE

dops = 1
outps = '~/wire/wire_eps/alpha_cen_auto.ps'

; goto,pplot

damping_time = 2.5 ; 1.93 ; days
units = 'milliHz'
; WINDOW that is folded by your power spectrum:
center = 2.2 ; milli Hz
fwhm   = 1.5 ; milli Hz

if units eq 'milliHz' then factor = 1e-3

fwhm_damping = 1./(86400. * damping_time * factor)

print,' %%% Damping time set to: ' + $
 string(fwhm_damping,format='(F7.4)') + ' ' + units

cut_factor = -0.5
wset,0

col = getcolor(/load)

for date =0,1 do begin

case date of
0: restore,'/ai1/bruntt/wire/wire_process/wirep_procyon_alphaCen_1999_spectr.idl'
1: restore,'/ai1/bruntt/wire/wire_process/wirep_procyon_alphaCen_2004_spectr2.idl'
; 0: restore,'/ai1/bruntt/wire/wire_process/wirep_procyon_sept99.idl'
;1: restore,'/ai1/bruntt/wire/wire_process/wirep_procyon_sept00.idl
; /usr/users/bruntt/wire/wire_process/wirep_procyon_sept00.idl'
endcase

freq = wirep_struct.sp.freq(0,*)
amp = wirep_struct.sp.amp(0,*)
w = where(freq ge 0.001 and freq le 500.0,c)
freq = freq(w) & amp = amp(w)

; Uniformly distrib. frequencies:
f1 = min(freq) & f2 = max(freq) & ra = f2-f1
nf = n_elements(freq)
freq2 = ra * (findgen(nf)/nf) + f1
amp2a = interpol(amp,freq,freq2)

; Construct a gaussian with a certain width
gauss_fwhm = fwhm_damping & gauss_sigma = gauss_fwhm / 2.35
df = median(freq2(1:nf-1) - freq2(0:nf-2))
ng = ceil(10. * round(6. * gauss_fwhm / df)) / 10.
gg = fltarr(2,ng)
gg(0,*) = findgen(ng) * df & x0 = gg(0,round(ng*0.5)-1)
gg(1,*) = exp(- ( (gg(0,*)-x0)^2.0) / (2. * (gauss_sigma)^2.0 ) )
gg(1,*) = gg(1,*) / total(gg(1,*))
; plot,gg(0,*),gg(1,*) ; Debug 
; plots,x0 + gauss_fwhm * [-1,1] * 0.5, max(gg(1,*)) * [1,1] * 0.5
amp2 = convol(amp2a, reform(gg(1,*)), /edge_truncate)

; The smoothed amplitude spetrum:
;plot,freq2,amp2a
;oplot,freq2,amp2,col=col.red
;oplot,gg(0,*)+1.0,gg(1,*) * (max(amp2a))/max(gg(1,*)),col=col.sky


n = 10. & 
sigma    = fwhm/( 2 * (ALOG(2))^(1/n) )
window   = EXP(-0.5*((freq2-center)/sigma)^n)
spec2    = amp2 * amp2 * window

; Set a reasonable cut off level
mm = median(spec2) & rr = robust_sigma(spec2)
cut_level = mm + rr * cut_factor
wlow = where(spec2 lt cut_level,clow)
if clow ge 1 then spec2(wlow) = 0.0 ; cut off level


if date eq 0 then orb_freq = 15. * 1e3 / 86400. ; orbital freq in mHz
if date eq 1 then orb_freq = 15.34  * 1e3 / 86400.
;orb_freq = 0.174660
killorb = spec2 & killorb(*) = 1.0
for i=0,30 do begin
 wid_fwhm = 0.012 & widr = wid_fwhm / 2.35
 wid = 2. * (widr)^2.0
 kill = (exp(-(freq2 - i * orb_freq)^2.0 / wid))
 killorb = killorb - kill
endfor

spec2 = spec2 * killorb

; --------------------------

plot,freq2,amp2
oplot,freq2,window * max(amp),line=2,thick=3
; print,' %%% Hit any key!' & s = get_kbrd(1)
wait, 2.0
plot,freq2,spec2
plots,!x.crange,cut_level

nee = n_elements(freq2)
dfreq = median(freq2(1:nee-1) - freq2(0:nee-2))
lag = findgen(5000)/1.5
 auto2 = a_correlate(spec2, lag)
;auto2 = a_correlate(amp2, lag)
plot,lag*dfreq,auto2
orb = 15.08 * 1000. / 86400.
for i=0,100 do plots,i * orb,!y.crange,col=col.sky,thick=2

;sep = 0.0807 & for i=0,30 do plots,sep*i, !y.crange,line=2,col=col.red
;sep = 0.0587 & for i=0,30 do plots,sep*i, !y.crange,line=2,col=col.sky
;sep = 0.0929 & for i=0,30 do plots,sep*i, !y.crange,line=2,col=col.sky

; Store ...

case date of 
0: begin
 freq99 = lag * dfreq
 auto99 = auto2

endcase
1: begin
 freq00 = lag * dfreq
 auto00 = auto2
endcase

endcase

endfor


pplot:

m99 = median(auto99(0:1500))
m00 = median(auto00(0:1500))


colx = [col.white, col.cyan, col.red, col.sky, col.magenta, col.yellow]
if dops ge 1 then begin
 aa4,x=18,y=12,t=0,name=outps
 colx = replicate( col.black, 6)
endif

plot,freq99,auto99/m99,xr=[0,0.3],xtit='mHz',ytit='Auto',$
 yr=[.7,1.5],xsty=1,ysty=1
 oplot,freq00,auto00/m00 + 0.2,col=colx(1),thick=3

xyouts,0.03,1.4,'Alpha Centauti A - 1999'
xyouts,0.03,1.3,'Alpha Centauti A - 2004'
xyouts,0.03,1.2,'Cut = '+string(cut_factor,format='(F4.1)') + ' !4r!3'

if dops eq 0 then begin
 print,' %%% Hit any key!' & s = get_kbrd(1)


; sep2 = 0.093 & for i=0,30 do plots,sep2*i, !y.crange,line=2,col=colx(3)
; plots,sep2 * 0.5, !y.crange,col=colx(3),thick=2,line=3

endif

sep1 = 107.0*1e-3 & for i=0,30 do plots,sep1*i, !y.crange,line=2,col=colx(2)
plots,sep1 * 0.5, !y.crange,col=colx(2),thick=2,line=3

hansk = 104e-6 * 1000.
plots,hansk,!y.crange,col=colx(3),thick=2

if dops ge 1 then begin
 device,/close
 set_plot,'x'
 print,' $ggv  ' + outps + ' &  '
endif


END
