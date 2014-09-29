; Bliver nodt til at fjerne eclipser, de giver ogsaa
; POWER i period04, perioden er 6.0664 days ---> 0.165 c/day + harmonics

if n_elements(period) eq 0 then begin

readcol,'/export/brixx1/bruntt/wire/wire_lc/wire_lc_ARCas.dat',t,d,w,format='d,d,d'
; readcol,'/export/brixx1/bruntt/wire/wire_lc/wire_lc_ARCas2.dat',t,d,w,format='d,d,d'

t0 = 2.76268745D
period = 6.06638929D
phase = ((t - t0) mod period)/period
w = where(phase lt 0.,c) & phase(w) = phase(w) + 1.0D
s = sort(phase)
phase2 = phase(s) & d2 = d(s) & time2 = t(s)
obs2 = bytarr(n_elements(s))

col=getcolor(/load)
plot,phase2,d2,psym=1,xr=[-.1,1.05],symsi=.3,yr=[.15,-.03]
x = where(phase2 lt .10 or abs(phase2-.6) lt .1 or phase2 gt .9)
; oplot,phase2(x),d2(x),psym=1,col=col.red
obs2(x) = 0B
xp = where(phase2 le .10 or phase2 ge .9,xp) ; primary eclipse
xs = where(abs(phase2-.6) le .1,cs) ; secondary eclipse
obs2(xp) = 1
obs2(xs) = 2
oplot,phase2(xs),d2(xs),psym=1,col=col.cyan
oplot,phase2(xp),d2(xp),psym=1,col=col.red

readcol,'/export/brixx1/bruntt/wire/wire_phased/ARCas_iter2.pha',ph,jkt
oplot,ph,jkt,col=col.yellow,thick=3

s2 = sort(time2)
n = n_elements(time2)
time3 = time2(s2) & data3 = d2(s2) & obs3 = obs2(s2)

fileo = '/export/brixx1/bruntt/wire/wire_lc/wire_lc_ARCas_OBS.dat'

openw,1,fileo
 for i=0L,n-1 do printf,1,time3(i),data3(i),obs3(i),format='(2D16.8,I3)'
close,1


; Export indv. lc: outside ecl, prim. ecl. + sec. eclipse (for power density normalisation)
for m=0,2 do begin
 wg = where(obs3 eq m,cg)
 fileindiv = fileo + '.' + string(m,format='(I1)')
  openw,1,fileindiv
     for i=0L,cg-1 do printf,1,time3(wg(i)),data3(wg(i)),obs3(wg(i)),format='(2D16.8,I3)'
  close,1
print,' %%% Wrote LC: ' + fileindiv
endfor


print,' %%% Exported file: ',fileo

;; /export/brixx1/bruntt/wire/wire_lc/wire_lc_ARCas_OBS.dat
;; Detected modes:
;; 
;; Iteration 4: fitter kun til data uden for eclipse -- men
;; der er reflektion i JKTs lyskurve.
;;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;      Freq (c/d)     Ampl [ppm]   Phase (0..1)
;;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;         0.02006         2363.5        0.00430
;;         0.61143         2303.6        0.29850
;;         0.83170         2434.3        0.17491
;;         0.49002         2176.5        0.09619
;;         1.79220         1011.1        0.27770
;;         0.53862          788.0        0.94726
;;         0.98742          910.2        0.84872
;;         0.89907          788.3        0.37702
;;         2.45912          466.6        0.24652
;; 
;; Iteration 5:
;;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;      Freq (c/d)     Ampl [ppm]   Phase (0..1)
;;  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;         0.02997         2256.7        0.02230
;;         0.61301         2460.0        0.30649
;;         0.83267         2282.4        0.21093
;;         0.48789         2172.7        0.08477
;;         1.79258          998.5        0.27812
;;         0.89770          881.9        0.37696
;;         0.54102          798.9        0.95823
;;         0.98880          848.6        0.83958
;;         2.45917          479.0        0.26262
;;         0.80979          716.6        0.92403


;; Noter: jeg traekker baggrunden fra, ved at dele tidsserien
;; op i seks stykker ---> times: 29 days / 6 = 4.83 days.
;; Alle perioder der er laengere end 5 dage vil blive 
;; slaaet ned! P = 4.83 svarer til f = 0.20 c/day.
;; Du finder modes i omraadet 0.6 til 2.5 c/day. Det
;; burde vaere ok. Med n = 4, P = 7.25 giver ca.
;; det samme decor resultat.
;; 
;; JKT solution: amplituder for reflektion i lyskurven
;; har amplitude paa ca. 0.005 = 5 mmag og maaske
;; to perioder... en fra phase .1 til .5 og .7 til .9.
;; Delta phase er altaa 0.4 og 0.2 for disse to
;; intervaller. Perioderne er nok det dobbelte af
;; disse dvs. print, 0.8 * 6.06, 0.4 * 6.06
;; Periods = 4.85 and 2.42 days
;; Frequencies: 0.21 and 0.41 c/day and anything
;; in between.

; Freq. fundet i P04: 
f = [.613,.832,.488,1.793, 0.898, .541, .989, 2.459, 0.810]
print,1./f,format='(9F9.2)'

; Perioder:
;     1.63     1.20     2.05     0.56     1.11     1.85     1.01     0.41     1.23

endif

easyps, keywords, kk, dops=dops, /prepare, dim = [20,14.0,-1,1], $
 fil = 'arcas_modes.ps', dir = '/export/brixx1/bruntt/wire_analysis/binary/arcas/'
   col=getcolor(/load)
   
   plot,phase2,d2,psym=1,xr=[-.1,1.05],symsi=.3,yr=[.15,-.03],tit='AR Cas (WIRE)',$
     xtit='Phase (P='+strcompress(string(period,format='(D8.5)'),/remove_all)+')',/nodata
   xyouts,.025,0.045,'Detected periods:'
   x = where(phase2 lt .10 or abs(phase2-.6) lt .1 or phase2 gt .9,comp=pp)
   oplot,phase2(x),d2(x),psym=1,symsi=.3,col=col.charcoal
   oplot,phase2(pp),d2(pp),psym=1,symsi=.3
   p = 1./f
   period=6.0663
   for i=0,8 do oplot,[0,p(i)/period],[.06,.06]+i*0.01,thick=3
   for i=0,8 do oplot,p(i)/period*[1.,1],[.06,.06]+i*0.01+[-1,1]*.001,thick=3
   for i=0,8 do oplot,[0.,0],[.06,.06]+i*0.01+[-1,1]*.001,thick=2
   for i=0,8 do xyouts,p(i)/period+0.005,.06+i*0.01+0.002,string(i+1,format='(I1)')+ $
     ' (' + strcompress(string(p(i),format='(F5.2)'),/remove_all)+'d)',charsi=1.0
   oplot,ph,jkt,col=col.red,thick=5
   oplot,[.6,.75],[1.,1]*.14,thick=2,col=col.red
   xyouts,.77,.142,'JKT solution',col=col.red

easyps, keywords, kk, dops=dops, /close

stop

; I importe file "fileo" to Period04, calc. the ampl spectra 0-300
; c/day at medium resolution. I will now compare the power density spectra!

readcol,'/export/brixx1/bruntt/wire/wire_lc/wire_lc_ARCas_OBS.dat',t,d,o,format='d,d,i'

readcol,'/export/brixx1/bruntt/wire_analysis/binary/arcas/spec_outside_eclipse.fou'

; You only want to run this once... takes a while
;fres = 7.7 ; c/day, low modes!
;fmax = 500. ; nyquist ~ 2763 c/day from period04
;highres = 1.5
;for m=0,2 do begin
; fileindiv = fileo + '.' + string(m,format='(I1)')
; wire_power_density_resolution, fileindiv, fres=fres, fmax=fmax, highres=highres
;endfor
; This is the result:
pnorm = [0.352035, 1.17643, 0.917737]

readcol,'

if n_elements(a1) eq 0 then begin
 readcol,'/export/brixx1/bruntt/wire_analysis/binary/arcas/spec_outside_eclipse.fou',f0,a0
 readcol,'/export/brixx1/bruntt/wire_analysis/binary/arcas/spec_primary_eclipse.fou',f1,a1
 readcol,'/export/brixx1/bruntt/wire_analysis/binary/arcas/spec_secondary_eclipse.fou',f2,a2
endif

fac = 1e6 / 1.086 ; mag -> ppm
sf = 51

easyps, keywords, kk, dops=dops, /prepare, dim = [20,14.0,-1,1], $
 fil = 'arcas_modes2.ps', dir = '/export/brixx1/bruntt/wire_analysis/binary/arcas/'
   col=getcolor(/load)


plot_oo,f0,((smooth(a0,sf,/edge)*fac)^2.)/pnorm(0),$
 xtit='Frequency [c/day]',ytit='Power density [ppm!E2!N/(c/day)]',tit='AR Cas (WIRE)',yr=[1e3,1e7],xr=[.1,100] ; outside eclipse
  oplot,f1,((smooth(a1,sf,/edge)*fac)^2.)/pnorm(1),col=col.sky ; during prim. eclipse
  oplot,f2,((smooth(a2,sf,/edge)*fac)^2.)/pnorm(2),col=col.red ; during sec. eclipse

fwire = 15.413
; Peaks seen in the frequency spe
sec = 5.1912255 ; sec.
pri = 6.3343098 ; prim.

plots,sec,10^!y.crange,col=col.sky,line=5
plots,pri,10^!y.crange,col=col.red,line=5
plots,fwire-sec,10^!y.crange,col=col.sky,line=5
plots,fwire-pri,10^!y.crange,col=col.red,line=5
plots,fwire+sec,10^!y.crange,col=col.sky,line=5
plots,fwire+pri,10^!y.crange,col=col.red,line=5

; Harmonics of orbital period:
; for k=1,1 do plots,k/period,10^!y.crange,col=col.green

nf = n_elements(f)
for k=0,nf-1 do plots,f(k),10^!y.crange,col=col.green

easyps, keywords, kk, dops=dops, /close

print,1/sec
print,1/pri


END
