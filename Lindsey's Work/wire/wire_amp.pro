; (c) Sept. Hans Bruntt
; Program will calc. amplitude spectra and produce nice .ps plots
; $mv /ai39/bruntt/wire/altair/altair_plot.ps /ai39/bruntt/wire/altair/altair_plot_1101_5.ps
; $mv /ai39/bruntt/wire/altair/altair_plot_1101_*.ps /ai39/bruntt/wire/altair/altair_plot_1101_15.ps

dops = 1
only_weights = 0
only_plot = 0

if only_plot eq 1 then goto,ddps

; -------------------------------------------------------------------
;   minfreq = 0.05 & maxfreq = 50.  ; freq. range in c/day
   minfreq = 3.     & maxfreq  = 20.      ; freq. range in c/day
;   minfreq = 3.     & maxfreq  = 50.      ; freq. range in c/day
;   minfreq = 0.05     & maxfreq  = 450./fac ; freq. range in c/day
; -------------------------------------------------------------------

; -------------------------------------------------------------------
markfreq = -1
markfreq = [15.009]
; -------------------------------------------------------------------
pmax = 15000L ; max number of points in ampl. spectrum
dmax = 50000L ; max number of data points
; -------------------------------------------------------------------

; -------------------------------------------------------------------

; -------------------------------------------------------------------
remove_spline_fit_to_scattered_light = 0
remove_scattered_light_data = 1
data_rel_to_main_target = 0 ; always == 0 ? ---> not fully implem.
avoid_xyoff = 1 ; avoid data for the first 5 days/nights == sign. telescope offsets!
remove_offsets = 100.0
; -------------------------------------------------------------------

minhjd = -100.0
if avoid_xyoff eq 1 then minhjd = -5.0
maxhjd = 14.0 ; bad data after HJD = 14.0

fit_gaussian = 0
wire_per = fltarr(5)

; -------------------------------------------------------------------
; Construct output file names ... restore file is it exists!
; -------------------------------------------------------------------
suff = string(remove_spline_fit_to_scattered_light,format='(I1)') + $
 string(remove_scattered_light_data,format='(I1)') + $
 string(data_rel_to_main_target,format='(I1)') + $
 string(avoid_xyoff,format='(I1)') + '_' + $
 strcompress(string(remove_offsets,format='(I3)'),/remove_all) + '_f' + $
 strcompress(string(minfreq,format='(I3)'),/remove_all) + '-' + $
 strcompress(string(maxfreq,format='(I3)'),/remove_all)

ddps:
 dir = '/ai39/bruntt/wire/altair/'
 outfile = dir + 'altair_amp_'+suff+'.ps'
 outfile_dat = dir + 'altair_amp_'+suff+'.dat'
; Has this computation already been made? If so, restore it!
  reset_wireamp = 0
 spawn,'ls -1 '+outfile_dat,aa
 if aa(0) ne '' then begin
    print,' %%% Restoring previous fft computation: '+outfile_dat
    restore,outfile_dat
    only_plot = 1
 endif else begin
  reset_wireamp = 1
 endelse

; -------------------------------------------------------------------
if dops eq 1 then begin
 a4,y=26,x=19,name=outfile
 colx = 90
endif

; -------------------------------------------------------------------
if n_elements(wire4) eq 0 then $
 restore,'/ai39/bruntt/wire/altair/altair_merged_allslots_swap.idl'
nstar = n_elements(wire4(0).mag)
t0 = 51480.0D ; round(median(wire4.hjd))

; -------------------------------------------------------------------
!P.multi=0
!P.charsize=1.1
xx = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
; -------------------------------------------------------------------

; -------------------------------------------------------------------
inf = strarr(2,5) ; info from wire_ident.pro
inf(*,0) = ['!18ALTAIR!3'   ,'!18A7V!3']
inf(*,1) = ['!4s!3 Aql'      ,'!18K0III!3']
inf(*,2) = ['!18NSV-12557!3','!18G8IV!3']
inf(*,3) = ['!18HD189695!3' ,'!18K5III!3']
inf(*,4) = ['!18TARAZED!3'  ,'!18K3II!3']
inf2 = ['Altair','Tau_Aql','NSV_12557','HD189695','Tarazed']

; print,wire_per,format='(5F10.5)'
wire_per = [ 173.86192, 173.46840, 174.21091, 173.53639, 174.00027]
wire_per(*) = 173.71527

; Fixes y-range for each star?
wire_y_ran = [800.,7000,2500,12000,4000]
; wire_y_ran(*) = 2000 & wire_y_ran(0) = 1000
wire_y_ran = [750.,4500,1500,4500,3000] ; * 2.
wire_y_ran = [750.,6000,2500,6000,4000] ; * 2.
; wire_y_ran(*) = 400
; -------------------------------------------------------------------

; -------------------------------------------------------------------
if only_plot eq 1 and only_weights eq 1 then goto,dopl
if only_plot eq 1 and only_weights eq 0 then goto,doplpl
; -------------------------------------------------------------------



; -------------------------------------------------------------------
if only_weights eq 1 then goto,go_weights
; -------------------------------------------------------------------

; -------------------------------------------------------------------
doplpl:
print,'plotting...'
getpos,3,5,0.02,0.0,0,pos
plot,[0,1],/nodata,xsty=6,ysty=6
dd = 0.5 ; fraction of day to plot (2 times this range in hjd)
cnt = 0

wday = where(wire4.hjd gt -100.,c)
minday = round(min(wire4(wday).hjd-t0))
maxday = round(max(wire4(wday).hjd-t0))

minday = -4
maxday = 4

print,' %%% From day '+strcompress(minday)+' to day ' + strcompress(maxday)

; -------------------------------------------------------------------

if n_elements(wireamp) eq 0 then goto,dopl
; -------------------------------------------------------------------
for day = minday,maxday,3 do begin
print,day,format='(I3,$)'

for i=0,nstar-1 do begin

dat = wireamp(i).dat & tt  = wireamp(i).tt & wei = wireamp(i).wei
 w9 = where(tt gt -100 and tt lt 14.,c9) 
 nn = robust_sigma(dat(w9)) & mdat = median(dat(w9))
 w9_2 =  where(tt gt -100. and tt lt 14. and abs(dat-mdat) lt 10.*nn,c) 
 w9 = w9(w9_2)

 dat = dat(w9) & tt = tt(w9) & wei = wei(w9)
 mmag = median(dat)
 nn = robust_sigma(dat)

; wg = where(wire4.mag(i) gt 0. and wire4.mag(i) lt 20.,c)
; mmag = median(wire4(wg).mag(i))
; nn = robust_sigma(wire4(wg).mag(i))

; wg2 = where((wire4.mag(i)-mmag) lt 20.*nn,cg2)
; wg = wg(wg2)

; Debug:
; plot,wire4(wg).hjd,wire4(wg).mag(0),psym=3,yr=[-1,1]*20.*nn+mmag,ysty=3
; plots,!x.crange,20.*nn+mmag,line=2,col=col.green
; plots,!x.crange,-20.*nn+mmag,line=2,col=col.green

 xxo = xx & xxt = '' & yyt = '' & yyo = ''
 if (i+1) mod 5 eq 0 then begin
   xxo = '' & xxt = '!4D!3 HJD' & yyt = 'mag'
 endif
 if cnt ge 5 then yyo = xx

; plot,wire4(wg).hjd-t0,wire4(wg).mag(i),yr=[-1,1]* nn * 5. + mmag,$
;  psym=1,symsi=.1,xr=[-1,1]*dd+day,position=pos(*,cnt),/noerase,xtickname=xxo,$
;  xtit=xxt,ytickname=yyo
 plot,tt,dat,yr=[-1,1]* nn * 4.5 + mmag,$
  psym=1,symsi=.1,xr=[-1,1]*dd+day,position=pos(*,cnt),/noerase,xtickname=xxo,$
  xtit=xxt,ytickname=yyo

;per2 = 15.7668893 & amp2 = 0.00048516  & pha2 = 0.3907 
;per3 = 20.7854131 & amp3 = 0.0004099   & pha3 = 0.3032
;per4 = 14.9730198 & amp4 = 0.000346836 & pha4 = 0.42921
;per5 = 10.9480321 & amp5 = 0.000236077 & pha5 = 0.818465
;per6 = 15.9839497 & amp6 = 0.0002146   & pha6 = 0.708431

;if i eq 0 then begin
   readcol,'/ai39/bruntt/wire/altair/altair4.per',dummy,p98f,p98a,p98p,format='A,D,D,D'
   nfreq = n_elements(p98f) & np = 8000
   tart = min(tt) + (max(tt)-min(tt)) * (findgen(np)/(np-1.))
   dart = fltarr(np)
   for pp=0, nfreq-1 do $
    dart = dart + p98a(pp) * sin(2. * !DPI * p98f(pp) * tart + p98p(pp) * (2. * !DPI)  ) 
    oplot,tart,dart ; only for altair
;endif

 plots,!x.crange,mmag, line = 2
 plots,!x.crange,mmag + 3. * nn, line = 1
 plots,!x.crange,mmag - 3. * nn, line = 1

 if cnt le 4 then $
 xyouts,-dd+dd*0.2+day,mmag + nn * 3.25,'!18'+inf(0,i)+' (' + inf(1,i) + ')!3',charsi=1.1

 if cnt eq 14 then $
  xyouts,-dd+dd*0.2+day,mmag - nn * 4.0,'!3HJD!L0!N = '+string(t0,format='(I5)'),charsi=1.1

 cnt = cnt + 1
 if cnt eq 15 then begin
  plot,[0,1],/nodata,xsty=6,ysty=6
  cnt = 0
 endif

endfor ; next star
endfor ; next day
; -------------------------------------------------------------------
if only_plot eq 1 then goto,dopl

; -------------------------------------------------------------------
go_weights:
; -------------------------------------------------------------------


dopl:
; -------------------------------------------------------------------
if n_elements(wireamp) eq 0 or reset_wireamp eq 1 then begin
 wireamp = replicate({f:fltarr(pmax),a:fltarr(pmax),p:fltarr(pmax), $
                      dat:fltarr(dmax), tt:fltarr(dmax), $
                      wei:fltarr(dmax), gc:fltarr(dmax)},nstar)
 for k=0,nstar-1 do wireamp(k).dat(*) = -99.9
endif

; -------------------------------------------------------------------

; -------------------------------------------------------------------
cnt = 0
cx_max = 0L & nf_max = 0L
fac = 1e6/86400.
getpos,1,5,0.0,0.0,0,pos
plot,[0,1],/nodata,xsty=6,ysty=6 ; erase window
; -------------------------------------------------------------------

; -------------------------------------------------------------------
print,' %%% Ampl. spectra for '+strcompress(nstar)+' stars ...'
for i=0,nstar-1 do begin
 print,i,format='(I3,$)'

if only_plot eq 1 then goto,dopl2

 wx = where(wire4.mag(i) gt 0. and wire4.mag(i) lt 20. and $
           (wire4.hjd-t0) gt (minhjd) and (wire4.hjd-t0) lt maxhjd,c) 
; exclude data where,xy-offsets were made?
 noise = robust_sigma(wire4(wx).mag(i))
 mmag = median(wire4(wx).mag(i))
 wx2 = where((wire4.mag(i)-mmag) lt 10.*noise,cg2)
 wx = wx(wx2) ; Remove 10 sigma outliers!

   dat  = wire4(wx).mag(i)
   tt   = wire4(wx).hjd - t0
   gc   = wire4(wx).gc
   if (data_rel_to_main_target eq 1) and (i ne 0) then $
      dat = dat - wire4(wx).mag(0)
   resistant_mean,dat,3,me,sd,nr
   dat = dat - me
   
; -------------------------------------------------------------------
   per1 = wire_per(i) / fac ; period for wire craft
; -------------------------------------------------------------------

; -------------------------------------------------------------------
if remove_spline_fit_to_scattered_light eq 1 then begin
; Spline fit to the known scattered light

   wire_spline,tt, dat, 1./per1, phasex, dat2x, dat3x
                                ; plot,phase,dat,psym=3,yr=[-1,1]*0.4
                                ; oplot,phase,dat2,col=col.red,psym=1
                                ; oplot,phase,dat3,col=col.green,psym=3
 dat = dat3x - median(dat3x)
 print,' %%% Spline fit removed!'
endif
; -------------------------------------------------------------------


; -------------------------------------------------------------------
if remove_scattered_light_data eq 1 then begin
   pha = (tt mod (1./per1)) * per1
   wx = where( (pha ge -.51 and pha lt -0.22) or $
               (pha ge  .495 and pha lt  0.79),cx) ; found using wire_phase.pro

    ; plot,pha,dat,psym=3,yr=[-1,1]*0.01
    ; oplot,pha(wx),dat(wx),psym=3,col=col.red
   
   dat = dat(wx) - median(dat(wx))
   tt = tt(wx)
   gc = gc(wx)

   if n_elements(dat3x) ne 0 then begin
    phasex = phasex(wx)
    dat2x = dat2x(wx)
    dat3x = dat3x(wx)
   endif
   print,' %%% Scattered light data removed!'
endif

if remove_offsets gt 0.0 then begin
 wire_remove_offsets,tt,dat,datx,remove_offsets ; remove offsets?
  if i eq 0 then stop ; HK stop
 dat = datx - median(datx)
 print,' %%% Offsets removed: '+strcompress(remove_offsets)
endif

cx = n_elements(dat)
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; Calculate Stetson Weights!
   resistant_mean,dat,3,me,sd,nr
   fivesigma = 5. * noise
   astet = 0.7 & bstet = 8.0 ; Stetson outlier weights !
   fudge_weight = (1. + ((dat-me)/(astet*fivesigma))^bstet)^(-1.)
   wei = fudge_weight / total(fudge_weight)
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; New copy: ampl_spec will change the data == dat2, tt2, wei2 + min/maxfreq!
   dat2 = dat  &   tt2 = tt  &   wei2 = wei
   minfreq2 = minfreq & maxfreq2 = maxfreq  ; freq. range in c/day
; -------------------------------------------------------------------


; -------------------------------------------------------------------
; Calcultate the ampl. spectrum --- using weights!
    ampl_spec_calc_wire,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase

wok = where(strcompress(string(amp),/remove_all) ne 'NaN') ; Remove NaN!
amp = amp(wok) & freq = freq(wok) & phase = phase(wok)

; -------------------------------------------------------------------
dopl2:
if only_plot eq 1 then begin
      freq  = wireamp(i).f
      amp   = wireamp(i).a    
      phase = wireamp(i).p
      dat   = wireamp(i).dat ; to avoid deletion of data!
      tt    = wireamp(i).tt   
      wei   = wireamp(i).wei
      gc    = wireamp(i).gc
      w = where(freq gt 0.,c) & freq = freq(w) & amp = amp(w) & phase = phase(w)
endif
; -------------------------------------------------------------------

; -------------------------------------------------------------------
   xxo = xx & xxt = '' & yyt = '' & yyo = '' & titl = '!3'
   if i eq 0 then titl='!18'+suff+'!3'
   if (i+1) mod 5 eq 0 then begin
    xxo = '' & xxt = '!4m!3 [!4l!3Hz]' & yyt = 'Ampl [ppm]'
   endif
   ; if cnt ge 5 then yyo = xx
; -------------------------------------------------------------------

; -------------------------------------------------------------------
; Plot the result
    ra = max(freq) - min(freq) & ra1 = min(freq) ; plotting range x
    rb = max(amp)  - min(amp)  & rb1 = min(amp)  ; plotting range y
    plot,freq*fac,amp,$
     xtit=xxt,position=pos(*,cnt),/noerase,xtickname=xxo,ytit=yyt,$
      yr=[0,wire_y_ran(i)], xr=[minfreq,maxfreq]*fac,xsty=1,ysty=1,$
      tit=titl

; -------------------------------------------------------------------
; Mark frequencies!
; -------------------------------------------------------------------
    for kk=0,n_elements(markfreq)-1 do begin
       wmm = where(abs(markfreq(kk)-freq) lt 0.2,c_wmm)
       if c_wmm ge 2 and markfreq(kk) gt 0. then begin
        mi_y = max(amp(wmm)) & mx_y = wire_y_ran(i)
        if mi_y gt mx_y then mi_y = mx_y
        plots,[1.,1.]*markfreq(kk)*fac,[mi_y,mx_y],line=2
       endif
    endfor

;    if i eq 0 then begin
     for kk=0,n_elements(p98f)-1 do begin
       wmm = where(abs(p98f(kk)-freq) lt 0.2,c_wmm)
       if c_wmm ge 2 and p98f(kk) gt 0. then begin
        border_y = rb * 0.15
        border_x = ra * 0.06
        mi_y = max(amp(wmm)) + border_y & mx_y = wire_y_ran(i)
        if mi_y gt mx_y then mi_y = mx_y
        plots,[1.,1.]*p98f(kk)*fac,[mi_y,mx_y],line=1
        arrow,p98f(kk)*fac,mx_y,p98f(kk)*fac,mi_y,/data,thick=2
        xyouts,orientation=270,charsize=0.7,$
          p98f(kk)*fac+border_x,mx_y-border_y*1.0,$
          strcompress(string(p98f(kk),format='(F9.1)'),/remove_all),alignment=0.0
       endif
     endfor
;    endif
; -------------------------------------------------------------------


 ;    yr=[0,(rb+rb1)+0.1 * rb], xr=[minfreq,maxfreq]*fac,xsty=1,ysty=1

 ; star name + spec. type
    xyouts,(ra1 + ra * 0.7)*fac,wire_y_ran(i)*0.85,$
     '!18'+inf(0,i)+' (' + inf(1,i) + ')!3',charsi=1.1

; Estimate noise level above 250 microHz
 wnoise = where(freq*fac gt 0.80 * max(freq*fac),cnoise)
 if cnoise ge 50 then begin
  resistant_mean,amp(wnoise),3,ppm,sd,nr
   addnote = '' & if i eq 0 then addnote = '!3 (>80%)'
   xyouts,(ra1 + ra * 0.7)*fac,wire_y_ran(i)*0.7,$
     '!18'+strcompress(string(ppm,format='(F9.1)'),/remove_all)+' ppm!3'+addnote,$
     charsi=1.1
 endif


; Fit gaussian
if fit_gaussian eq 1 then begin
 wgauss = where(abs(freq-per1) lt 0.2,cgauss)
 g = gaussfit(freq(wgauss)*fac,amp(wgauss),ter,nterms=4)
; debug:
; plot,freq*fac,amp,xr=([-1,1]*0.5+per1)*fac
; oplot,freq(wgauss)*fac,amp(wgauss),psym=2
; oplot,freq(wgauss)*fac,g,col=col.red
 wire_per(i) = ter(1) ; fitted freq. of wire orbit
 plots,ter(1),!y.crange,line=2,thick=3
endif

; -------------------------------------------------------------------

if only_plot eq 1 then goto,skip_data_handling
; -------------------------------------------------------------------
     nf = n_elements(freq)
     if nf gt pmax then begin
         print,' *** WARNING: Structure wireamp does not have enough entries!'
         help,nf,pmax,wireamp
         nf = pmax 
     endif
     if nf gt nf_max then nf_max = nf
; -------------------------------------------------------------------

      wireamp(i).f(0:nf-1) = freq(0:nf-1)
      wireamp(i).a(0:nf-1) = amp(0:nf-1)
      wireamp(i).p(0:nf-1) = phase(0:nf-1)

; -------------------------------------------------------------------
     if cx gt dmax then begin
         print,' *** WARNING: Structure wireamp does not have enough entries!'
         help,cx,dmax,wireamp
         cx = dmax 
     endif
; -------------------------------------------------------------------
 
      wireamp(i).dat(0:cx-1) = dat(0:cx-1)
      wireamp(i).tt(0:cx-1)  = tt(0:cx-1)
      wireamp(i).wei(0:cx-1) = wei(0:cx-1)
      wireamp(i).gc(0:cx-1)  = gc(0:cx-1)
      if cx gt cx_max then cx_max = cx
; -------------------------------------------------------------------
skip_data_handling:

;   plot,dat,wei,psym=3,xr=[-1,1]*noise*10.,position=pos(*,cnt),/noerase
   cnt = cnt + 1

endfor
; -------------------------------------------------------------------

if only_plot eq 1 then goto,skip_save
; Remove unused entries!
; -------------------------------------------------------------------
wireamp2 = replicate({f:fltarr(nf_max),a:fltarr(nf_max),p:fltarr(nf_max), $
                     dat:fltarr(cx_max),  tt:fltarr(cx_max), $
                     wei:fltarr(cx_max), gc:fltarr(cx_max)},nstar)
for i=0,nstar-1 do begin
 wireamp2(i).f   = wireamp(i).f(0:nf_max-1) 
 wireamp2(i).a   = wireamp(i).a(0:nf_max-1)
 wireamp2(i).p   = wireamp(i).p(0:nf_max-1)
 wireamp2(i).dat = wireamp(i).dat(0:cx_max-1)
 wireamp2(i).tt  = wireamp(i).tt(0:cx_max-1)
 wireamp2(i).wei = wireamp(i).wei(0:cx_max-1)
 wireamp2(i).gc  = wireamp(i).gc(0:cx_max-1)
endfor
 wireamp = wireamp2 & wireamp2 = 0B
; -------------------------------------------------------------------

; -------------------------------------------------------------------
save,filename=outfile_dat,wireamp
print,' %%% Saved ampl file: '+outfile_dat
; -------------------------------------------------------------------

for i=0,nstar-1 do begin
ttx  = wireamp(i).tt & datx = wireamp(i).dat & weix = wireamp(i).wei & gcx = wireamp(i).gc
w = where(ttx gt -100. and abs(datx) lt 2. and ttx lt 14.0,c) 
nn = robust_sigma(datx(w))
w2 =  where(ttx gt -100. and abs(datx) lt 10.*nn,c) 
w = w(w2)

ttx = ttx(w) & datx = datx(w)-median(datx(w)) & weix = wei(w) / total(wei(w)) & gcx = gcx(w)
outfile_p98 = dir + 'wire_lc_' + inf2(i) + '.dat'
openw,1,outfile_p98
for d=0L,c-1 do $
 printf,1,ttx(d),datx(d),weix(d),format='(D10.6,X,F10.6,X,F9.7)'
close,1
print,' %%% Output to .p98: '+outfile_p98
endfor ; next star!

skip_save:
; =====================================================================
if dops eq 1 then begin
 device,/close
 set_plot,'x'

 print,''
 print,' $gv '+outfile + ' & '
 print,' $lpr '+outfile + ' & '
 print,''
endif
; =====================================================================




END
