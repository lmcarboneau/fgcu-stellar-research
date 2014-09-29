PRO  wirep_plot, wid, $
                 d, lc, t0, $
                 onoff, $
                 fac, $
                 sp, $
                 t1, t2, $
                 fit2, fit2t, fit2on, $
                 fit, fitsub, $
                 freq1, freq2, $
                 wp, decor, dec, $
                 colon, dops, starnam=starnam, slot=slot, every=every, yyy=yyy

; To get amplitude spectrum power should be 1.0
; For a power spectrum power must be 2.0
power = d(0).power 

if n_elements(every) eq 0 then every = 1

if n_elements(yyy) eq 1 then stop

if n_elements(fit(0,*)) ne n_elements(lc.mag(0)) then fitsub = 0

add_phase = d(0).add_phase ; modify the phase ...
if n_elements(add_phase) eq 0 then add_phase = 0.5D

; colon = 1 ; plot in colours?

default9, vert_on, 0B

onceppm = 0B

print_hd = 1B ; print HD number on LC plot?

wplot = where(onoff eq 1,cplot) ; LC for which stars should program plot?

speccc = 0 ; set to 1 for special xyouts for altair -- f123

period = 1. / d(0).orb_freq ; orbital period c/day
org_period = period

unit_freq = "Frequency [!4l!3Hz]"
if abs(fac-1.0) lt 0.001 then unit_freq = 'Frequency [c/d]'
if abs(fac - 1e3/86400D) lt .001 then unit_freq = "Frequency [!3mHz]" ; milli Hz



cntrrr = 0B
cnt_plot_fft = 0B

; wp = which windows to plot

; Plot details: width of axes, symbol, symbols size ...
symm   = 1 & sym_sz = 0.1 & xxx = 1 & yyy = 1
cz_x = 0.75 ; Change Character size on plots here !!! Recommended range: [0.7..1.2]
cth = 1.0

if dops ge 1 then begin
 plotsym,0,/fill
 symm = 1 ; 3 ; symm = 8
 sym_sz = 0.4 ; 0.1
 xxx = 2 & yyy = 2
 cz_x = 1.4
 cth=2.0
endif



; Check the colours ... if you need them:
col      = getcolor(/load)
colx = dblarr(10)

; Line styles:
ls = findgen(2) * 2 ; solid = 0, dashed = 1
ls(*) = 0
nls = n_elements(ls)
th = [3,5]
nth = n_elements(th)

if dops eq 0 then begin
 ls(*) = 0
 th(*) = 1
endif


col_fft = findgen(n_elements(sp(0).calc) )

; help,cplot,colon

if cplot ge 2 and colon eq 1 then begin ; only use colours if more that one star!
 ; print,' %%% Setting colours ... ' 

 colx(*)  = col.white ; default colour
 colx(1)  = col.red
 colx(2)  = col.green
 colx(3)  = col.sky
 colx(4)  = col.magenta
 colx(5)  = col.pink
 colx(6)  = col.cyan
 colx(7)  = col.yellow
 colx(8)  = col.charcoal
 colx(9)  = col.orchid
 col_fit  = col.green

 col_fft(0) = col.green
 col_fft(1) = col.yellow
 col_fft(2) = col.red
; col_fft(3) = col.magenta

endif else begin
 colx(*)     = col.white
 col_fit     = col.green ; white
endelse

if dops ge 1 then begin
     !P.multi=0
     colx(*)   = col.black
     col_fit   = 150 ; col.black
     col_fft   = col.black

     case dops of
      0: suffix = 'Invalid'
      1: suffix = 'LC'
      2: suffix = 'AMPL' 
     endcase
     
     cx = 1 & a = 'First check ...'
     
;     while a(0) ne '' do begin
 
  starname = 'wire_unknown_star'

  if n_elements(starnam) eq 3 and n_elements(slot) ne 0 then $
   starname = 'wire_' + strcompress(starnam(0),/remove_all) + $
     strcompress(slot,/remove_all) + '_' + $ 
     strcompress(starnam(1),/remove_all) + '_' + $
     strcompress(starnam(2),/remove_all)

  initname = starname + '.ps'
  m4_get_basedir, bases
  initdir  = bases + '/wire/wire_eps/'

  ysiz = 10. &   x = 18.
  xoff = (21.0 - x) * 0.5
  yoff = (29.7 - ysiz) * 0.5
 
  keywords = PSConfig(Cancel=cancelled, $
                      Filename=initname, directory=initdir, $
                      /European, $
                      xsize=x,    xoffset=xoff, $
                      ysize=ysiz, yoffset=yoff)

   if cancelled then RETURN

   Set_Plot, 'PS'
   Device, _Extra=keywords

endif


 ncolx = n_elements(colx)
 ncol_fft = n_elements(col_fft)

; Default text sizes:
cz = 0.9
cz_text = 1.1
cz_modelname = 0.8 ; model name output
if dops ge 1 then cz_text = 0.9

; Plot counter (for getting the right position of the plots)
cnt_plot = 0


goo = bytarr(3+5) ; the three plots (+ decor if set!)
wd  = findgen(3+5)
wd2 = findgen(3+5) ; -1)

if wp eq -1 then goo(wd2) = 1 else goo(wp) = 1

if dops eq 1 then begin ; light curve plot
 goo(*) = 0
 goo(1) = 1
endif
if dops eq 2 then begin ; ampl. spec. plot
 goo(*) = 0
 goo(2) = 1
endif
if decor eq 0 then goo(3:*) = 0 ; do not plot decor windows!


if cplot eq 0 then begin
 print,' %%% No light curvers to plot!'
 RETURN
endif



; =====================================================
; Find the maximum value of any ampl. spectrum plotted:
; =====================================================
if goo(2) eq 1 then begin ; fft plot on ?
allmx = 0.
for stars=0,cplot-1 do begin
 ampl = (sp(wplot(stars)).y2)^2.0
 ws    = where(sp(wplot(stars)).calc ge 1,cs)

for jj=0,cs-1 do begin
 w = where(sp(wplot(stars)).freq(ws(jj),*) ge 0.05*fac and $
           sp(wplot(stars)).freq(ws(jj),*) ge freq1*fac and $
           sp(wplot(stars)).freq(ws(jj),*) le freq2*fac,c)
  ; valid points f > 0.05 c/day
 mx = 0
 ; help,c,freq1,freq2,fac

 if c ge 20 then mx = max(sp(wplot(stars)).amp(ws(jj),w))
 if mx gt allmx then allmx = mx 
endfor
endfor

 if allmx lt 0.5 then allmx = 1000.
 y_ran_fft1 = 0. ; min(sp(wplot).y1)
 y_ran_fft2 = (allmx * 1.1)^power ; max(sp(wplot).y2)
endif ; find the y-range of the fft plot?

y_ran      = max(d(wplot).y)   ; Y range for magnitude plots



; --------------------------------------------------------------------------
cntmax = cplot-1
for cnt=0,cplot-1 do begin ; plot all stars that are marked
; for cnt=cplot-1,0,-1 do begin ; plot all stars that are marked
; --------------------------------------------------------------------------
star = wplot(cnt)

; For old data sets, angle was not part of the lc structure:
angle = fltarr(n_elements(lc)) 
if tag_exist(lc,'angle') then angle = lc.angle(star)


; --------------------------------------------------------------------------
; The first plot: complete light curve
if goo(0) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(0).win

w = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c) ; any valid points?

w2 = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) eq (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c2) ; any valid points?

; Scattered light data:
w3  = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c3) ; any valid points?

; y_ran = 1.5e-3

 if c ge 2 then begin
   if cnt eq 0 then $
    plot,[0,1],xr=[d(star).ta1,d(star).ta2]-t0,yr=y_ran*[1,-1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
           xtit='HJD - '+$
           strcompress(string(t0,format='(D10.1)'),/remove_all) + $
           ' [d]', ytit='!4D!3 m',xsty=3 else $
    plot,[0,1],xr=[d(star).ta1,d(star).ta2]-t0,yr=y_ran*[1,-1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy,/noerase,$
               xsty=3
    x = lc(w).hjd - t0


; !4D!3t = 

     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation

    wire_get_every, x, every, dops, ev ; do not plot all points?
    oplot,x(ev),y(ev),psym=symm,symsize=sym_sz ; ,col=colx(star mod ncolx)
;    oplot,x(ev),y(ev),psym=6,symsi=.5, col=col.green; ,col=colx(star mod ncolx)

if c2 ge 2 then begin
     y2 = lc(w2).mag(star) - d(star).mmag - $
          fitsub * fit(star,w2) - $               ; Fitted light curve
          dec.appfwhmfit * dec.ffit(star,w2) - $  ; FWHM poly fit
          dec.apptimefit * dec.tfit(star,w2) - $  ; TIME spline fit
          dec.appbackfit * dec.bfit(star,w2) - $  ; Background spline
          dec.appscatfit * dec.sfit(star,w2) - $  ; Scat light spline
          dec.appxyfit * dec.xfit(star,w2)        ; XY Decorrelation
    x2 = lc(w2).hjd - t0

    wire_get_every, x2, every, dops, ev ; do not plot all points?
    oplot,x2(ev),y2(ev),psym=3,col=col.red ; colx(1)
endif

     print,' %%% Object median ADU: ' + $
             strcompress(string(10.^((25. - d(star).mmag ) / 2.5),format='(I8)'),$
              /remove_all)

if vert_on then begin
 plots,!x.crange,0,col=col.green
 plots,!x.crange,0.038,col=col.green
 plots,!x.crange,0.005,col=col.green
endif

print,' %%% Offset values ... t0 = '+string(t0,format='(F12.3)') + $
              ' m0 = ' + string(d(star).mmag,format='(F7.4)')
   
   ; Mark plotting range:
    plots,t1-t0,!y.crange,col=colx(1),line=2,thick=3
    plots,t2-t0,!y.crange,col=colx(1),line=2,thick=3
    yy = !y.crange & ray = (yy(1)-yy(0)) * 0.5
    arrow,t1-t0,ray*0.8,t2-t0,ray*0.8,thick=1,col=colx(1),/data
    arrow,t2-t0,ray*0.8,t1-t0,ray*0.8,thick=1,col=colx(1),/data
    xyouts,(t1-t0) + ((t2-t0)-(t1-t0))*0.5D,ray*0.87,$
     alignment=0.5,'z00m',charsi=0.7,col=colx(1),charthick=1.0
 endif
endif
; --------------------------------------------------------------------------


; --------------------------------------------------------------------------
; Second plot = the light curve, zoomed
if goo(1) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(1).win

w = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge t1 and lc.hjd le t2,c) ; any valid points?

w2 = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) eq (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c2) ; any valid points?


facy = 1000. ; multiply by 1000

    if c2 ge 2 then begin
     y2 = lc(w2).mag(star) - d(star).mmag - $
         fitsub * fit(star,w2) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w2) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w2) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w2) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w2) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w2)        ; XY Decorrelation
     x2 = lc(w2).fwhm(star) 
    endif

; Original plots:
    if cnt eq 0 then $
    plot,[0,1],xr=[t1,t2]-t0,yr=y_ran*[1.,-1.]*facy,$ ; yr=[-1,1]*0.0015*facy, $ ; ,
               /nodata,charthick=cth,charsize=cz_x,xthick=xxx,ythick=yyy, $
               xtit='HJD - '+$
               strcompress(string(t0,format='(D10.1)'),/remove_all) + $
               ' [d]', ytit='!4D!3m [mmag]',ysty=1,xsty=3 else $
    plot,[0,1],xr=[t1,t2]-t0,yr=y_ran*[-1.,1.]*facy,$ ; yr=[-1,1]*0.0015*facy, $   
              /nodata,charthick=cth,charsize=cz_x,xthick=xxx,ythick=yyy,/noerase,$
               ysty=1,xsty=3

if vert_on then begin
 plots,!x.crange,0,col=col.green
 plots,!x.crange,0.038*facy,col=col.green
 plots,!x.crange,0.005*facy,col=col.green
endif

   
    if c ge 2 then begin
     x = lc(w).hjd - t0
     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation
     y = ( y * facy )

     if fit2on eq 1 then oplot,fit2t(star,*)-t0,fit2(star,*) * facy,col=col_fit
     ; plot all points in zoomed lc! wire_get_every, x, every, dops, ev ; do not plot all points?
     oplot, x, y, psym=symm, symsize=sym_sz, col = colx(star mod ncolx)


     if fit2on eq 1 and dops ge 1 then oplot,fit2t(star,*)-t0,fit2(star,*) * facy,col=col_fit
 
     if print_hd and n_elements(starnam) eq 3 then begin
      outname_hd = starnam(1) + ' (' + starnam(2) + ')'
       x1pp = (t2-t1)*0.04 + t1 - t0 & y1pp = -1.0 * facy * 0.8 * y_ran
      xyouts, x1pp, y1pp, outname_hd, $
       charsi=1.2,charthick=2
      ; print,' HD name on plot: ' + outname_hd, x1pp, y1pp
     endif

    endif

endif
; --------------------------------------------------------------------------


; --------------------------------------------------------------------------
; Third plot: amplitude spectrum
if goo(2) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(2).win


; Force y-plot range for the AMPLITUDE SPECTRUM ? 
;  y_ran_fft2 =  20.



if cnt eq 0 then $
plot,[0,1],xr=[freq1,freq2]*fac,yr=[y_ran_fft1,y_ran_fft2],$
           /nodata,charthick=cth,charsize=cz_x,xthick=xxx,ythick=yyy, $
           xtit=unit_freq,ytit=d(0).powertext,ysty=1,xsty=1 else $
plot,[0,1],xr=[freq1,freq2]*fac,yr=[y_ran_fft1,y_ran_fft2],$
           /nodata,charthick=cth,charsize=cz_x,xthick=xxx,ythick=yyy, /noerase,$
           ysty=1,xsty=1

    if print_hd and n_elements(starnam) eq 3 then begin
      outname_hd = starnam(1) + ' (' + starnam(2) + ')'
       x1pp = fac * ((freq2-freq1)*0.05 + freq1) 
       y1pp = y_ran_fft2 - (y_ran_fft2 - y_ran_fft1) * 0.10
      xyouts, x1pp, y1pp, outname_hd, $
       charsi=1.2,charthick=2
      ; print,' HD name on plot: ' + outname_hd, x1pp, y1pp
     endif


; A SPECIAL CASE:
if cnt eq 0 then begin
; Mark two freqs. that are not yet subtracted (Altair paper)
; offset_x = 0.2
; f1 = 23.27952 + offset_x & a1 = (122. / 1.08) * 0.95
; xyouts,f1/fac,a1, '!17f!I6!N!3', charthick=cth,charsi=1.5
; f2 = 28.40869 + offset_x & a2 = (142. / 1.08) * 0.95
; xyouts,f2/fac,a2, '!17f!I7!N!3', charthick=cth,charsi=1.5
endif


ws = where(sp(star).calc ge 1,cs) ; one or more calc. amplitude spectra?

for jj=0,cs-1 do begin
 w = where(sp(star).freq(ws(jj),*) ge 0.,c) ; any valid points?
 
  if c ge 2 then begin
    x = sp(star).freq(ws(jj),w)
    y = sp(star).amp(ws(jj),w) / 1.086 ; conv. from delta mag to ppm: FACT 1 ppt = 1.086 mmag
    y = y^power

   if dops ge 1 then begin ; altair
    col_fft = [col.black,col.black] ; [0,100]
    th = [2,2] ; [4,4]

   col_fft = [0,120]
   th = [3,4]

    ; Three freq, altair:
   endif else begin
    col_fft = [col.white,col.green, col.red]
    th = [1,1,1]
   endelse

   ; Altair, comp. plots 
    ncol_fft = n_elements(col_fft)
    nth = n_elements(th)

   ; Plot the spectrum:
     if dops eq 0 then $
     oplot,x,y  , col = col_fft(sp(star).calc(ws(jj))-1), $
                  line = ls(star mod nls), $
                  thick = th(star mod nth)
     if dops ge 1 then $
     oplot,x,y  , col = col_fft(cnt_plot mod ncol_fft), $
                  line = ls(star mod nls), $
                  thick = th(star mod nth)

;     print,' %%% Col fft: ',col_fft(cnt_plot mod ncol_fft)

; Mark orbital period + overtones:
nfreq_orb = 200
wirefreq = (1./org_period) * (findgen(nfreq_orb)+1.) ; 15 c/day
for j=0,nfreq_orb-1 do $
  if wirefreq(j) gt freq1 and wirefreq(j) lt freq2 then $
   if dops eq 0 then $
   plots,wirefreq(j)*fac,!y.crange,line=2,thick=2,col=col.red else $
   ; print,' No 15 * n lines plotted ... '

; Important counter:
     cnt_plot = cnt_plot + 1

;    if dops ge 1 then $
;     oplot,x,y,line = ls(sp(star).calc(ws(jj))-1)
;    if dops eq 0 then oplot,x,y, col = col_fft(star)
;    if dops ge 1 then oplot,x,y,line = ls(star)

    wg = where(x ge freq1*fac and x le freq2*fac,cg)
    if cg ge 10 then begin
    me = median(y(wg))

; Mark noise/Ampl. levels:    

    if dops eq 0 then $
    plots,!x.crange,me,line=2

    ra5 = (freq2-freq1)*fac
    yy = !y.crange 
    yr5 = max(yy) - min(yy)
    ymax = max(yy)

; Vertical lines on FFT plot
    if ymax gt 220^power then plots,!x.crange,200^power,line=1
    if ymax gt 550^power then plots,!x.crange,500^power,line=1

    if dops eq 0 and onceppm eq 0 then begin
    onceppm = 1B
    if me lt 100. then $
     outppm = strcompress(string(me,format='(F7.1)'),/remove_all) else $
     outppm = strcompress(string(me,format='(I7)'),/remove_all)

    xppm = freq1*fac+ra5 * 0.7 + star * 0.05
    yppm = yy(1) - yr5 * 0.1 * (cntrrr + 1)
   
    xyouts,/data, xppm, yppm, outppm + ' ppm',charsi=1.4
    cntrrr = cntrrr + 1
    endif
 
    print,' %%% Noise level: ' + $
      strcompress(string(me,format='(F7.1)'),/remove_all) + $
      ' ppm^' + string(power,format='(F3.1)')
    endif

; Mark frequencies
    wm = where(sp(star).mark gt 0.,cm)
    wm = where(sp(star).mark gt 90.,cm)
    ; print,' %%% Number of freqs to mark: ',cm

    for im=0,cm-1 do begin

     ; wm2 & wm3: compute max y value in range around peak:
     ; print,sp(star).mark(wm(im))
     wm2 = where(abs(x-sp(star).mark(wm(im))) lt 0.08*fac,cm2)
     wm3 = where(abs(x-sp(star).mark(wm(im))) lt 0.04*fac,cm3)
 
    yy1 = !y.crange & yy1 = yy1(1)
     if cm2 ge 3 then begin
        maxpeak = max(y(wm2)) 
        yy2 = maxpeak * 1.05 ; 1.1
       ; print,sp(star).mark(wm(im)),' peak: ',maxpeak, ' ppm.'
    endif else yy2 = 0.0
     if yy2 gt y_ran_fft2 then yy2 = max(y(wm2))

     if cm3 ge 3 then begin
        maxpeak = max(y(wm3)) 
        yy3 = maxpeak * 1.15        
    endif else yy3 = 0.0
     if yy3 gt y_ran_fft2 then yy3 = max(y(wm3))

     xp = sp(star).mark(wm(im)) 
     xp_text = xp  + 0.05

    nnn = findgen(cm) + 1

; Altair: all freqs
; ff_out = [0,1,2,2,9,3,0,4,2 ,0,91,5,0,7,95,6,97]

    ff_out = indgen(150)
    ff_out2 = ff_out(cnt_plot_fft+1)

; Mark frequencies?
   if d(0).markfreq then begin
    xyouts,xp_text,yy2,'!17f!I'+$
      strcompress(string(ff_out2,format='(I3)'),/remove_all)+'!N!3',/data,$
     charthick=cth,charsi=1.5
    cnt_plot_fft = cnt_plot_fft + 1
    oplot,[xp,xp],[yy1,yy3],thick=1.2,col=col_fft(sp(star).calc(ws(jj))-1),line=2
;    arrow,/data,xp,yy1,xp,yy3,thick=1.2,col=col_fft(sp(star).calc(ws(jj))-1)
   endif

    if (star eq 0) and (abs(xp-16.0) le 1.0) and speccc eq 1 then begin ; special AMP plot
    speccc = 0
    nnn = findgen(cm) + 1
    xyouts,xp,yy2,'!17f!I1+4+5'+'!N!3',/data,$
     charthick=cth
    ; cnt_plot_fft = cnt_plot_fft + 1

;      arrow,/data,xp,yy1,xp,yy3,thick=1.2, $
;       col=col_fft(sp(star).calc(ws(jj))-1)

    endif else begin
     ; cnt_plot_fft = cnt_plot_fft + 1
    endelse




    endfor

  endif


endfor

; stop

endif
; --------------------------------------------------------------------------


; --------------------------------------------------------------------------
; Fourth plot: Decorrelation window #1  dx-position
if goo(4) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(4).win

w = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2 and $
            angle gt -400.,c) ; any valid points?



wzoom = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge t1 and lc.hjd le t2 and $
            angle gt -400,czoom)

 if c ge 2 then begin

     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation

     if czoom ge 5 then $
     yzoom = lc(wzoom).mag(star) - d(star).mmag - $
         fitsub * fit(star,wzoom) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,wzoom) - $  ; FWZOOMHM poly fit
         dec.apptimefit * dec.tfit(star,wzoom) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,wzoom) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,wzoom) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,wzoom)

    x = lc(w).x(star) &  offx = floor(median(x)) 
    x1 = min(x-offx) & x2 = max(x-offx)

    xt = findgen(c)
    rc = ((median(lc(w).x(star))) - 260.)^2. + $
         ((median(lc(w).x(star))) - 260.)^2.
    rc = sqrt(rc)
    dt = lc(w).hjd - t0
    factor = 0.30705 * !PI / 180 ; degrees of rotation pr. julian day
    xt = rc * dt * factor
    xt1 = min(xt) * 0.95
    xt2 = max(xt) * 1.05


; 15 aPR 2004:
    xt  = angle(w) ; lc(w).angle(star) ; * !PI * rc / 180.
    xt1 = min(xt) * 0.999
    xt2 = max(xt) * 1.001

    plot,[0,7],xr=[xt1,xt2],xsty=3,ysty=1,yr=y_ran*[-1,1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
               ytit='!4D!3 mag',xtit='!4D!3x [pix] --- True'
   wire_get_every, xt, every, dops, ev ; do not plot all points?
   oplot,xt(ev),y(ev),psym=symm,symsize=sym_sz 


    if czoom ge 5 then begin
     xt_zoom = angle(wzoom) ; lc(wzoom).angle(star)
     wire_get_every, xt_zoom, every, dops, ev ; do not plot all points?
     oplot,xt_zoom(ev),yzoom(ev),psym=symm,symsize=sym_sz,col=col.green ; ,xsty=1,yr=y_ran*[-1,1],ysty=1
    endif

    
 endif

endif
; --------------------------------------------------------------------------


; --------------------------------------------------------------------------
; Fifth plot: Decorrelation window #2
if goo(5) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(5).win

!P.multi=[0,2,1]

w = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c) ; any valid points?


 if c ge 2 then begin

     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation


; Compute stdev of xx,yy position. Use the larger one for x,y range!
    xx = lc(w).x(star) &  offxx = floor(median(xx)*10.) /10.
    rrx = robust_sigma(xx)
    yy = lc(w).y(star) &  offy = floor(median(yy)*10.) /10.
    rry = robust_sigma(yy)
    rruse = max([rrx,rry])

    x1 = - 1.0 * rruse * 8.
    x2 =         rruse * 8.

; Round off to nearest 10'th of pixel:
    if x1 gt 0. then x1 = floor(x1 * 10.) / 10. else x1 = floor(x1 * 10.) / 10.
    if x2 gt 0. then x2 = ceil(x2 * 10.) / 10. else x2 = ceil(x2 * 10.) / 10.

;     plot,[0,7],xr=[x1,x2],xsty=3,ysty=1,yr=y_ran*[-1,1],$
    plot,[0,7],xr=[x1,x2],xsty=3,ysty=1,yr=y_ran*[-1,1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
               xtit='!4D!3x [pix]'
    wire_get_every, xx, every, dops, ev ; do not plot all points?
    oplot,xx(ev)-offxx,y(ev),psym=symm,symsize=sym_sz 

    y1 = - 1.0 * rruse * 8.
    y2 =         rruse * 8.

; Round off to nearest 10'th of pixel:
    if y1 gt 0. then y1 = floor(y1 * 10.) / 10. else y1 = floor(y1 * 10.) / 10.
    if y2 gt 0. then y2 =  ceil(y2 * 10.) / 10. else y2 =  ceil(y2 * 10.) / 10.


    plot,[0,7],xr=[y1,y2],xsty=3,ysty=1,yr=y_ran*[-1,1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
               xtit='!4D!3y [pix]'
    wire_get_every, yy, every, dops, ev ; do not plot all points?
    oplot,yy(ev)-offy,y(ev),psym=symm,symsize=sym_sz 

;    wbad = where(abs(x-261.12) lt 0.05 and $
;                 abs(y-0.005) lt 0.0033,cbad)
;    if cbad ge 3 then $
;    if cbad ge 1000 then $
;     wire_get_every, wbad, every, dops, ev else ev = findgen(n_elements(x))
;    oplot,x(wbad(ev))-offx,y(wbad(ev)),col=col.red,psym=symm,symsize=sym_sz 
    ; print,offx, d(star).mmag

 endif

!P.multi=0

endif
; --------------------------------------------------------------------------






; --------------------------------------------------------------------------
; Fourth plot: Decorrelation window #4 -- background level!
if goo(3) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(3).win

w3  = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c3) ; any valid points?

w2 = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) eq (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c2) ; any valid points?

w = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c) ; any valid points?

wz = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge t1 and lc.hjd le t2,cz) ; any valid points?


 if (c ge 2) or (c2 ge 2) then begin

    if c ge 1 then begin
     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation

      x = fltarr(n_elements(lc(w))) ; missing in old reductions 
      if tag_exist(lc,'back') then x = lc(w).back(star)

;     x  = lc(w).back(star)
    endif

    if c3 ge 3 then begin
     y3 = lc(w3).mag(star) - d(star).mmag - $
         fitsub * fit(star,w3) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w3) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w3) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w3) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w3) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w3)        ; XY Decorrelation

      x3 = fltarr(n_elements(lc(w3))) ; missing in old reductions
      if tag_exist(lc,'back') then x3 = lc(w3).back(star)

;     x3 = lc(w3).back(star) 
    endif

    if cz ge 3 then begin
     yz = lc(wz).mag(star) - d(star).mmag - $
         fitsub * fit(star,wz) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,wz) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,wz) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,wz) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,wz) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,wz)        ; XY Decorrelation

      xz = fltarr(n_elements(lc(wz))) ; missing in old reductions
      if tag_exist(lc,'back') then xz = lc(wz).back(star)

;     xz = lc(wz).back(star) 
  
    endif

    xx1 = min(x3) * 0.9
    xx2 = max(x3) * 1.1

    plot,[0,1],xsty=1,ysty=1,yr=y_ran*[-1,1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
               ytit='!4D!3 mag',xtit='!3Background Flux [1 ADU=15e!E-!N]',xr=[xx1,xx2]
    if c  ge 2 then begin
     wire_get_every, x, every, dops, ev ; do not plot all points?
     oplot,x(ev), y(ev), psym=symm,symsize=sym_sz,col = colx(star mod ncolx)
;    if c3 ge 2 then $
;     oplot,x3,y3,psym=symm,symsize=sym_sz,col = col.red else print,' %%% No valid background points!'
    endif
     if cz ge 3 then begin
       wire_get_every, xz, every, dops, ev ; do not plot all points?
       oplot,xz(ev),yz(ev),psym=3,col=col.green
     endif

 endif

endif
; --------------------------------------------------------------------------




; --------------------------------------------------------------------------
; Seventh plot: Decorrelation window #4 -- FWHM !
if goo(6) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(6).win

w3  = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16*dec.remscat) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c3) ; any valid points?


; No bad data:
w2 = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) eq (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c2) ; any valid points?

; All data:
w  = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
           (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c) ; any valid points?

 if (c ge 2) or (c2 ge 2) then begin

    mint = min(lc.hjd)

    if c ge 1 then begin
     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation
     x  = lc(w).fwhm(star)
    endif

    if c2 ge 2 then begin
     y2 = lc(w2).mag(star) - d(star).mmag - $
         fitsub * fit(star,w2) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w2) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w2) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w2) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w2) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w2)        ; XY Decorrelation
     x2 = lc(w2).fwhm(star) 
    endif

    if c3 ge 2 then begin
     y3 = lc(w3).mag(star) - d(star).mmag - $
         fitsub * fit(star,w3) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w3) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w3) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w3) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w3) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w3)        ; XY Decorrelation
     x3 = lc(w3).fwhm(star) 
    endif

    ff = median(lc(w3).fwhm(star))
    if ff lt .2 or ff gt 5. then ff = 1.5
    rff = robust_sigma(lc(w3).fwhm(star)) * 26.

    plot,[0,1],xsty=1,ysty=1,yr=y_ran*[-1,1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
               ytit='!4D!3 mag',xtit='FWHM [pix]',xr=ff + [-1,1] * rff
    if c2  ge 2 then begin
     wire_get_every, x2, every, dops, ev ; do not plot all points?
     oplot,x2(ev), y2(ev), psym=symm,symsize=sym_sz,col = col.red
    endif
    if c3  ge 2 then begin
     wire_get_every, x3, every, dops, ev ; do not plot all points?
     oplot,x3(ev), y3(ev), psym=symm,symsize=sym_sz,col = colx(star mod ncolx)
    endif


 endif

endif
; --------------------------------------------------------------------------



; --------------------------------------------------------------------------
; Sixth plot: Decorrelation window #3
if goo(7) eq 1 then begin
; --------------------------------------------------------------------------
if dops eq 0 then wset,wid(7).win

w2 = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) eq (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c2) ; any valid points?

w = where((d(star).w XOR 1) eq (d(star).w-1) and $ ; good data
            (d(star).w XOR 8) ne (d(star).w-8) and $ ; bad points (10 sigma data?)
            (d(star).w XOR 16) ne (d(star).w-16) and $ ; no scat light!
            lc.hjd ge d(star).ta1 and lc.hjd le d(star).ta2,c) ; any valid points?


 if (c ge 2) or (c2 ge 2) then begin

    mint = min(lc.hjd)

;npp = 1 ; 25
;nm = ((findgen(npp) / (npp-1.))  - 0.5 ) * 0.002 + 1.00 ; 1.000 ; - 0.01
;print,nm
;for i=0,npp-1 do begin ; npp-1 do begin
;period = org_period * nm(i)
;print,' Hit any key ... ' & s = get_kbrd(1)

    if c ge 1 then begin
     y = lc(w).mag(star) - d(star).mmag - $
         fitsub * fit(star,w) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w)        ; XY Decorrelation
     x  = (( ((lc(w).hjd - mint) mod period ) / period) + add_phase) mod 1.0

     print,' %%% Modulation period: ' + string(period,format='(F10.7)') + ' days'
    endif

    if c2 ge 2 then begin
     y2 = lc(w2).mag(star) - d(star).mmag - $
         fitsub * fit(star,w2) - $               ; Fitted light curve
         dec.appfwhmfit * dec.ffit(star,w2) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,w2) - $  ; TIME spline fit
         dec.appbackfit * dec.bfit(star,w2) - $  ; Background spline
         dec.appscatfit * dec.sfit(star,w2) - $  ; Scat light spline
         dec.appxyfit * dec.xfit(star,w2)        ; XY Decorrelation
    x2 = ( (( (lc(w2).hjd - mint) mod period ) / period) + add_phase ) mod 1.0
    endif

    plot,[0,1],xsty=1,ysty=1,yr=y_ran*[-1,1],$
               /nodata,charthick=1.0,charsize=cz_x,xthick=xxx,ythick=yyy, $
               ytit='!4D!3 mag',xtit='!4D u!3',xr=[0,1]
    if c  ge 2 then begin
     wire_get_every, x, every, dops, ev ; do not plot all points?
     oplot,x(ev), y(ev), psym=symm,symsize=sym_sz,col = colx(star mod ncolx)
    endif

    if c2 ge 2 then begin
     wire_get_every, x2, every, dops, ev ; do not plot all points?
     oplot,x2(ev),y2(ev),psym=symm,symsize=sym_sz,col = col.red
    endif

; endfor ; try to find the correct orb. period?

endif


endif
; --------------------------------------------------------------------------






endfor ; go to the next star to plot ...

if dops ge 1 then begin ; print info on the .ps plot ... if it was made?
 device,/close
 set_plot,'x'
 psname = Keywords.Filename

 print,' '
 print,' %%% Output .ps file: '
 print,'  $ gv  ' + psname +'  & '
 print,'  $ lpr '+psname + ' '
; print,' %%%  $ lpr -Phpast02 '+psname + ' '
 print,' '
endif

;t0 = t0a

END
