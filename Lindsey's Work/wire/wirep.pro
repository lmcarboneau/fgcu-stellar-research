; This widget will help you analyse the light curves from WIRE
; (c) AUTUMN 2003 - SPRING 2004 -- Hans Bruntt, Univ. of Aarhus, Denmark
;
; Various improvements Autumn 2004 and spring 2005 by HB.
;
; Feb 2007: Various small improvements: FWHM and Scat Fit now much more robust.


; ---------------------------------------------------------------------
PRO wirep_Quit, event
Widget_Control, event.top, /Destroy
END 
;----------------------------------------------------------------------

;----------------------------------------------------------------------
PRO wirep_Help, event
 Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=help_entry
 print,'Help: '+help_entry
 vwa_hx_vwa_help, help_entry
END 
;----------------------------------------------------------------------

;----------------------------------------------------------------------
PRO wirep_ImportFreq, event

; -----> Import frequencies from period98/ period04 ...

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(Traceback=1)
   IF N_Elements(info) NE 0 THEN $
      Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

   ; Get the info structure 
 Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Prompt user for accurate file name
    spawnrob,'pwd',ddir
    dir = ddir(0)

   filt = '*' + info.target + '*.per'

   m4_get_basedir, base
   path = base + 'wire/wire_periods/'

; Rahmi Jackson, exception, December 2006:
   spawnrob,'whoami',whoami
   if strmatch(whoami,'rahmi') then begin
     path = '/import/suphys1/laszlo/rahmi/wire/'
     filt = '*'
   endif

    startFilename = 'wire_' + info.target + '.per'
    inputname = Dialog_Pickfile(File=startFilename,Filter=filt,path=path)

   IF inputname EQ "" THEN begin
     Widget_Control, event.top, Set_UValue=info, /No_Copy ; return the info!
     RETURN
   ENDIF

   lc = info.lc & nstar = info.nstar
   npoints = n_elements(lc)


   readcol,inputname,$
           dummy,p98f,p98a,p98p,$
           format='A,D,D,D'
   nfreq = n_elements(p98f) & np = n_elements(lc.hjd)
   fit = *info.fit  ;  fltarr(nstar,np)
   if n_elements(fit(0,*)) ne npoints then fit = fltarr(nstar,npoints) ; 16MAR2004

   wplot = where(info.onoff ge 1,cplot) ; import lc to all displayed light curves!
   if cplot ge 2 or cplot eq 0 then begin
      print,' %%% WARNING: FREQ. IMPORTED TO MORE THAN ONE STAR!'
      print,' %%% ABORTED!  --  cplot = ', cplot
      Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   endif
   if cplot eq 1 then wplot = wplot(0)

; wset,0
; col=getcolor(/load) & plot,fit(0,*) & oplot,fit(1,*),col=col.sky
; hitme,s
; if s eq 'x' then stop

   for st=0, cplot-1 do begin ; for the lc of each star
    fit(wplot(st),*) = 0.0 ; reset light curve (the new fit array!)
     for pp=0, nfreq-1 do $
      fit(wplot(st),*) = fit(wplot(st),*) + p98a(pp) * $
      sin(2. * !DPI * p98f(pp) * (lc.hjd - info.t0) + p98p(pp) * (2. * !DPI)  ) 
   endfor

   fit2d = *info.fit2d 
   fit2t = *info.fit2t - info.t0

  ; First time you import a light curve? Set up ...
   if n_elements(fit2d(0,*)) le 10 then begin
      tmin = min(lc.hjd-info.t0)-.1 & tmax = max(lc.hjd-info.t0)+.1
      np = 5e4 & fittimes = tmin + (findgen(np)/(np-1.)) * (tmax-tmin)
      fit2d = fltarr(nstar,np)
      fit2t = fltarr(nstar,np)
      for kk=0,nstar-1 do fit2t(kk,*) = fittimes
   endif

   print,' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% '
   print,'Freq (c/d)','Ampl [ppm]','Phase (0..1)',format='(3A15)'
   pi2 = (2D) * !DPI
   for st=0, cplot-1 do begin
    fit2d(wplot(st),*) = 0.0 ; erase any old lc.
   print,' %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% '

   ; Reset frequencies for this star:
   info.sp(wplot(st)).mark(*) = -9.9

   for pp=0, nfreq-1 do begin  ;  FOR EACH FREQUENCY
    fit2d(wplot(st),*) = fit2d(wplot(st),*) + $
     p98a(pp) * sin( (pi2 * p98f(pp) * fit2t(st,*)) + (p98p(pp) * pi2)  ) 
   print,p98f(pp),p98a(pp)*1e6,p98p(pp),format='(D15.5,D15.1,D15.5)'
    info.sp(wplot(st)).mark(pp) = p98f(pp) * info.fac ; freq in microHz    
   endfor ; next frequency
    ; print,st,wplot(st)
   endfor
                          ; next star


   ; Store the fitted light curve:
   *info.fit2d = fit2d &   *info.fit2t = fit2t + info.t0 ; better sampling 
   *info.fit = fit ; same # points as LC

   wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
               info.t1,info.t2, $
               fit2d, fit2t, info.fiton, $
               fit, info.fitsub, $
                info.freq1,info.freq2, $
               -1, info.decor,  info.dec,$
               info.colon, 0, every=info.every

Widget_Control, event.top, Set_UValue=info, /No_Copy

END
; ------------------------------------------------------------------------------



;----------------------------------------------------------------------
PRO wirep_Phased, event

; -----> Import phased LC: useful for binaries (LambdaSco, 57 Eri, AR Cas)

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(Traceback=1)
   IF N_Elements(info) NE 0 THEN $
      Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

   ; Get the info structure 
 Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Prompt user for accurate file name
    spawnrob,'pwd',ddir
    dir = ddir(0)

   filt = '*' + info.target + '*.pha'

   m4_get_basedir, base
   path = base + 'wire/wire_phased/'
    startFilename = 'wire_' + info.target + '.pha'
    inputname = Dialog_Pickfile(File=startFilename,Filter=filt,path=path)

   IF inputname EQ "" THEN begin
     Widget_Control, event.top, Set_UValue=info, /No_Copy ; return the info!
     RETURN
   ENDIF

   lc = info.lc & nstar = info.nstar
   npoints = n_elements(lc)

   readcol,inputname,phase,ampl,format='D,D'
   n = n_elements(phase)

   fit = *info.fit  ;  fltarr(nstar,np)
   if n_elements(fit(0,*)) ne npoints then fit = fltarr(nstar,npoints)

   wplot = where(info.onoff ge 1,cplot) ; import lc to all displayed light curves!
   if cplot ge 2 or cplot eq 0 then begin
      print,' %%% WARNING: PHASES IMPORTED TO MORE THAN ONE STAR!'
      print,' %%% ABORTED!  --  cplot = ', cplot
      Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   endif
   if cplot eq 1 then wplot = wplot(0)

; Get the initial phase curve:
   fit2d = *info.fit2d 
   fit2t = *info.fit2t - info.t0
   fit   = *info.fit
; Note that if sizes are only fltarr(5,5) then no data was imported before now!

   print,' %%% Reset fitted curve (default = n -- y/n)?'
   s = get_kbrd(1) & if s eq 'y' then reset = 0. else reset = 1.
   print,' %%% Add zero point in time (default = 0.0): '
   tzero  = 0D  &  read,tzero
   print,' %%% Zero point in time for phases: '
   tphase = 0D  &  read,tphase
   print,' %%% Period for phased LC: '
   per_phase = 0D  & read, per_phase

; Phased lc sampled at the same times as the observations:
   phase2 = ( (info.lc.hjd+tzero-tphase-info.t0) mod per_phase) / per_phase
   w = where(phase2 lt 0.,c) & if c ge 1 then phase2(w) = phase2(w) + 1.0 ; added 24/2/2006 HB, AR Cas
   fiti   = interpol(ampl, phase, phase2)

; Complete evenly sampled time coverage:
   phase2d  = ( ( reform(fit2t(wplot,*)) + tzero-tphase) mod per_phase) / per_phase
   w = where(phase2d lt 0.,c) & if c ge 1 then phase2d(w) = phase2d(w) + 1.0 ; added 24/2/2006 HB, AR Cas
   phase2d = reform(phase2d)
   fit2di   = reform( interpol(ampl, phase, phase2d) )

wset,0 ; feb 24th
   plot,info.lc.hjd, info.lc.mag(wplot),psym=1,symsi=.3,min_val=5,ysty=3
   oplot,lc.hjd,fiti+median(info.lc.mag(wplot)),col=250,thick=2
   hitme,s99 &    if s99 eq 'x' then stop

help,fit2d,fit2di

  ; First time you import a light curve? Set it up ...
   if n_elements(fit2d(0,*)) le 10 then begin
      tmin = min(lc.hjd-info.t0)-.1 & tmax = max(lc.hjd-info.t0)+.1
      np = 5e4 & fittimes = tmin + (findgen(np)/(np-1.)) * (tmax-tmin)
      fit2d  = fltarr(nstar,np)
      fit2t  = fltarr(nstar,np)
      for kk=0,nstar-1 do fit2t(kk,*) = fittimes
; stop
; Set up 24/2/066:
;    fit    = fltarr(nstar,np)
;    phase2d  = ( ( reform(fit2t(wplot,*)) + tzero-tphase) mod per_phase) / per_phase
;    phase2d = reform(phase2d)
;    fit2di = reform( interpol(ampl, phase, phase2d) )
   fit    = fltarr(nstar,n_elements(lc.hjd))
   phase2 = ( (info.lc.hjd+tzero-tphase-info.t0) mod per_phase) / per_phase
   w = where(phase2 lt 0.,c) & if c ge 1 then phase2(w) = phase2(w) + 1.0 ; added 24/2/2006 HB, AR Cas
   fiti   = interpol(ampl, phase, phase2)

; Complete evenly sampled time coverage:
   phase2d  = ( ( reform(fit2t(wplot,*)) + tzero-tphase) mod per_phase) / per_phase
   w = where(phase2d lt 0.,c) & if c ge 1 then phase2d(w) = phase2d(w) + 1.0 ; added 24/2/2006 HB, AR Cas
   phase2d = reform(phase2d)
   fit2di   = reform( interpol(ampl, phase, phase2d) )

  endif

help,fit2d,fit2di
hitme,s9 & if s9 eq 'x' then stop


   ; Store the fitted light curve: add it to any existing freq
   fit2d(wplot,*) = reset * (reform(fit2d(wplot,*))) + fit2di
   fit(wplot,*)   = reset * (fit(wplot,*))   + fiti

   *info.fit2t = fit2t ; 24/2/2006
   *info.fit2d = fit2d
   *info.fit   = fit 

   wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
               info.t1,info.t2, $
               *info.fit2d, fit2t, info.fiton, $
               *info.fit, info.fitsub, $
                info.freq1,info.freq2, $
               -1, info.decor,  info.dec,$
               info.colon, 0, every=info.every

Widget_Control, event.top, Set_UValue=info, /No_Copy

END
; ------------------------------------------------------------------------------




; ------------------------------------------------------------------------------
PRO wirep_SaveAs, event

; This event handler saves the light curve(s)

; Get the info structure 
 Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Gather button information. Construct default filename.
 Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=output_needed

 star = where(info.onoff eq 1,c)
 if (c ne 1) then begin
   print,' %%% WARNING: I can only export a single light curve at a time ... '
   print,info.onoff
   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN ; no LC to export
 endif
 star = star(0)
 
   m4_get_basedir, base
   filt = '*' + info.target + '*.dat'
   path = base + 'wire/wire_lc/'

; Rahmi Jackson, exception, December 2006:
   spawnrob,'whoami',whoami
   if strmatch(whoami,'rahmi') then begin
     base = '/import/suphys1/laszlo/rahmi/wire/'
     filt = '*'
     path = base
   endif

   startFilename = 'wire_lc_'+info.target+'_s' + $
                   strcompress(string(star),/remove_all) + '_' + $
                   strcompress(info.obj(star).hdnam,/remove_all) + '_' + $
                   strcompress(info.obj(star).spec,/remove_all) + '.dat'
                   
   
   ; Prompt user for accurate file name
   outname = Dialog_Pickfile(File=startFilename, Filter=filt,path=path,/Write)

   IF outname EQ "" THEN begin
     Widget_Control, event.top, Set_UValue=info, /No_Copy ; return the info!
     RETURN
   ENDIF

; ============================================================================
   ; Filename for t0 information:
; ============================================================================
   g = strsplit(outname,'.',/extract) & ng = n_elements(g)
   t0_name = ''

   if ng ge 3 then $
    for klo=0,ng-2 do t0_name = t0_name + g(klo) + '.' else $
    for klo=0,ng-2 do t0_name = t0_name + g(klo) 
   g2 = strsplit(t0_name,'/',/extract) & ng2 = n_elements(g2)
   t0_name2 = '/'
   for kl=0,ng2-2 do t0_name2 = t0_name2 + g2(kl) + '/'
   out_name2 = t0_name2 + 'smooth/' ; for saving the smoothed light curve
   t0_name2 = t0_name2 + 't0/' 
   t0_name2 = t0_name2 + g2(kl)
   t0_name2 = t0_name2 + '.t0' 

   smooth_factor = 11. ; number of data points per orbit!
;   if info.eclipsing then begin     
;   endif
   out_name2 = out_name2 + g2(kl) + '.sm' + $
                strcompress(string(smooth_factor,format='(I8)'),/remove_all) + '.dat'

   get_lun,uu & openw,uu,t0_name2
   printf,uu, strcompress(string(info.t0,format='(I10)'),/remove_all) ; + 'D'
   close,uu & free_lun,uu
   print,' %%% Saved t0 information: ' + t0_name2


; ============================================================================

    wg = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
               (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
               (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
                abs(info.lc.hjd-info.t0) lt 1200.0, cg)

; DECOR mode, output all data points!
;      if output_needed eq 'tdwfb' then begin
;        cg = n_elements(info.lc.hjd) & wg  = findgen(cg)
;    endif

     if cg ge 10 then begin
      decor_star = star ; default
      ;; decor_star = 3 ; for eps cep, star 3 has TIGHER correlations btw. BACKGR, FWHM and X-position!

      print,' %%% IMPORTANT: DECORRELATION STAR IS: ', decor_star

      ttm = info.lc(wg).hjd - info.t0
      fwhm = info.lc(wg).fwhm(decor_star) ; FWHM of stellar profile
      xvalue = info.lc(wg).x(decor_star) ; added 29. March 2006
      yvalue = info.lc(wg).y(decor_star)
      backgr = info.lc(wg).back(decor_star)
      ; backgr = info.lc(wg).back(star) ; try this
      dataid = wg


     fit = *info.fit
     fitsub = double(info.fitsub)
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0 

; For calculating weights: subtract any fitted LC !!
    ddwei  = info.lc(wg).mag(star) - info.d(star).mmag - $
         1D * fit(star,wg) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wg) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wg) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wg) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wg) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wg)      ; XY Decorrelation

    dd = info.lc(wg).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wg) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wg) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wg) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wg) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wg) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wg)      ; XY Decorrelation

       dd    = dd    - median(  dd)
       ddwei = ddwei - median(ddwei)

     rr = robust_sigma(ddwei)
     ; wg2 = where(abs(ddwei) lt 10. * rr,cg2)
     wg2 = where(abs(ddwei) lt 1000. * rr,cg2)
; DECOR mode, output all data points!
      if output_needed eq 'tdwfb' then begin
         cg2 = n_elements(ttm) & wg2  = findgen(cg2)
      endif
 
     wg = wg(wg2) & cg = cg2
     dd = dd(wg2)
     ddwei = ddwei(wg2)
     ttm = ttm(wg2)
     fwhm2 = fwhm(wg2)
     xvalue2 = xvalue(wg2)
     yvalue2 = yvalue(wg2)
     backgr2 = backgr(wg2)
     dataid2 = dataid(wg2)

     wire_stetson, ddwei, wei, 0 ; Stetson Weights
     wei_stet = wei / total(wei)
     wei_ptp  = fltarr(cg2) & wei_ptp(*) = 1.0 / float(cg2) ; equal ptp weights

     ; Remove really bad points === ultra low weights!
       w2  = where(wei gt max(wei_stet) * 0.004,c2) ; for variable stars!

; For eclipsing binaries... do not remove eclipses!
      if (info.eclipsing eq 1) then begin
       print, ' %%% Eclipsing binary mode! '
       w2  = where(wei gt max(wei_stet) * 0.004 or ddwei gt 0.,c2) ;
      ; MODE FOR ECL BINARIES -- added 18th of March 2005 by HB
      endif

; Save all data for decorrelation:
      if output_needed eq 'tdwfb' then begin
       print, ' %%% Decorrelation data saved ... saving almost all data!'
       c2 = n_elements(ttm) & w2  = findgen(c2)
      endif

      tt2 = ttm(w2) 
      dd2 = dd(w2) & dd2 = dd2 - median(dd2)
      dd2wei = ddwei(w2) & dd2wei = dd2wei - median(dd2wei)
      we2 = wei_stet(w2) * wei_ptp(w2) & we2 = we2 / total(we2)
      dd2 = dd2 - median(  dd2)
      fwhm3 = fwhm2(w2)
      xvalue3 = xvalue2(w2)
      yvalue3 = yvalue2(w2)
      backgr3 = backgr2(w2)
      dataid3 = dataid2(w2) ; to be able to identify the original data ID

 frac_limit = 0.25 ; limit on weights --> discard points?

 wb=where(we2 lt median(we2 * frac_limit),cb, comp=wout)

; For eclipsing binaries... do not remove eclipses!
      if (info.eclipsing eq 1) then begin
       print, ' %%% Eclipsing binary mode! '
       wout = where(we2 gt median(we2) * frac_limit or dd2 gt 0.,comp=wb) 
       cb = n_elements(wb)
      ; MODE FOR ECL BINARIES -- added 18th of March 2005 by HB
      endif

 window,0,tit='Discard data points?'
 plot,tt2,dd2,psym=3
 if cb ge 2 then oplot,tt2(wb),dd2(wb),psym=6,symsi=.7

      get_lun, u
      openw,u,outname

     ; Export time, data, Weight?
      if output_needed eq 'tdw' then begin

       print,' %%% Remove data points with low weight? '
       answer = get_kbrd(1)
       if answer ne 'y' then wout = findgen(n_elements(tt2))
       c2out = n_elements(wout)

       for i=0L,c2out-1 do begin
        printf,u, $
          tt2(wout(i)), dd2(wout(i)), we2(wout(i)), $ 
         format = '(D12.7,F12.7,F11.8)'
       endfor 
      endif


     ; Time series + smoothed time series
      if output_needed eq 'tdwsmooth' then begin
       for i=0L,c2-1 do begin
        printf,u, $
          tt2(i), dd2(i), we2(i), $ 
         format = '(D12.7,F12.7,F11.8)'
       endfor 
      close,u ; close the file so I can import it:


      ; Get smoothed light curve:
      wire_bin_ts,outname, smooth_factor, ssd, ssd2  ; ,/debug

      ptp_robust_fin, ssd.d, noise_sm, 1
      wei = 1. / (ssd.s^2.0 + (noise_sm*5.)^2.0)
      wei = wei / total(wei)

      ; plot,ssd.t,ssd.d,psym=3,xr=[0,1]  ; new lc
      ; oplot,ssd2.t,ssd2.d,psym=1,col=col.red,symsi=.1 ; org

      cout = n_elements(ssd)

      get_lun,u2 & openw,u2,out_name2

       for i=0L,cout-1 do $
        printf,u2, $
          ssd(i).t, ssd(i).d, wei(i), $ 
         format = '(D12.7,F12.7,F11.8)'
       close,u2      
       print,' %%% Saved smoothed light curve: ' + out_name2

      endif


     ; Export time, data, weight, fwhm ?        
      if output_needed eq 'tdwf' then begin
       for i=0L,c2-1 do begin
        printf,u, $
          tt2(i), dd2(i), we2(i), fwhm3(i),$
         format = '(D12.7,F12.7,F11.8,F9.5)'
       endfor 
      endif
  
                                ; Export time, data, weight, fwhm,
                                ; background, x-y ? decor March 2006
      if output_needed eq 'tdwfb' then begin
       for i=0L,c2-1 do begin
        printf,u, $
          tt2(i), dd2(i), we2(i), dataid3(i), xvalue3(i),yvalue3(i) , backgr3(i), fwhm3(i), $
         format = '(D12.7,D12.7,F11.8, I12, D10.4,D10.4, D10.2, D10.6)'
       endfor 
      endif


       
      close,u
      free_lun, u
     endif else begin
      print,' %%% No valid data points to export!'
     endelse

 print,' %%% Light curve saved as: '+outname

 Widget_Control, event.top, Set_UValue=info, /No_Copy
END 
; ---------------------------------------------------------------------

; ---------------------------------------------------------------------
PRO wirep_OpenALL, event

   ; Read saved reductions of light curves ...

   ; Get the info structure 
   Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Gather button information. Construct default filename.
   Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=file_extension
 
   spawnrob,'pwd',present_dir
   startFilename = 'wirep_struct' + file_extension
   filt = present_dir + '/wirep*.idl'
   m4_get_basedir, basedirr
   path = basedirr + 'wire_process/'

   ; Prompt user for accurate file name
   inname = Dialog_Pickfile(File=startFilename, path=path, Filter=filt, /Read)

   IF inname EQ "" THEN begin
     print,' *** Invalid filename.'
     Widget_Control, event.top, Set_UValue=info, /No_Copy ; return the info!
     RETURN
   ENDIF

    restore,inname ; restore the information (wirep_struct)
; print,'OpenALL1: ',info.dec.appxyfit, wirep_struct.dec.appxyfit

    decor = info.decor
    wirep_update_info, info, wirep_struct, info.field, decor=decor

; print,'OpenALL2: ',info.dec.appxyfit, wirep_struct.dec.appxyfit

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; return info structure!
END
; ---------------------------------------------------------------------

; ---------------------------------------------------------------------
PRO wirep_SaveALL, event

; This event handler saves the complete reduction !!

; Get the info structure 
 Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Gather button information. Construct default filename.
 Widget_Control, event.id, Get_Value=buttonValue, Get_UValue=file_extension
 
   spawnrob,'pwd',present_dir
   ; startFilename = present_dir + '/wirep_struct' + file_extension
   startFilename = 'wirep_' + info.target + '.idl'

   m4_get_basedir, base

   ; filt = present_dir + '/wirep*.idl'
   filt = base + '/wire_process/wirep_*' + file_extension

   ; Prompt user for accurate file name
   outname = Dialog_Pickfile(File=startFilename, Filter=filt, /Write)

   IF outname EQ "" THEN begin
     print,' *** Invalid filename.'
     Widget_Control, event.top, Set_UValue=info, /No_Copy ; return the info!
     RETURN
 ENDIF



    wirep_struct ={d:info.d, lc:info.lc, $
             fiton:info.fiton, fitsub:info.fitsub, $
             obj:info.obj, $
             sp:info.sp, $
             fil:info.fil, $
             starname:info.starname, $
             nstar:info.nstar, $
             t0:info.t0, $
             t1:info.t1, t2:info.t2, $
             per1:info.per1, $
             fit2d:*info.fit2d, fit2t:*info.fit2t, $
             fit:*info.fit, $
             freq1:info.freq1, freq2:info.freq2, $
             fac:info.fac, $
             orb_freq:info.d(0).orb_freq, $
             specmax:info.specmax, $
             dec:info.dec}

; print,'SaveALL xy: ',info.dec.appscatfit, wirep_struct.dec.appscatfit

   save,filename=outname,wirep_struct
 
   print,' %%% Wirep Structure Saved As:  '+outname

 Widget_Control, event.top, Set_UValue=info, /No_Copy
END 
; ---------------------------------------------------------------------


; ---------------------------------------------------------------------------
PRO wirep_orbital_frequency, event
    ; Read the spectral resolution box that was entered in the box
     Widget_Control, event.id, Get_Value=freq_entered


    ff = float(*freq_entered)

    ; If wl_shift is too large ...
    if abs(ff - 15.0) gt 1.0 then begin
       print,' *** Orbital frequency outside acceptable range ' + strtrim(ff)
       RETURN
    endif

    ; change orbital freq for all stars 
     Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!
     info.d(*).orb_freq = ff

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec, $
                info.colon, 0, every=info.every



     Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
; ------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------
PRO wirep_zero_phase, event
    ; Change the zero phase value
     Widget_Control, event.id, Get_Value=freq_entered
    ff = float(*freq_entered)

    ; If wl_shift is too large ...
    if abs(ff) gt 1D then begin
       print,' *** Zero point phase outside acceptable range ' + strtrim(ff)
       RETURN
    endif

    ; change orbital freq for all stars 
     Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!
     info.d(*).add_phase = ff

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec, $
                info.colon, 0, every=info.every

     Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
; ------------------------------------------------------------------------------------



; ------------------------------------------------------------------------------------
PRO wirep_recalc_noise, event
 
    Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!

   nstar = n_elements(info.d.w(0))
   for star = 0,nstar-1 do begin

; ALL DATA THAT WILL OBTAIN A CORRECTION (DEPENDING ON X,Y POSITION):
      w2 = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
                 (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
                 (info.d(star).w XOR 16*info.dec.remscat) ne (info.d(star).w-16) and $ ; no scat light!
                abs(info.lc.hjd-info.t0) lt 1200.0, c2)

     if c2 ge 50 then begin
       ; light = info.lc(w2).mag(star)

      fit = *info.fit
      fitsub = info.fitsub
      if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

     y = info.lc(w2).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,w2) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,w2) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,w2) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,w2) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,w2) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,w2)      ; XY Decorrelation

      dev = robust_sigma(y)
      info.d(star).noi = dev
      info.d(star).y   = info.d(star).noi * info.scale_noise

      print,' %%% RMS noise in light curve for star #' + $
        string(star,format='(I2)') + ' ' + $
        string(dev,format='(D10.5)')

     endif

 endfor

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec, $
                info.colon, 0, every=info.every

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
;----------------------------------------------------------------------



;----------------------------------------------------------------------
PRO wirep_postscript_lc, event
 
    Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!

    slot = where(info.onoff eq 1,n_slot)
    if n_slot eq 1 then begin 
        slot = '_lc_s' + strcompress(slot(0),/remove_all)
    endif else begin 
     slot = '_Sev'
    endelse

    won = where(info.onoff eq 1,con)
    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 1, $
                starnam=[info.target,info.obj(won).hdnam,info.obj(won).spec], $
                slot=slot
    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
;----------------------------------------------------------------------

;----------------------------------------------------------------------
PRO wirep_postscript_fft, event

    Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!

    slot = where(info.onoff eq 1,n_slot)
    if n_slot eq 1 then begin 
        slot = '_fft_s' + strcompress(slot(0),/remove_all)
    endif else begin 
     slot = '_fft_Sev'
    endelse

    won = where(info.onoff eq 1,con)
    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 2, $
                starnam=[info.target,info.obj(won).hdnam,info.obj(won).spec], $
                slot=slot
    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
;----------------------------------------------------------------------


;----------------------------------------------------------------------
PRO wirep_Zoom_Slider, event

    ; Get info structure!
    Widget_Control, event.top, Get_UValue=info, /No_Copy

    Widget_Control, event.id, Get_Value=buttonValue
    if buttonValue lt 1 or buttonValue gt 25 then RETURN
    
     ; Set the new button value!
    info.scale_noise = float(buttonValue)

    nstar = info.nstar
    for i=0,nstar-1 do info.d(i).y = info.d(i).noi * info.scale_noise
    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every


    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!

END
;----------------------------------------------------------------------


;----------------------------------------------------------------------
; Decorrelation options ...
;----------------------------------------------------------------------
PRO wirep_decor, event

    ; Get info structure!
   Widget_Control, event.top, Get_UValue=info, /No_Copy
    ; Get the button value!
   Widget_Control, event.id, Get_Value=buttonValue

;   print,buttonValue

   case buttonValue of  
    'Scat Fit'     : begin
  
   wx = where(info.onoff eq 1,cx)
   for xx=0,cx-1 do begin

    star = wx(xx)
    wok = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
               (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
               (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 2200.0, cok)

            period = 1.0 / info.d(0).orb_freq
            mint = min(info.lc.hjd)
            phase = ( (( (info.lc.hjd - mint) mod period ) / period ) + info.d(0).add_phase) mod 1D
            
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

    flux  = info.lc(wok).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wok) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wok) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wok) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wok) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wok) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wok)      ; XY Decorrelation
    
            xmin = min(phase(wok)) & xmax = max(phase(wok)) & rax = xmax - xmin
            npt = 150 & fitpha = fltarr(2,npt)
            phastep = rax / (npt-1.) & border = phastep * 0.3
            fitpha(0,*) = findgen(npt) * phastep + xmin ; x-axis 
            for i=0,npt-2 do begin
              s = where(phase(wok) ge (fitpha(0,i)-border) and $
                        phase(wok) lt (fitpha(0,i+1)+border),cs)
              if cs ge 10 then begin
                 resistant_mean,flux(s),3,me,sd,nr
                 fitpha(1,i) = me
              endif
             
            endfor
            fitpha(1,i) = fitpha(1,i-1) ; last point...

            info.dec.sfit(star,*) = 0.0 ; reset all 
            info.dec.sfit(star,wok) = interpol(fitpha(1,*),fitpha(0,*),phase(wok))

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                7,  $ ; update 7th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

             col=getcolor(/load)
             oplot,fitpha(0,wok),fitpha(1,wok),col=col.sky,psym=1,symsi=1.0
             oplot,phase,info.dec.sfit(star,*),col=col.red,psym=3
             wait,1.0

        endfor ; next star !

    endcase
     'App Scat Fit' : begin
         info.dec.appscatfit = (info.dec.appscatfit + 1) mod 2
         print,info.dec.appscatfit
        endcase
     'Rem Scat'     : info.dec.remscat = (info.dec.remscat + 1) mod 2

; ------------------------------------------------------------------------
; Added 22. MARCH 2005 by HB:
; ------------------------------------------------------------------------
; Scat Fit2: Time string devided in two parts (for long time strings)
    'Scat Fit2'     : begin

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   again99:
   print,' %%% You want to sub-divide time string and make SCAT fits to'
   print,' %%% the individual time strings. Good. How many 
   print,' %%% sub-time strings to you want (1-6 recommended).'
   s99 = 0. & read,'>>> ',s99 & antal = float(s99)
   if s99 le 1 or s99 ge 50 then goto,again99
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   again_smooth:
    print,''
    print,' %%% Choose a value for smoothing (recom. range 1-10):'
    smfac = 0. & read,'>>> ',smfac & smfac = float(smfac)
   if smfac lt 1 or smfac gt 40 then goto,again_smooth
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  
   wx = where(info.onoff eq 1,cx)
   for xx=0,cx-1 do begin
    star = wx(xx)

   tajm = info.lc.hjd & wg = where(tajm gt 50000.,c)
   maxt = max(tajm) & mint = min(tajm)
   time1 = mint + ((maxt-mint) / antal) * findgen(antal) ; start times
   time2 = mint + ((maxt-mint) / antal) * (1.+findgen(antal)) ; start times

   info.dec.sfit(star,*) = 0.0 ; reset phase fit! 
   
   for k=0,antal-1 do begin
    wok = where((info.d(star).w XOR  1) eq (info.d(star).w-1) and $ ; good data
                (info.d(star).w XOR  8) ne (info.d(star).w-8) and $ ; bad points 
                (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 2200.0 and $
               info.lc.hjd ge time1(k) and info.lc.hjd lt time2(k), cok)
    if cok lt 20 then goto,toofew

            period = 1.0 / info.d(0).orb_freq
            mint = min(info.lc.hjd)
            phase = ( (( (info.lc.hjd - mint) mod period ) / period ) + info.d(0).add_phase) mod 1D
            
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

    flux  = info.lc(wok).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wok) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wok) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wok) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wok) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wok) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wok)      ; XY Decorrelation
    
            xmin = min(phase(wok)) & xmax = max(phase(wok)) & rax = xmax - xmin
            npt = 200 & fitpha = fltarr(3,npt)
            phastep = rax / (npt-1.) & border = phastep * 0.3
            fitpha(0,*) = findgen(npt) * phastep + xmin ; x-axis 

; For each frequency bin:
            for i=0,npt-2 do begin
              s = where(phase(wok) ge (fitpha(0,i)-border) and $
                        phase(wok) lt (fitpha(0,i+1)+border),cs)
              if cs ge 10 then begin
                 resistant_mean,flux(s),3,me,sd,nr
                 fitpha(1,i) = me
                 fitpha(2,i) = cs - nr ; # used points
             endif else fitpha(1,i) = -999.
             
            endfor
            fitpha(1,i) = fitpha(1,i-1) ; last point...           

            wg = where(fitpha(1,*) gt -50.)
; debug
            window,1,tit='Debug: red = observed points; white == avg; blue == smoothed curve'
            col=getcolor(/load)
            rr = robust_sigma(fitpha(1,wg))
            plot,fitpha(0,wg),smooth(fitpha(1,wg),1,/edge),psym=1,symsi=.5,xsty=3,ysty=3,yr=[-1,1]*rr*5,xr=[.35,.55]
            oplot,phase(wok), flux,psym=3,col=col.red
            oplot,fitpha(0,wg),smooth(fitpha(1,wg),smfac,/edge),col=col.sky
            wait,1.5
            
            info.dec.sfit(star,wok) = interpol(smooth(fitpha(1,wg),smfac,/edge),fitpha(0,wg),phase(wok))
    toofew:
        endfor                  ; next PART of light curve!


    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                7,  $ ; update 7th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

             col=getcolor(/load)
             oplot,fitpha(0,wok),fitpha(1,wok),col=col.sky,psym=1,symsi=1.0
             oplot,phase,info.dec.sfit(star,*),col=col.red,psym=3
             wait,1.0

        endfor ; next star !

    endcase


; ------------------------------------------------------------------------
    'Back Fit'     : begin
  
   wx = where(info.onoff eq 1,cx)
   for xx=0,cx-1 do begin
      star = wx(xx)

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   againBACK2:
    print,' %%% You want to sub-divide time string and make BACK fits to'
    print,' %%% the individual time strings. Good. How many equally spaced'
    print,' %%% time strings to you want (1-8 recommended).'
    print,' %%% Beware: for any value > 1 this is effectively a high pass filter.'
    s99 = 0. & read,'>>> ',s99 & antal = float(s99)
   if s99 lt 1 or s99 ge 50 then goto,againBACK2
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   againBACK2p:
    print,' %%% How many grid values of the background (recommended: 50-300):'
    s99p = 0. & read,'>>> ',s99p & npt = float(s99p)
   if s99p lt 10 or s99p ge 500 then goto,againBACK2p
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   againBACK2s:
    print,' %%% Smooth the grid by this factor ' + $
                   '(recommended: 1-'+strcompress(string(s99p*0.1),/remove_all)+'):'
    s99s = 0. & read,'>>> ',s99s & sm_back = float(s99s)
   if s99s lt 1 or s99s ge (s99p*0.25) then goto,againBACK2s
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   againBACK2log:
    print,' %%% Use log (type 1) or linear background (type 0) ? '
    print,' %%% Recommendation: log may be best if large background range e.g. 0-2000 ADU.'
    use_log = 0B & s = get_kbrd(1) & use_log = fix(s) 
   if use_log ne 0 and use_log ne 1 then goto,againBACK2log
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   tajm = info.lc.hjd & wg = where(tajm gt 50000.,c)
   maxt = max(tajm) & mint = min(tajm)
   time1 = mint + ((maxt-mint) / antal) * findgen(antal) ; start times
   time2 = mint + ((maxt-mint) / antal) * (1.+findgen(antal)) ; start times

   info.dec.bfit(star,*) = 0.0 ; reset all 
  
   for k=0,antal-1 do begin

    wok = where((info.lc.mag(star) gt 5. and info.lc.mag(star) lt 25.) and $
              (info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
              (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
              (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 21200.0 and $
               info.lc.hjd ge time1(k) and info.lc.hjd lt time2(k), cok)
    if cok lt 20 then goto,toofew

     if use_log then begin
      background = alog10(info.lc.back(star)) 
      b1=1.2 & b2 =3.2 ; x-range      ; log scale on background?
     endif else begin
      background = info.lc.back(star) ; Linear background.
      b1=10 & b2 = 1700; x-range
     endelse
            
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0
 
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

     flux  = info.lc(wok).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wok) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wok) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wok) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wok) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wok) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wok)      ; XY Decorrelation

         xmin = min(background(wok)) & xmax = max(background(wok)) & rax = xmax - xmin
         fitback = fltarr(3,npt) & fitback(*,*) = 1e6
         backstep = rax / (npt-1.) & border = backstep * 0.1 ; beam size
         fitback(0,*) = findgen(npt) * backstep + xmin ; x-axis 

         for i=0,npt-2 do begin
           s = where(background(wok) ge (fitback(0,i)  -border) and $
                     background(wok) lt (fitback(0,i+1)+border),cs)
           if cs ge 5 then begin
              resistant_mean,flux(s),3,me,sd,nr
              fitback(1,i) = me
           endif 
           if cs ge 2 and cs le 4 then fitback(1,i) = avg(flux(s)) 
           if cs eq 1 then begin
               if s ne -1 then fitback(1,i) = flux(s)
           endif
         endfor
         fitback(1,i) = fitback(1,i-1) ; last point...


         wg = where(abs(fitback(1,*)) lt 5,cg)
         fitback(1,*) = interpol(fitback(1,wg),fitback(0,wg), fitback(0,*))
         fitback(2,*) = smooth(fitback(1,*), sm_back, /edge)
         fitback(0,*) = fitback(0,*) + backstep * 0.5
    
         col=getcolor(/load)
         window,1,tit='Background decorrelation. Bin = ' + string(k+1) + '/' + string(fix(antal))
         plot,background(wok),flux,psym=3,xr=[b1,b2],xtit='!6log (background)',ytit='!4D!6mag',$
           tit='White = observed points. Red = value in bin. Green = smoothed.'
          oplot,fitback(0,*),fitback(1,*),psym=1,symsi=.5,col=col.red
          oplot,fitback(0,*),fitback(2,*),psym=1,symsi=.5,col=col.green
         wait,2

; Use smoothed version: fitback(2,*)
         info.dec.bfit(star,wok) = interpol(fitback(2,*),fitback(0,*),background(wok))



    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                3,  $ ; update 3rd window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

             col=getcolor(/load)
             oplot,fitback(0,wok),fitback(1,wok),col=col.sky,psym=1,symsi=1.0
             oplot,10.^background,info.dec.bfit(star,*),col=col.red,psym=3
             oplot,background,info.dec.bfit(star,*),col=col.red,psym=3
             wait,3.0

         endfor


        endfor ; next star !

    endcase

; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
    'Back Fitxx'     : begin
  
   wx = where(info.onoff eq 1,cx)
   for xx=0,cx-1 do begin
      star = wx(xx)

    wok = where((info.lc.mag(star) gt 5. and info.lc.mag(star) lt 25.) and $
              (info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
              (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
              (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 21200.0, cok)

; Log scale used since March 2006:
            background = alog10(info.lc.back(star))
            
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0
 
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

    flux  = info.lc(wok).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wok) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wok) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wok) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wok) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wok) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wok)      ; XY Decorrelation

         xmin = min(background(wok)) & xmax = max(background(wok)) & rax = xmax - xmin
         npt = 300 & fitback = fltarr(2,npt)
         backstep = rax / (npt-1.) & border = backstep * 0.2 ; beam size
         fitback(0,*) = findgen(npt) * backstep + xmin ; x-axis 
         for i=0,npt-2 do begin
           s = where(background(wok) ge (fitback(0,i)  -border) and $
                     background(wok) lt (fitback(0,i+1)+border),cs)
           if cs ge 5 then begin
              resistant_mean,flux(s),3,me,sd,nr
              fitback(1,i) = me
           endif
          
         endfor
         fitback(1,i) = fitback(1,i-1) ; last point...

         
         info.dec.bfit(star,*) = 0.0 ; reset all 
         info.dec.bfit(star,wok) = interpol(fitback(1,*),fitback(0,*),background(wok))


    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                3,  $ ; update 3rd window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

             col=getcolor(/load)
             oplot,fitback(0,wok),fitback(1,wok),col=col.sky,psym=1,symsi=1.0
             oplot,10.^background,info.dec.bfit(star,*),col=col.red,psym=3
             wait,3.0

         endfor ; Next time sub-sample

                 ; next star !

    endcase
; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
     'App Back Fit' : begin
         info.dec.appbackfit = (info.dec.appbackfit + 1) mod 2
         print,info.dec.appbackfit
        endcase
; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
    'TIME Fit'     : begin
  
   wx = where(info.onoff eq 1,cx)
   for xx=0,cx-1 do begin
      star = wx(xx)

    w = where((info.lc.mag(star) gt 5. and info.lc.mag(star) lt 25.) and $
              (info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
              (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
              (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 1200.0, c)

     time = info.lc.hjd
            
     fit    = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

  flux = info.lc(w).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,w) - $
         info.dec.appscatfit * info.dec.sfit(star,w) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,w) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,w) - $  ; FWHM poly fit
         info.dec.appxyfit   * info.dec.xfit(star,w)      ; XY Decorrelation

  w2 = where(abs(flux) le info.d(star).noi * 10.,c2) ; data within 10 sigma of noise
  flux = flux(w2) &  w = w(w2) & c = c2

            xmin = min(time(w)) & xmax = max(time(w)) & rax = xmax - xmin ; - 1. / 15.002

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                1,  $ ; update 0th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

            dt = (time(w(1:c-1)) - time(w(0:c-2)))
            dt_lim = 0.03 ; median(dt) * 10.
            wjump = where(dt gt dt_lim,cjump)

            wjump2 = lonarr(cjump+1)
            wjump2(1:cjump) = wjump
            npt = n_elements(wjump2)

            ;p_pday = 15.002 ; points per day
            ;npt = round(rax * p_pday) 

            p_pday = cjump / rax

            box_wid = 0.
            print,' %%% Suggested box width in time (days): ', box_wid
            print,' %%% Hit any key to accept... type another number to change:'
            box_inp = ''  &  read, box_inp
            if box_inp ne '' then box_wid = float(box_inp)
            
            
            print,' %%% Time range in days: ' + string(rax,format='(F6.2)')
            print,' %%% Points per day: ' + string(p_pday,format='(F6.1)')
            print,' %%% Points in time fit: ' + string(npt)

            fittime = fltarr(3,npt) & fittime(1,*) = 99.9
            timestep = rax / (npt-1.) & border = 0.0 + box_wid ; timestep * 0.00
            fittime(0,*) = time(w(wjump2))

;            fittime(0,*) = findgen(npt) * timestep + xmin ; x-axis 
             col=getcolor(/load)

            for i=0,npt-2 do begin
              s = where(time(w) ge (fittime(0,i)  -border) and $
                        time(w) lt (fittime(0,i+1)+border),cs)
              if cs ge 10 then begin
                 resistant_mean,flux(s),3,me,sd,nr
                 fittime(1,i) = me
                 fittime(2,i) = median(time(w(s)))
                 oplot,time(w(s))-info.t0,flux(s)*1000.,psym=3,col=col.red
              endif

             if fittime(2,i) gt info.t1 and fittime(2,i) lt info.t2 then begin
               plots,fittime(0,i)-border-info.t0,!y.crange
               plots,fittime(0,i+1)+border-info.t0,!y.crange,line=2
               plots,fittime(2,i)-info.t0,fittime(1,i)*1000.,col=col.red,psym=6,symsi=2
             ; print,' Hit ... ' & s = get_kbrd(1)
             endif

            endfor
            fittime(1,i) = fittime(1,i-1) ; last point...

 ;           print,' %%% Time range in days: ' + string(rax,format='(F6.2)')
 ;           print,' %%% Points per day: ' + string(p_pday,format='(F6.1)')
 ;           print,' %%% Points in time fit: ' + string(npt)
 ;           fittime = fltarr(2,npt) & fittime(1,*) = 99.9
 ;           timestep = rax / (npt-1.) & border = timestep * 0.0
 ;           fittime(0,*) = findgen(npt) * timestep + xmin ; x-axis 
 ;           for i=0,npt-2 do begin
 ;             s = where(time(w) ge (fittime(0,i)  -border) and $
 ;                       time(w) lt (fittime(0,i+1)+border),cs)
 ;             if cs ge 10 then begin
 ;                resistant_mean,flux(s),3,me,sd,nr
 ;                fittime(1,i) = me
 ;             endif
 ;            plots,fittime(0,i)-border-info.t0,!y.crange
 ;            plots,fittime(0,i+1)+border-info.t0,!y.crange,line=2
 ; 
 ;           endfor
 ;           fittime(1,i) = fittime(1,i-1) ; last point...

            wait,2.0

            wok = where(abs(fittime(1,*)) lt 50 and $
                        abs(fittime(2,*)-info.t0) lt 1200.,cok)
            wall = where(abs(info.lc.hjd-info.t0) lt 1200.,call)

            ; Swell method : fit a 1st or 2nd degree polynomial
            degree_pol = 2

;            rr = robust_poly_fit(fittime(2,wok),fittime(1,wok),degree_pol,yfit,sig)
;            info.dec.tfit(star,*) = 0.0 ; reset all 
;            if degree_pol eq 2 then $
;             info.dec.tfit(star,wall) = rr(0) + rr(1) * time(wall) + rr(2) * time(wall)^2.0
;            if degree_pol eq 1 then $
;             info.dec.tfit(star,wall) = rr(0) + rr(1) * time(wall)
;            if degree_pol eq 5 then $
;             info.dec.tfit(star,wall) = rr(0) + rr(1) * time(wall) + rr(2) * time(wall)^2.0 + $
;                                        rr(3) * time(wall)^3.0 + rr(4) * time(wall)^4.0 + $
;                                        rr(5) * time(wall)^5.0 

     orbit_smooth = 3.0
     f_time = reform(fittime(2,wok)-info.t0)
     f_data = smooth(reform(fittime(1,wok)),orbit_smooth,/edge_truncate)
     d_time = reform(time(w)-info.t0)

     time_correction = interpol(f_data,f_time,d_time)
     info.dec.tfit(star,w) = time_correction
     

;    wset,0
;    plot,time(w(w2))-info.t0,flux,psym=3
;    oplot,time(w)-info.t0,info.dec.tfit(star,w),col=col.red
;    oplot,fittime(2,wok)-info.t0,fittime(1,wok),col=col.sky
;    help,time,flux,w,w2,time(w(w2))

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                0,  $ ; update 0th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

           col = getcolor(/load)
           oplot,time(w)-info.t0,info.dec.tfit(star,w),col=col.red

    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                1,  $ ; update 0th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every
          oplot,time(w)-info.t0,info.dec.tfit(star,w)*1000.,col=col.red


;wset,0
;plot,fittime(0,*),fittime(1,*),psym=1

           wait,3.0

        endfor ; next star !

    endcase
     'App TIME Fit' : begin
         info.dec.apptimefit = (info.dec.apptimefit + 1) mod 2
         print,info.dec.apptimefit
        endcase








; ------------------------------------------------------------------------
    'FWHM Fit'     : begin
  
   wx = where(info.onoff eq 1,cx)
   for xx=0,cx-1 do begin
      star = wx(xx)

     decor_star = star ; use the FWHM measured for the star itself?
     print,' %%% Use which star for decorrelation of FWHM ? (star itself = '+string(star,format='(I1)')+')'
     decor_star = get_kbrd(1) & decor_star = fix(decor_star)

    w = where((info.lc.fwhm(decor_star) gt .5) and $
              (info.lc.mag(star) gt 5. and info.lc.mag(star) lt 25.) and $
              (info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
              (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
              (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 500.0, c)


     fwhm = info.lc.fwhm(decor_star)
            
     if c lt 5 then stop ; no valid data points !!!


     fit    = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

  flux = info.lc(w).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,w) - $
         info.dec.appscatfit * info.dec.sfit(star,w) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,w) - $  ; Background spline
         info.dec.apptimefit * info.dec.tfit(star,w) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,w)      ; XY Decorrelation

            xmin = min(fwhm(w)) & xmax = max(fwhm(w)) & rax = xmax - xmin
            npt = 150. & fitfwhm = fltarr(2,npt) & fitfwhm(1,*) = 99.9
            fwhmstep = rax / (npt-1.) & border = fwhmstep * 0.2 ; "BEAM SIZE" = border
            fitfwhm(0,*) = findgen(npt) * fwhmstep + xmin ; x-axis = FWMH values
            for i=0,npt-2 do begin
              s = where(fwhm(w) ge (fitfwhm(0,i)  -border) and $
                        fwhm(w) lt (fitfwhm(0,i+1)+border),cs)
              if cs ge 10 then begin
                 resistant_mean,flux(s),3,me,sd,nr
                 fitfwhm(1,i) = me ; magnitudes
              endif
            endfor

            fitfwhm(1,i) = fitfwhm(1,i-1) ; last point...

            wok = where(fitfwhm(1,*) lt 50,cok)
            wall = where(info.lc.fwhm(decor_star) gt .3 and info.lc.fwhm(decor_star) lt 5.,call)

            ; Swell method : fit a 1st or 2nd degree polynomial
            degree_pol = 2

            rr = robust_poly_fit(fitfwhm(0,wok),fitfwhm(1,wok),degree_pol,yfit,sig)

            info.dec.ffit(star,*) = 0.0 ; reset all 
            if degree_pol eq 2 then $
             info.dec.ffit(star,wall) = rr(0) + rr(1) * fwhm(wall) + rr(2) * fwhm(wall)^2.0

;            if degree_pol eq 1 then $
;             info.dec.ffit(star,wall) = rr(0) + rr(1) * fwhm(wall)
;            if degree_pol eq 5 then $
;             info.dec.ffit(star,wall) = rr(0) + rr(1) * fwhm(wall) + rr(2) * fwhm(wall)^2.0 + $
;                                        rr(3) * fwhm(wall)^3.0 + rr(4) * fwhm(wall)^4.0 + $
;                                        rr(5) * fwhm(wall)^5.0 

            info.dec.ffit(star,w) = interpol(fitfwhm(1,*),fitfwhm(0,*),fwhm(w))

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                6,  $ ; update 6th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

help,fitfwhm
             col=getcolor(/load)

oplot,fwhm(w),info.lc(w).mag(star) - info.d(star).mmag, psym=3,col=col.yellow
window,0
plot,fitfwhm(0,*),fitfwhm(1,*),psym=-7,symsi=.4,col=col.sky,xr=[1.9,2.2],yr=[-1,1]*.1
oplot,fwhm(w),info.lc(w).mag(star) - info.d(star).mmag, psym=3,col=col.yellow

             oplot,fitfwhm(0,*),fitfwhm(1,*),col=col.sky,psym=1,symsi=1.0
             oplot,fwhm,info.dec.ffit(star,*),col=col.red,psym=3


             wait,3.0

        endfor ; next star !

    endcase
     'App FWHM Fit' : begin
         info.dec.appfwhmfit = (info.dec.appfwhmfit + 1) mod 2
         print,info.dec.appfwhmfit
        endcase






     'XY Fit'       : begin


       wx = where(info.onoff eq 1,cx)
       for xx=0,cx-1 do begin
        star = wx(xx)

       mode_xyfit = 'use_x_y_positions'
;       if star ne 0 then mode_xyfit = 'use_angle' ; old data?

; DATA USED FOR XY FIT:
       w = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
                 (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
                 (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               abs(info.lc.hjd-info.t0) lt 1200.0, c)

; ALL DATA THAT WILL OBTAIN A CORRECTION (DEPENDING ON X,Y POSITION):
       w2 = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
                  (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
                  abs(info.lc.hjd-info.t0) lt 1200.0, c2)
          
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

           flux = info.lc(w).mag(star) - info.d(star).mmag - $
             fitsub * fit(star,w) - $               ; Fitted light curve
             info.dec.appbackfit * info.dec.bfit(star,w) - $  ; Background spline
             info.dec.appfwhmfit * info.dec.ffit(star,w) - $  ; FWHM poly fit
             info.dec.apptimefit * info.dec.tfit(star,w) - $  ; TIME splinefit
             info.dec.appscatfit * info.dec.sfit(star,w)      ; Scat light spline

           fluxall = info.lc(w2).mag(star) - info.d(star).mmag - $
             fitsub * fit(star,w2) - $               ; Fitted light curve
             info.dec.appbackfit * info.dec.bfit(star,w2) - $  ; Background spline
             info.dec.appfwhmfit * info.dec.ffit(star,w2) - $  ; FWHM poly fit
             info.dec.apptimefit * info.dec.tfit(star,w2) - $  ; TIME splinefit
             info.dec.appscatfit * info.dec.sfit(star,w2)      ; Scat light spline

if mode_xyfit eq 'use_angle' then begin
         angle = info.lc.angle(star)

         number_of_points_per_pixel = 4.

         xmin = min(angle(w)) & xmax = max(angle(w)) & rax = xmax - xmin
         dss = ceil(rax) * number_of_points_per_pixel 
         npt = dss & fitxxyy = fltarr(2,npt)

         xxyystep = rax / (npt-1.) & border = xxyystep * 0.3
         fitxxyy(0,*) = findgen(npt) * xxyystep + xmin ; x-axis 
         for i=0,npt-2 do begin
           s = where(angle(w) ge (fitxxyy(0,i)  -border) and $
                     angle(w) lt (fitxxyy(0,i+1)+border),cs)
           if cs ge 5 then begin
              resistant_mean,flux(s),3,me,sd,nr
              fitxxyy(1,i) = me
           endif
          
         endfor
         fitxxyy(1,i) = fitxxyy(1,i-1) ; last point...
         
         info.dec.xfit(star,*) = 0.0 ; reset all 
         info.dec.xfit(star,w) = interpol(fitxxyy(1,*),fitxxyy(0,*),angle(w))


    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                4,  $ ; update 6th window only!
                info.decor, info.dec,$
                info.colon, 0, every=info.every

             col=getcolor(/load)
             plotsym,0,/fill
             oplot,fitxxyy(0,*),fitxxyy(1,*),col=col.sky,psym=8,symsi=2.0
             oplot,angle,info.dec.xfit(star,*),col=col.red,psym=3
             wait,2.0
endif


if mode_xyfit eq 'use_x_y_positions' then begin
            pos = info.lc(w).gc(*,star)
            pos(0,*) = info.lc(w).x(star)
            pos(1,*) = info.lc(w).y(star)

            posall = info.lc(w2).gc(*,star)
            posall(0,*) = info.lc(w2).x(star)
            posall(1,*) = info.lc(w2).y(star)

            finegrid_size = 150.0 ; 15 = each pix is 15x15 subpix points
          ;  if star ge 1 then finegrid_size = 1.0 ; secondary targets

           wire_fin_decor, flux, pos, flux2, $
                            fluxall, posall, flux2all, $
                            posmap, map, err, finegrid_size, 99, status

            if status eq 1 then begin ; fit successful
              info.dec.xfit(star,*) = 0.0 ; reset previous fits for this star
              info.dec.xfit(star,w2) = fluxall - flux2all ; insert new xy decor fit
            endif
endif

       endfor


      endcase

     'App XY Fit'   : begin
         info.dec.appxyfit = (info.dec.appxyfit + 1) mod 2
         print,info.dec.appxyfit
         endcase
    endcase

;    print,info.dec

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec, $
                info.colon, 0, every=info.every

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!

END
;----------------------------------------------------------------------

;----------------------------------------------------------------------
; Mark some points that will be offset or labelled bad
;----------------------------------------------------------------------
PRO wirep_badpoints, event 
    ; Get info structure!
   Widget_Control, event.top, Get_UValue=info, /No_Copy

      star = where(info.onoff eq 1,c_star)
      if c_star eq 1 then begin
       star = star(0)

      flagg = 16  ; 16 == treated like scattered light points !!

     print, ''
     print,' %%% Entering  "keyboard"  mode ... ' 
     print, ''

     print,' %%% Reset bad points (y/n)? ' & reset = get_kbrd(1)
     if reset eq 'y' then begin

      ; Reset previously set scat light flags:
      wset = where( (info.d(star).w XOR flagg) eq $
                    (info.d(star).w  -  flagg),cset)

      if cset ge 1 then info.d(star).w(wset) = $
                        info.d(star).w(wset) - flagg

       print,' >>> Resetting ' + strcompress(string(cset,format='(I8)'),/remove_all) + $
             ' points!'
     endif

     print,' %%% Possible windows: FWHM (f) ... ' & poss = get_kbrd(1)

     pickmode = ['FWHM']
     wpick = where(strmatch(pickmode,'*'+poss+'*',/fold_case) eq 1,cpick)
     
     if cpick eq 1 then begin ; unique selection ?
     minmax  = '' & read,' %%% Enter min & max value x-val (eg. "  1.821 1.825 "): ',minmax
     minmax2 = '' & read,' %%% Enter min & max value magn. (eg. " -0.005 0.0   "): ',minmax2
     s1 = strsplit(minmax, ' ',/extract) & ns1 = n_elements(s1)
     s2 = strsplit(minmax2,' ',/extract) & ns2 = n_elements(s2)
     minmax  = float(s1) & sord  = sort(minmax)  & minmax  = minmax(sord)
     minmax2 = float(s2) & sord2 = sort(minmax2) & minmax2 = minmax2(sord2)

     if ns1 eq 2 and ns2 eq 2 then begin

     if pickmode(wpick) eq 'FWHM' then begin

     ; Pick valid points:
   w  = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
              (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points (10 sigma data?)
               info.lc.hjd ge info.d(star).ta1 and info.lc.hjd le info.d(star).ta2,c) ; any valid pts?
     print,' %%% Number of valid points (ALSO scat light points): ',c

     fit = *info.fit & fit = fit(star,*) 
     if n_elements(fit) eq c then fit = fit(w) else fit = 0.

     y = info.lc(w).mag(star) - info.d(star).mmag - $
         info.fitsub * fit - $                            ; Fitted light curve
         info.dec.appfwhmfit * info.dec.ffit(star,w) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,w) - $  ; TIME spline fit
         info.dec.appbackfit * info.dec.bfit(star,w) - $  ; Background spline
         info.dec.appscatfit * info.dec.sfit(star,w) - $  ; Scat light spline
         info.dec.appxyfit   * info.dec.xfit(star,w)      ; XY Decorrelation
     x = info.lc(w).fwhm(star)

     wacc = where(  y ge minmax2(0) and y le minmax2(1) and $
                    x ge minmax(0)  and x le minmax(1), cacc)
     print,' %%% Points that will be marked: ' + $
              strcompress(string(cacc,format='(I8)'),/remove_all)


     y_ran      = max([(info.d(star).y),1e-4])   ; Y range for magnitude plots
;     wset,0
     plot,x,y,psym=1,symsi=.2,yr=y_ran * 1. * [-1,1.]
     col = getcolor(/load)
     if cacc ge 2 then oplot,x(wacc),y(wacc),psym=4,symsi=.3,col=col.red
     wait,2

     endif ; FWHM mode

      print,' %%% Pick mode : ' + pickmode(wpick)

      print,' %%% What do you want to do to these data points: '
      print,' %%% Scat points (p), Bad points (b), Add offset (a): '
      ll = get_kbrd(1)
      if ll eq 'p' then flagg = 16
      if ll eq 'b' then flagg =  8
      if ll eq 'a' then begin
         flagg = 0 & offset = ''
         read, ' %%% Offset to be added: ',offset
         offset = float(offset)
      endif   
    
     ; And finally set the bad point light flags!
     if cacc ge 1 then begin ; only set flag if not already set !
 
        if ll eq 'p' or ll eq 'b' then $
         for jk=0,cacc-1 do $
          if (info.d(star).w(w(wacc(jk))) XOR flagg) NE (info.d(star).w(w(wacc(jk))) - flagg) then $
          info.d(star).w(w(wacc(jk))) = ( info.d(star).w(w(wacc(jk))) + flagg ) ; Set flag

        if ll eq 'a' then $
          info.lc(w(wacc)).mag(star) = info.lc(w(wacc)).mag(star) + offset

     endif ; any points to modify?

     endif else print,' *** Illegal values entered !!!' ; max / min x-range selected
     endif ; unique selection of mode?



    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec, $
                info.colon, 0, every=info.every

 endif else print,' %%% Select ONLY ONE star for plotting! ' ; any star selected for plotting?

   Widget_Control, event.top, Set_UValue=info, /No_Copy 
    ; Return the info structure!




END
;----------------------------------------------------------------------



;----------------------------------------------------------------------
PRO wirep_colon, event
    ; Get info structure!
   Widget_Control, event.top, Get_UValue=info, /No_Copy
    ; Get the button value!
    ; Widget_Control, event.id, Get_Value=buttonValue

     info.colon = (info.colon + 1) mod 2

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec, $
                info.colon, 0, every=info.every

   Widget_Control, event.top, Set_UValue=info, /No_Copy 
    ; Return the info structure!

END
;----------------------------------------------------------------------


;----------------------------------------------------------------------
PRO wirep_choose_lc, event

    ; Get info structure!
   Widget_Control, event.top, Get_UValue=info, /No_Copy
    ; Get the button value!
   Widget_Control, event.id, Get_Value=buttonValue

;   print,buttonValue
;   print,info.star
    
   w = where(buttonValue eq info.starname,c)
   if c eq 1 then begin

    info.star = w ; fix(buttonValue)-1L
    info.onoff(w) = (info.onoff(w) + 1) mod 2

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every

   endif

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!

END
;----------------------------------------------------------------------


;----------------------------------------------------------------------
PRO wirep_mark_freq, event
   Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!
   ; Widget_Control, event.id, Get_Value=buttonValue      ; Get the event structure.

  ; Mark freqs. in amplitude/power spectrum:
    info.d.markfreq = (info.d(0).markfreq + 1) mod 2

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit

    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
;----------------------------------------------------------------------

;----------------------------------------------------------------------
PRO wirep_amp_power, event
   Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!
   ; Widget_Control, event.id, Get_Value=buttonValue      ; Get the event structure.

   power = info.d(0).power
   if power eq 1.0 then begin
      power = 2.0 
      powertext = 'Power [ppm!E2!N]'
   endif else begin
      power = 1.0
      powertext = 'Amplitude [ppm]'
   endelse

   info.d.power = power
   info.d.powertext = powertext


    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit

    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
;----------------------------------------------------------------------

;----------------------------------------------------------------------
PRO wirep_weights, event
   Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!
   Widget_Control, event.id, Get_Value=buttonValue      ; Get the event structure.

   case buttonValue of
    'Wei_Stetson'    : info.wei_stetson  = (info.wei_stetson  + 1) mod 2
    'Wei_PTP'        : info.wei_ptp      = (info.wei_ptp + 1)      mod 2
   endcase

    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!
END
;----------------------------------------------------------------------

;----------------------------------------------------------------------
PRO wirep_fit_button, event

; 'Sub Fit' button ---->

   Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!
   Widget_Control, event.id, Get_Value=buttonValue      ; Get the event structure.
    
   case buttonValue of
    'Plot Fit' : info.fiton  = (info.fiton  + 1) mod 2
    'Sub Fit'  : info.fitsub = (info.fitsub + 1) mod 2
   endcase

if info.fitsub then print,' %%% Imported modes are subtracted' else $
 print,' %%% Imported modes are NOT subtracted!'

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit

    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every ; , yyy=1


    Widget_Control, event.top, Set_UValue=info, /No_Copy ; Return the info structure!

END
;----------------------------------------------------------------------

;----------------------------------------------------------------------
FUNCTION What_Button_Pressed, event

   ; Checks event.press to find out what kind of button
   ; was pressed in a draw widget.  This is NOT an event handler.

button = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
Return, button(event.press)
END 
;----------------------------------------------------------------------



; -----------------------------------------------------------------------
FUNCTION What_Button_Released, event

   ; Checks event.press to find out what kind of button
   ; was released in a draw widget.  This is NOT an event handler.

button = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']
Return, button(event.release)
END 
; -----------------------------------------------------------------------


; -----------------------------------------------------------------------
PRO wirep_button, event

; Get info structure!
Widget_Control, event.top, Get_UValue=info, /No_Copy 
   
; Get the name of the button that was pressed:  
Widget_Control, event.id, Get_Value=buttonValue

if buttonValue eq 'Rem Scat' then begin

if strmatch(info.fil,'*altair*') eq 1 then begin
 ph1 = -.510
 ph2 = -0.22 & ph3 = .495
 ph4 = 0.79
endif

if strmatch(info.fil,'*HD113226*') eq 1 then begin
 ph1 = 0.55
 ph2 = 0.9 & ph3 = 0.9
 ph4 = 0.8
 ; per1 = 15.0477215 ; HD113226
endif

if strmatch(info.fil,'*beta_UMi*') eq 1 then begin
 ph1 = -0.51
 ph2 = 0.9 & ph3 = 0.9
 ph4 = 0.8
  ; stop
endif


if n_elements(ph1) eq 0 then RETURN

   for i=0,info.nstar-1 do begin
      per1 = info.per1
      pha = ((info.lc.hjd-info.t0) mod (1./per1)) * per1
      wx = where( ((pha lt ph1) or $
                   (pha gt ph2 and pha lt ph3) or (pha gt ph4)),cx) 

      if cx ge 1 then $
       info.d(i).w(wx) = info.d(i).w(wx) XOR 2 ; non-scattered light points

       ; debug: phase limits found using wire_phase.pro
       ; stop & col=getcolor(/load)
       ; plot,pha,info.lc.mag(0)-info.d(0).mmag,psym=3,yr=[-1,1]*info.d(0).y,xr=[-1,1]
       ; oplot,pha(wx),info.lc(wx).mag(0)-info.d(0).mmag,psym=3,col=col.red

       ;  w = where(info.lc.hjd gt 40000. and $
       ;            (info.lc.mag(0) gt 14.1) and (info.lc.mag(0) lt 14.2),c ) ; and $
                 ; (pha gt ph1 and pha lt ph4), c)
       ;  plot,info.lc(w).hjd,info.lc(w).mag(0),psym=3
       ; openw,1,"/ai39/bruntt/wire/beta_UMi/beta_UMi.dat"
       ;  for i=0L,c-1 do $
       ;   printf,1,info.lc(w(i)).hjd,info.lc(w(i)).mag(0),$
       ;    format='(D15.7,X,F11.7)'
       ; close,1

    endfor ; next star
endif

if strmatch(buttonValue,'FFT*') eq 1 then begin

    cg = 0L
    if buttonValue eq 'FFT Erase' then begin ; amplitude spectrum!
      werase = where(info.onoff eq 1,c_erase)
      for l=0,c_erase-1 do begin
         l2 = werase(l)
         info.sp(l2).calc = 0 ; reset old calc!
         info.sp(l2).freq = -10.0
         info.sp(l2).amp  = -10.0
         info.sp(l2).pha  = -10.0     
      endfor
    endif

    if buttonValue eq 'FFT All' then begin ; amplitude spectrum!

     w = where(info.onoff eq 1,c) & if c eq 0 then RETURN
     star = w(0) ; pick the first star in the list if there are several stars!

 print,' STAR ==== ',star

    wg = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
               (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
               (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
                abs(info.lc.hjd-info.t0) lt 1200.0, cg)

     fit    = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0

    tt2  = info.lc(wg).hjd - info.t0
    dd   = info.lc(wg).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wg) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wg) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wg) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wg) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wg) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wg)      ; XY Decorrelation

; Remove bad points:
    mmag = median(dd)
    wg2  = where(abs(dd - mmag) lt 10. * info.d(star).noi,cg2)
    wg3  = wg(wg2) & cg = cg2
    dd   = dd(wg2)
    tt2  = tt2(wg2)
    dat2 = dd - median(dd)

     ; plot,info.lc(wg).mag(star)-info.d(star).mmag, info.d(star).wei(wg),psym=3

;     wei_stet = info.d(star).wei(wg3)     & wei_stet = wei_stet / total(wei_stet)
;     wei_ptp  = info.d(star).wei_ptp(wg3) & wei_ptp  = wei_ptp   / total(wei_ptp)

     gain = 15.0 ; electrons pr. ADU
     avg_fwhm = median(info.lc(wg3).fwhm(star))
     err_ff = 1000000. * gain
     counts = 10.^((25. - info.lc(wg3).mag(star)) / 2.5) * gain
     sky = fltarr(n_elements(wg3))
     if tag_exist(lc,'back') then sky = info.lc(wg3).back(star) * gain  
     ron = 5.0
     r_aperture = avg_fwhm

                                ; Variance in Ap. phot according to
                                ; Kjeldsen & Frandsen, PASP 104, 413, 192:
     var_ptp = (2. * alog(2.) ) / (avg_fwhm^2.0 * !PI * err_ff) + $
               1.0 / counts + $
               !PI * (r_aperture^2.0) * ( (sky + 5.0^2.0) / (counts^2.0) )
     wei_ptp = 1./var_ptp & wei_ptp = wei_ptp / total(wei_ptp)

 
     wire_stetson, dat2, wei, 0 ; Stetson Weights
     wei_stet = wei / total(wei)
     wei_ptp  = fltarr(cg2) & wei_ptp = 1.0 / float(cg2) ; even weights

     if info.wei_stetson eq 0 then wei_stet(*) = 1.0 ; WEIGHT FLAG OFF?
     if info.wei_ptp     eq 0 then wei_ptp(*)  = 1.0 ; WEIGHT FLAG OFF?
     ; WEIGHTS === PRODUCT OF WEIGHTS 
     wei2 = wei_stet * wei_ptp & wei2 = wei2 / total(wei2)     
     minfreq2 = info.freq1 & maxfreq2 = info.freq2

     wb = where(wei2 lt max(wei2) * 0.5,cb)
     col=getcolor(/load)
     plot,dat2,wei2/max(wei2),psym=3,xsty=1,xtit='Magn.',ytit='Weights',ysty=3
     if cb ge 2 then oplot,dat2(wb),wei2(wb)/max(wei2),col=col.red,psym=3
     ; print,' Hit any key! ' & s = get_kbrd(1)
     ; plot,tt2,dat2,psym=3
     ; if cb ge 2 then oplot,tt2(wb),dat2(wb),col=col.red,psym=3

   endif ; FFT all ended


    if buttonValue eq 'FFT' then begin ; amplitude spectrum of zoomed data points!
     w = where(info.onoff eq 1,c) & if c eq 0 then RETURN
     star = w(0) ; pick the first star in the list if there are several stars!

    wg = where((info.d(star).w XOR 1) eq (info.d(star).w-1) and $ ; good data
               (info.d(star).w XOR 8) ne (info.d(star).w-8) and $ ; bad points 
               (info.d(star).w XOR 16*info.dec.remscat) ne $
                                   (info.d(star).w-16) and $ ; scat light
               (info.lc.hjd) ge info.t1 and (info.lc.hjd le info.t2),cg)
                
     if cg ge 20 then begin
     fit = *info.fit
     fitsub = info.fitsub
     if n_elements(fit(star,*)) ne n_elements(info.lc.mag(star)) then fitsub = 0.

      fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
      tt2  = info.lc(wg).hjd - info.t0
  dat2 = info.lc(wg).mag(star) - info.d(star).mmag - $
         fitsub * fit(star,wg) - $                    ; Fitted light curve
         info.dec.appscatfit * info.dec.sfit(star,wg) - $  ; Scat light spline
         info.dec.appbackfit * info.dec.bfit(star,wg) - $  ; Background spline
         info.dec.appfwhmfit * info.dec.ffit(star,wg) - $  ; FWHM poly fit
         info.dec.apptimefit * info.dec.tfit(star,wg) - $  ; TIME splinefit
         info.dec.appxyfit   * info.dec.xfit(star,wg)      ; XY Decorrelation
      dat2 = dat2 - median(dat2)

   ; Remove bad points:
     mmag = median(dat2)
     wg2  = where(abs(dat2 - mmag) lt 10. * info.d(star).noi,cg2)
     wg   = wg(wg2) & cg = cg2
     dat2 = dat2(wg2)
     tt2  = tt2(wg2)
     dat2 = dat2 - median(dat2)

;      wei_stet = info.d(star).wei(wg)     & wei_stet = wei_stet / total(wei_stet)
;      wei_ptp  = info.d(star).wei_ptp(wg) & wei_ptp  = wei_ptp  / total(wei_ptp)

     wire_stetson, dat2, wei, 0 ; Stetson Weights
     wei_stet = wei / total(wei)
     wei_ptp  = fltarr(cg2) & wei_ptp = 1.0 / float(cg2) ; even weights

      if info.wei_stetson eq 0 then wei_stet(*) = 1.0
      if info.wei_ptp     eq 0 then wei_ptp(*)  = 1.0
      wei2 = wei_stet * wei_ptp & wei2 = wei2 / total(wei2)

     wb = where(wei2 lt max(wei2) * 0.5,cb)
     col=getcolor(/load)
     plot,dat2,wei2/max(wei2),psym=3,xsty=1,xtit='Magn.',ytit='Weights',ysty=3
     if cb ge 2 then oplot,dat2(wb),wei2(wb)/max(wei2),col=col.red,psym=3
     wait,2
     ; print,' Hit any key! ' & s = get_kbrd(1)
     plot,tt2,dat2,psym=3
     if cb ge 2 then oplot,tt2(wb),dat2(wb),col=col.red,psym=3



      minfreq2 = info.freq1 & maxfreq2 = info.freq2
  endif ; FFT button ended
 endif ; FFT buttonS ended


    ; --------------------------------------------------------------------------
    cok = 0 ; default == FFT calc. failed!
    np = n_elements(wei2)

;    help,np

    if np ge 25 and cg ge 25 then begin
     print,' %%% Calculating FFT in the frequency range: ',minfreq2,maxfreq2
     freq = 0. & amp = 0. & phase = 0.
     ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase
     namp = n_elements(amp)
     if namp ge 100 then begin
     wok = where(strcompress(string(amp),/remove_all) ne 'NaN',cok) ; Remove NaN!
      if cok ge 2 then begin
       amp = amp(wok) & freq = freq(wok) & phase = phase(wok)
      endif
     endif

; help,info.sp,/str

      w = where(info.sp(star).calc eq 0,c) ; look for free slots!
      if c ge 1 then s = w(0) else begin ; no free slots?
         s = where(info.sp(star).calc eq max(info.sp(star).calc),cs)
         s = s(0)
         info.sp(star).calc(s) = 0 ; reset old calc!
         info.sp(star).freq(s,*) = -10.0
         info.sp(star).amp(s,*)  = -10.0
         info.sp(star).pha(s,*)  = -10.0     
      endelse

      if cok ge 15 then begin
         n_space = n_elements( info.sp(star).freq(s,*) )
         c_store = cok
         if n_space lt cok then begin
           print,' %%% Too little room for ampl. spectrum in info struct: ',n_space, ' vs. ', cok
           c_store = n_space
           print,' %%% Storing the first ',c_store,' data points!'
         endif

         info.sp(star).freq(s,*) = -1.0 ; default = bad points
         info.sp(star).freq(s,0:c_store-1) = freq(0:c_store-1) * info.fac
         info.sp(star).amp(s,0:c_store-1)  = amp(0:c_store-1)
         info.sp(star).pha(s,0:c_store-1)  = phase(0:c_store-1)
   
         info.sp(star).x1 = minfreq2 ; plotting range in x,y
         info.sp(star).x2 = maxfreq2
         info.sp(star).y1 = 0.0
         info.sp(star).y2 = max(info.sp(star).amp) * 1.1 ; max of ALL calc. spectra!
   
         wu = where(info.sp(star).calc ge 1,cu)
         if cu ge 1 then $
          info.sp(star).calc(wu) = info.sp(star).calc(wu) + 1
          info.sp(star).calc(s)  = info.sp(star).calc(s) + 1            
     endif

;wset,0
;ff = info.sp(star).freq(0,*)
;aa = info.sp(star).amp(0,*)
;plot,ff,aa,yr=[0,100],$
; xr = [30,50],xsty=1,ysty=1
;w = where(ff gt 30 and ff lt 50 and aa gt 0 and aa lt 1e6,c)
;mm = median(aa(w))
;print,' ppm for real ',mm

   endif ; any points : np & cg > 25

    ; --------------------------------------------------------------------------

endif



if strmatch(buttonValue,'<*') eq 1 or $
   strmatch(buttonValue,'*>') eq 1 then begin
ra_t = info.t2 - info.t1 ; range in time to zoom plot
w = where( (info.d(0).w XOR 8) ne (info.d(0).w - 8),c) ; valid points
if buttonValue eq '|>' then begin
 tx = max(info.lc(w).hjd) 
  new_t1 = tx - ra_t
  new_t2 = tx 
endif
if buttonValue eq '<|' then begin
 tx = min(info.lc(w).hjd) 
  new_t1 = tx 
  new_t2 = tx + ra_t
endif
if buttonValue eq '>' then begin
 step_t = ra_t * 0.5
  new_t1 = info.t1 + step_t
  new_t2 = info.t2 + step_t
endif
if buttonValue eq '<' then begin
 step_t = ra_t * 0.5
  new_t1 = info.t1 - step_t
  new_t2 = info.t2 - step_t
endif
if buttonValue eq '>>>' then begin
 step_t = ra_t * 1.0
  new_t1 = info.t1 + step_t
  new_t2 = info.t2 + step_t
endif
if buttonValue eq '<<<' then begin
 step_t = ra_t * 1.0
  new_t1 = info.t1 - step_t
  new_t2 = info.t2 - step_t
endif

; Make sure the new times are ok
mx_t = max(info.lc(w).hjd)
mi_t = min(info.lc(w).hjd)
 if new_t2 gt mx_t then begin
  new_t1 = mx_t - ra_t
  new_t2 = mx_t
 endif
 if new_t1 lt mi_t then begin
  new_t1 = mi_t 
  new_t2 = mi_t + ra_t
 endif
info.t1 = new_t1 & info.t2 = new_t2
endif                       ; end of time zoom buttons


    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every
 
; Return info structure!
Widget_Control, event.top, Set_UValue=info, /No_Copy

END
; -----------------------------------------------------------------------


; -----------------------------------------------------------------------
PRO wirep_Events, event

   ; Error handling -- courtesy of David Fanning, www.dfanning.com
Catch, theError

IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   IF !Error_State.Code EQ -167 THEN BEGIN
      ok = Error_Message('A required value is undefined.')
   ENDIF ELSE BEGIN
      ok= Error_Message()
  ENDELSE
   IF N_Elements(info) NE 0 THEN $
      Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

Widget_Control, event.top, Get_UValue=info, /No_Copy ; Get info structure!

eventName = Tag_Names(event, /Structure_Name)

    IF eventName EQ 'WIDGET_DRAW' THEN BEGIN

     w = where(event.id eq info.wid.id,c) ; which window was clicked?
     w = w(0)

print,' %%% w = ' + strcompress(w)

   ; Print "YES" in the plot window ...
     wset,info.wid(w).win
        xyouts,.5,.5,/normal,'YES'+string(w,format='(I2)'),charsi=3,charthick=3
    
      possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
      mouseEvent = possibleEventTypes(event.type)

      ; Turn motion events off.
       Widget_Control, event.id, Draw_Motion_Events=0
    
    CASE mouseEvent of
 
    'UP': BEGIN

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
      wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every

  ENDCASE

     'DOWN': BEGIN ; STORE NEW LIMITS!

       buttonPressed = What_Button_Pressed(event)

    fit2d = *info.fit2d & fit2t = *info.fit2t & fit = *info.fit
      wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                w,  info.decor, info.dec,$
                info.colon, 0, every=info.every

   rr = 0   
   IF buttonPressed EQ 'RIGHT' THEN rr = 1

;      status = tag_exist(event, 'x') 
;      if status eq 0 then print,' event.x did not exist ... '
;      if status eq 1 then begin

    ; Convert the x device coordinates to data coordinates.
       x = [info.v_plot_x_siz, event.x]
       y = [info.v_plot_y_siz, event.y]
       coords = Convert_Coord(x, y, /Device, /To_Data)
       x_mouse = coords(0,1) & y_mouse = coords(1,1)        

       print,' >>> Mouse values:  x: ',x_mouse, ' y:',y_mouse, $
             ' window: w = ' + string(w,format='(I2)')
       

; --------------------------------------------------------------------------
    if w eq 0 or w eq 1 then begin ; mouse clicked lc windows
; --------------------------------------------------------------------------
        ; What was the original time range:
           t0 = info.t0 & t1 = info.t1 & t2 = info.t2
    
           if (x_mouse lt (t1-t0)) and (x_mouse lt (t2-t0)) then t1 = x_mouse + t0
           if (x_mouse gt (t1-t0)) and (x_mouse gt (t2-t0)) then t2 = x_mouse + t0
           if (x_mouse gt (t1-t0)) and (x_mouse lt (t2-t0)) then begin
            if rr eq 0 then t1 = x_mouse + t0 ; left  mouse button
            if rr eq 1 then t2 = x_mouse + t0 ; right mouse button
           endif
    
        ; Insert new time range if t1 < t2:
          if t1 lt t2 then begin
              info.t1 = t1 
              info.t2 = t2 
          endif else begin
            print,'Illegal limits ...'
          endelse

           ; print,info.t2-t0,info.t1-t0

    endif
; --------------------------------------------------------------------------

; --------------------------------------------------------------------------
    if w eq 2 then begin ; FFT window clicked
; --------------------------------------------------------------------------
            freq1 = info.freq1 & freq2 = info.freq2
            print,freq1,freq2
            if rr eq 0 then freq1 = x_mouse/info.fac  ; left  mouse button
            if rr eq 1 then freq2 = x_mouse/info.fac  ; right mouse button
            print,freq1,freq2
               
        ; Insert new time range if t1 < t2:
          if freq1 lt freq2 then begin
              info.freq1 = freq1
              info.freq2 = freq2
          endif else begin
            print,'Illegal limits ... return to defaults'
              info.freq1 = 0.0  ; min(info.sp.freq)
              info.freq2 = 1000.0 ; max(info.sp.freq) .. unit = c/day
          endelse
    endif
; --------------------------------------------------------------------------

;    if w eq 3 then begin
;      ; print,' Decor window 1 clicked ... '
;    endif
;    if w eq 4 then begin
;      ; print,' Decor window 2 clicked ... '
;    endif

   ; --------------------------------------------------------
   ;   MARK SCATTERED LIGHT LIMITS ...
   ; --------------------------------------------------------
    if w eq 7 then begin
      w_sel_star = where(info.onoff eq 1,c_sel_star)
      if c_sel_star ge 1 then begin
        
; Left mouse button clicked:
        if rr eq 0 and x_mouse lt info.dec.rscat(w_sel_star,1) then $
                info.dec.rscat(w_sel_star,0) = x_mouse          
; Right mouse button clicked:
        if rr eq 1 and x_mouse gt info.dec.rscat(w_sel_star,0) then $
                info.dec.rscat(w_sel_star,1) = x_mouse          
           
        print,info.dec.rscat

        ; Update flags for points to use:
        for st=0,c_sel_star-1 do begin

              period = 1.0 / info.d(0).orb_freq
              mint = min(info.lc.hjd)
              phase = ( (( (info.lc.hjd - mint) mod period ) / period ) + info.d(0).add_phase ) mod 1D
              wacc = $
                where(( phase lt info.dec.rscat(w_sel_star(st),0)) or $
                      ( phase gt info.dec.rscat(w_sel_star(st),1) ), cacc) 

              ; Reset previously set scat light flags:
              wset = where( (info.d(w_sel_star(st)).w XOR 16) eq $
                             (info.d(w_sel_star(st)).w - 16),cset)
              if cset ge 1 then info.d(w_sel_star(st)).w(wset) = $
                                info.d(w_sel_star(st)).w(wset) - 16
 
              ; And finally set the new scat light flags!
              if cacc ge 1 then $
                  info.d(w_sel_star(st)).w(wacc) = $
                  ( info.d(w_sel_star(st)).w(wacc) + 16 ) ; Set flag
        endfor
     endif
 endif ; w = 5 ?
   ; --------------------------------------------------------------



          ENDCASE                         ; END OF DOWN MOUSE EVENT

      'MOTION': BEGIN
       
          ; Most of the action in this event handler occurs here while we are waiting
          ; for an UP event to occur. As long as we don't get it, keep erasing the
          ; old zoom box and drawing a new one.
    
        print,'Motion detected ...'
    
        
      ENDCASE ; END OF MOTION


    ENDCASE ; END OF WIDGET DRAW


   ENDIF ; end of mouse-clicking in the Abund fit window...
   ; --------
    
    ; Return the info structure!
       Widget_Control, event.top, Set_UValue=info, /No_Copy

END 
; =================================================================



PRO wirep_Cleanup, tlb

; The purpose of this procedure is to clean up pointers,
; objects, pixmaps, and other things in our program that
; use memory. This procedure is called when the top-level
; base widget is destroyed.

Widget_Control, tlb, Get_UValue=info, /No_Copy
IF N_Elements(info) EQ 0 THEN RETURN

   ; Free the pointers.

Ptr_Free, info.fit

END ;---------------------------------------------------------------------------


;---------------------------------------------------------------------------
; End of event_handler programs
;---------------------------------------------------------------------------

;                vwa_hx_select_abund, [event.id, event.top], Group_Leader=event.top,$
;                                     sel, wl1, wl2, abund_pars

;---------------------------------------------------------------------------
; Main Program starts here:
;---------------------------------------------------------------------------
PRO wirep, lc, fil, target, decor, f=f, objects=objects, every=every, $
         eclipsing=eclipsing, $
         wiredat=wiredat, thefreq_unit=thefreq_unit, $
         Group_Leader=group_leader ; Group leader of this program.

if n_elements(thefreq_unit) ne 0 then freq_unit = thefreq_unit


phase_on = 1B

; Analysis widget for light curve(s) from WIRE satellite 
; (c) Hans Bruntt 2003/2004

   if n_elements(fil) eq 0 then fil = 'Not Available'
   if n_elements(decor) eq 0 then decor = 0

                                ; Restore a previous reduction run
                                ; (ie. the limits you have set,
                                ; XY-decor function, etc. etc.)
   update_info = 0B
   if n_elements(f) eq 1 then begin
    spawnrob,'ls -1 ' + f,a
     if a(0) ne '' then begin ; file exists ! 
      restore,f ; restore the information (wirep_struct)
      update_info = 1B      ; set flag to update info structure later!
      print,' %%% Restored WIRE reduction file: '+f
     endif
   endif

default9, eclipsing, 0B ; for eclipsing binaries, do not discard data points
                       ; that are eclipses

   ; -----------------------------------------------------------
   ; Check parameters and keywords.
   ; -----------------------------------------------------------
   if n_elements(lc) eq 0 then begin
      print,' %%% No points in lc ...'
      RETURN
   endif

   nstar = n_elements(lc(0).mag) ; number of stars in the result structure
   np = n_elements(lc)           ; number of light curve points

   if n_elements(objects) eq 0 then begin
    objects = replicate('Unknown',nstar)
    for kk=0,nstar-1 do objects(kk) = objects(kk) + strcompress(kk,/remove_all)
   endif


; Get additional information about each star
   obj = replicate({hd:0L, hdnam:'', nam:'', $
                    vsini:-1., spec:'', V:99., BV:9., par:-1.,$
                    lcfile1:'', lcfile2:'', lcfile3:'', $
                    lcfile1_sub:'', lcfile2_sub:'', lcfile3_sub:''},nstar)
   obj.hdnam = objects & obj.nam = 'HD ' + objects
   if n_elements(wiredat) ne nstar then begin
     wiredat = -1
   endif else begin
    obj.hd    = wiredat.hd
    obj.hdnam = wiredat.hdnam
    obj.nam   = wiredat.nam
    obj.vsini = wiredat.vsini
    obj.spec  = wiredat.spec1
    obj.v     = wiredat.v
    obj.bv    = wiredat.bv
    obj.par   = wiredat.par
    obj.lcfile1      = wiredat.lcfile1
    obj.lcfile1_sub  = wiredat.lcfile1_sub 
    obj.lcfile2      = wiredat.lcfile2
    obj.lcfile2_sub  = wiredat.lcfile2_sub 
    obj.lcfile3      = wiredat.lcfile3
    obj.lcfile3_sub  = wiredat.lcfile3_sub 
   endelse

   ; Information on each object:
   for st=0,nstar-1 do print,obj(st)

   m4_get_basedir, base
   wire_red_setup, position_file, target_file, sec = secinfo
   restore,position_file
   restore,secinfo ; wireobj structure ! Added November 2004
   wobj = where(strmatch(wireinfo.object,'*'+target+'*') eq 1,cobj)

   if cobj ge 2 then begin ; added 1st of Nov 2004
    print, ' %%% Target found multiple times:'
    for ll=0,cobj-1 do print,strcompress(ll)+': '+ wireinfo(wobj(ll)).object
    print,' %%% Select the right one: '
    sll = get_kbrd(1)
    wobj = wobj(sll)
    cobj = 1 
   endif

   if cobj ne 1 then begin
    print,' %%% Object not in wireinfo structure ... aborting : ' + target
    help,wobj
    stop
    RETURN
   endif
   t0 = wireinfo(wobj).t0

   a = sort(lc.hjd(0))
   lc = lc(a)          ; sort the times so time increases ...

   ; Frequency unit to use in amplitude plot:
   default9, freq_unit, 'c/day'
   fac = 1.0 ; IF unit = c/day
   if freq_unit eq 'microHz' then fac = 1e6/86400D
   if freq_unit eq 'milliHz' then fac = 1e3/86400D

;   if update_info eq 1 then fac = wirep_struct.fac

; ------------------------------------------------------------------------------
; Specific for wire & altair data:
; ------------------------------------------------------------------------------
freq1 = 0.5 & freq2 = 25.0 ; freq. range in cycles pr day in plots
mark = fltarr(nstar,30) ; freq1, freq2, mark needed in all cases!
t1 = 51480. & t2 = t1 + 1. ; needed if update_info EQ 1

if update_info eq 0 then begin ; read data ...

fil_per = ''
;; if strmatch(fil,'*altair*') eq 1 then fil_per = '~/wire/altair_t0_51480_f7.per'

; Import a FIXED .per file? DEFAULT ---> NO!!!
if fil_per ne '' then begin
   readcol,fil_per, $
           dummy,p98f,p98a,p98p,$
           format='A,D,D,D'
   nfreq = n_elements(p98f) & np = n_elements(lc.hjd)
   fit = fltarr(nstar,np)

   for pp=0, nfreq-1 do begin
    fit(0,*) = fit(0,*) + p98a(pp) * $
     sin(2. * !DPI * p98f(pp) * (lc.hjd-t0) + p98p(pp) * (2. * !DPI)  ) 
    
   mark(0,pp) =  p98f(pp) * fac ; frequencies to mark
   endfor

   wg = where(lc.hjd gt 1000. and lc.mag(0) gt 5.,c)
   wb = where(lc.hjd le 1000. or lc.mag(0) le 5.,cb)
   np2 = np * 2.0
   fit2d = fltarr(nstar,np2)  &  fit2t = fltarr(np2) ; DATA + TIMES ARRAY
   fit2t = min(lc(wg).hjd) + $
    (findgen(np2)/(np2-1.0)) * (max(lc(wg).hjd) - min(lc(wg).hjd)) - t0
   for pp=0, nfreq-1 do $
    fit2d(0,*) = fit2d(0,*) + $
     p98a(pp) * sin(2. * !DPI * p98f(pp) * fit2t + p98p(pp) * (2. * !DPI)  ) 
   fit2t = fit2t + t0
endif ; end of importing freqs.

; ------------------------------------------------------------------------------

; RETTER + BRUNTT results for altair!
if strmatch(fil,'*altair*') eq 1 and n_elements(lc(0).mag) eq 6 then begin
    fil_per = '~/bruntt/wire/altair_retter.per'
   readcol,fil_per, $
           dummy,p98f,p98a,p98p,$
           format='A,D,D,D'
   nfreq = n_elements(p98f) & np = n_elements(lc.hjd)

   fit(5,*) = 0.
   for pp=0, nfreq-1 do $
    fit(5,*) = fit(5,*) + p98a(pp) * $
     sin(2. * !DPI * p98f(pp) * (lc.hjd) + p98p(pp) * (2. * !DPI)  ) 

   ; fit2d, fit2t were defined when altair lc (bruntt reduction) was made above!

   for pp=0, nfreq-1 do $
    fit2d(5,*) = fit2d(5,*) + $
     p98a(pp) * sin(2. * !DPI * p98f(pp) * fit2t + p98p(pp) * (2. * !DPI)  ) 

endif ; end of importing freqs.
; ------------------------------------------------------------------------------
endif

                 ; only import data if wirep_Struct not restored!
; ------------------------------------------------------------------------------


; ------------------------------------------------------------------------------   
   ; Basic pars. for each lc
; ------------------------------------------------------------------------------

   orb_freq = 15.009 ; WIRE craft, 15 c/day
   orb_freq = 15.356 ; WIRE craft, 15 c/day

; Calibration of orbital frequency (autumn 2004):
   orb_freq =  4.12698  + 0.000211202 * t0 ; accurate to within 0.009 (1-sigma)

   power = 1.0 ; amplitude spectrum ... to get power spec, set: power = 2.0
   if power eq 1 then powertext = 'Amplitude [ppm]'
   if power eq 2 then powertext = 'Power [ppm!E2!N]'
   markfreq = 1B

   add_phase = 0.3 ; Added 17 Nov 2004
   d = replicate({add_phase:add_phase, $
                  orb_freq:orb_freq, $
                  power:power, powertext:powertext, $
                  markfreq:markfreq, $
                  noi:0., mmag:0., y:0., $
                  w:lonarr(np), wei:fltarr(np), wei_ptp:fltarr(np), $
                  ta1:0D, ta2:0D, $
                  t1: 0D, t2: 0D}, nstar) 
   
   d.w = 8 ; bad points
   pp_zoom = 6.0

   if strmatch(fil, '*alphaCen_merged_allslots_101*') eq 1 then $
     d.orb_freq = 1D / 0.0666633
   if strmatch(fil, '*AlphaBoo_merged_allslots_*') eq 1 then $
     d.orb_freq = 1D / 0.0664268 

   if strmatch(fil, '*AlphaUMi_merged_31.*') eq 1 then begin
     print,' %%%%%% Removing data for FWHM < 1.8 pix'
     d.orb_freq = 15.354
     freq_unit = 'c/day'
   endif
                      
   if strmatch(fil, '*alphaCen_2004_allslots_31*') eq 1 then begin
     print,' %%%%%% Removing data for FWHM < 1.8 pix'
     d.orb_freq = 15.34
     freq_unit = 'milliHz'
   endif

if update_info eq 0 then begin

   for star=0L,nstar-1 do begin
    wg = where(lc.hjd gt 1000. and lc.mag(star) gt 5.,c)
    if c ge 10 then begin
     lc_good = lc(wg).mag(star)
     wire_stetson, lc_good, wei, 0 ; Stetson Weights
     ; wire_weights, lc(wg), star, wei_ptp ; slow computation!

     d(star).noi     = robust_sigma(lc_good)
     d(star).mmag    = median(lc_good)
     d(star).y       = d(star).noi * pp_zoom ; plot range
     d(star).w(wg)   = 1 ; 
     d(star).wei(wg) = wei
     ; d(star).wei_ptp(wg) = wei_ptp
     d(star).ta1     = min(lc(wg).hjd)
     d(star).ta2     = max(lc(wg).hjd)
     d(star).t1      = lc(wg(0)).hjd
     d(star).t2      = lc(wg(c-1)).hjd
     ; plot,lc.mag(0)-median(lc.mag(0)),wei_ptp,psym=2,xr=[-1,1]*0.01

     if star eq 0 then begin
      ra_time = max(lc(wg).hjd) - min(lc(wg).hjd)
           t1 = median(lc(wg).hjd) - ra_time * 0.10
           t2 = median(lc(wg).hjd) + ra_time * 0.05
     endif
 
    endif ; any good points?
endfor
; ------------------------------------------------------------------------------
; plot,lc.mag(0)-d(0).mmag,d(0).wei,psym=3,xr=[-1,1]*.1


endif  ; only import data if wirep_Struct not restored!
; ------------------------------------------------------------------------------


; ------------------------------------------------------------------------------
; If no artif. lc. were imported:
; ------------------------------------------------------------------------------
if n_elements(fit2d) eq 0 then begin
 fit2d = fltarr(nstar,5) ; 0B
 fit2t = fltarr(nstar,5) ; 0B
 fit   = fltarr(nstar,5) ; 0B
endif

; ------------------------------------------------------------------------------


; -----------------------------------------------------------
; Details on each light curve:
; -----------------------------------------------------------
specmax = 3
maxmodes = 200 ; max number of modes
sp   = replicate({x1:0.0, x2:150.0, y1:0.0, y2:2000.0, $
                  calc:bytarr(specmax), $
                  amp: fltarr(specmax,50000), $
                  freq:fltarr(specmax,50000), $
                  pha: fltarr(specmax,50000), $
                  mark: fltarr(maxmodes)}, nstar)
sp(*).freq = -1.0
for i=0,nstar-1 do sp(i).mark = mark(i,*)

; Number of plots in each direction
   nx = 1
   ny = 3
   if decor eq 1 then begin
      nx = 2 & ny = 4
   endif

; Which lcs to plot
 star = 0
 onoff = bytarr(nstar)
 onoff(star) = 1B

   ; -----------------------------------------------------------
   ; Create a top-level base.
   tlb = Widget_Base(Column=1, $
                     Title='WIRE Light Curve Manager -- (c) 2003-2007 Bruntt Memorial Software', $
                     /Base_Align_Center, MBar=menubarID)
   ; -----------------------------------------------------------

   ; Struct for storing button IDs
    field = {button_fit_onoff:0L,button_fit_subfit:0L, $
             button_scat3:0L, $
             button_scat2:0L,button_dec_xy1:0L,button_dec_xy2:0L, $
             button_timefit:0L, button_backfit:0L, button_fwhmfit:0L, $
             field_zero_phase:0L, $
             field_orb_freq:0L}


   ; Define the File pull-down menu.
   fileID = Widget_Button(menubarID, Value='File')

   ; Define the HELP button
   ;   helpID = $
   ;   Widget_Button(fileID, Value='Help', Event_Pro='wirep_Help', /Separator, /Menu)
   ;   button = Widget_Button(helpID, Value='Wirep', UValue='wirep')
 
   ; Define the Quit button.
   ; Define the Save As pull-down menu.
   importID = $
    Widget_Button(fileID, Value='Import Freq', Event_Pro='wirep_ImportFreq', UValue='.per')

   if phase_on then $
    importID = $
     Widget_Button(fileID, Value='Import Phased (Binary Stars!)',$
        Event_Pro='wirep_Phased', UValue='.pha')


   saveLCAsID = $
    Widget_Button(fileID, Value='Save LC', Event_Pro='wirep_SaveAs',/Separator,/Menu)
    aa = Widget_Button(saveLCAsID, Value='Save T+D+W', UValue='tdw')
    bb = Widget_Button(saveLCAsID, Value='Save T+D+W+F', UValue='tdwf')
    bb = Widget_Button(saveLCAsID, Value='Save DECOR', UValue='tdwfb') ; SAVE ALL INFORMATION! MARCH 2006
    dd9 = Widget_Button(saveLCAsID, Value='Save T+D+W+SMOOTH', UValue='tdwsmooth')

   saveAsID = $
    Widget_Button(fileID, Value='Save All', Event_Pro='wirep_SaveALL', UValue='.idl')
   openFileID = $
    Widget_Button(fileID, Value='Open All', Event_Pro='wirep_OpenALL', UValue='.idl')
   quitID = $
    Widget_Button(fileID, Value='Quit', Event_Pro='wirep_Quit', /Separator)

   ; -----------------------------------------------------------
   ;      LC Related buttons
   ; -----------------------------------------------------------
  
   if decor eq 0 then begin
     over = Widget_Base(tlb, column=1, frame=3)
     zoom_handle2 = Widget_Base(over, column=2, frame=3)
   endif

   over = Widget_Base(tlb, column=2, frame=3)
   lc_handle = Widget_Base(over, row=4, frame=3)

   plotbase = Widget_Base(lc_handle, row=1, frame=3)
    buttonl = Widget_Button(plotbase, Value='<|', Event_Pro='wirep_button')
    buttonl = Widget_Button(plotbase, Value='<<<', Event_Pro='wirep_button')
    buttonl = Widget_Button(plotbase, Value='<', Event_Pro='wirep_button')
    buttonr = Widget_Button(plotbase, Value='>', Event_Pro='wirep_button')
    buttonr = Widget_Button(plotbase, Value='>>>', Event_Pro='wirep_button')
    buttonr = Widget_Button(plotbase, Value='|>', Event_Pro='wirep_button')

   if decor eq 1 then begin
     decor_handle = Widget_Base(over, row=4, frame=3)
   endif


   zoom_handle3 = Widget_Base(lc_handle, column=2, frame=3)
   slider_zoomID = Widget_Slider(zoom_handle3, Value=pp_zoom, Min=1, Max=25, $
     Event_Pro='wirep_Zoom_Slider')
    Widget_Control, slider_zoomID, Set_Slider_Max=25
    Widget_Control, slider_zoomID, Set_Slider_Min=1
    Widget_Control, slider_zoomID, Set_Value=pp_zoom

    plotbase3 = Widget_Base(zoom_handle3, row=1, frame=3)
     button3_1 = $
       Widget_Button(plotbase3, Value='Postscript LC', Event_Pro='wirep_postscript_lc')
   ; -----------------------------------------------------------
    fitbase = Widget_Base(lc_handle, row=1, frame=3, /NonExclusive)

    button_fit_onoff   = $
      Widget_Button(fitbase, Value='Plot Fit', Event_Pro='wirep_fit_button')
    button_fit_subfit = $
      Widget_Button(fitbase, Value='Sub Fit', Event_Pro='wirep_fit_button')

    Widget_Control, button_fit_onoff, Set_Button=0
    Widget_Control, button_fit_subfit, Set_Button=0

    field.button_fit_onoff  = button_fit_onoff
    field.button_fit_subfit = button_fit_subfit

   ; -----------------------------------------------------------


   ; -----------------------------------------------------------
    if decor eq 1 then begin

    decorbase1 = Widget_Base(decor_handle, row=2, frame=3)
    decorbase2 = Widget_Base(decor_handle, row=2, frame=3, /NonExclusive)
    decorbase3 = Widget_Base(decor_handle, column=2, frame=3) ; text fields

    button_scat1 = $
      Widget_Button(decorbase1, Value='Scat Fit', Event_Pro='wirep_decor')
    button_scat1x = $
      Widget_Button(decorbase1, Value='Scat Fit2', Event_Pro='wirep_decor')
    button_scat2 = $
      Widget_Button(decorbase2, Value='App Scat Fit', Event_Pro='wirep_decor')
    button_scat3 = $
      Widget_Button(decorbase2, Value='Rem Scat', Event_Pro='wirep_decor')

    button_back1 = $
      Widget_Button(decorbase1, Value='Back Fit', Event_Pro='wirep_decor')
    button_back2 = $
      Widget_Button(decorbase2, Value='App Back Fit', Event_Pro='wirep_decor')

    button_fwhm1 = $
      Widget_Button(decorbase1, Value='FWHM Fit', Event_Pro='wirep_decor')
    button_fwhm2 = $
      Widget_Button(decorbase2, Value='App FWHM Fit', Event_Pro='wirep_decor')

    button_time1 = $
      Widget_Button(decorbase1, Value='TIME Fit', Event_Pro='wirep_decor')
    button_time2 = $
      Widget_Button(decorbase2, Value='App TIME Fit', Event_Pro='wirep_decor')

    button_dec_xy1 = $
      Widget_Button(decorbase1, Value='XY Fit',     Event_Pro='wirep_decor')
    button_dec_xy2 = $
      Widget_Button(decorbase2, Value='App XY Fit', Event_Pro='wirep_decor')

    field.button_scat2   = button_scat2    ; app scat fit == dec.appscatfit
    field.button_scat3   = button_scat3    ; dec.remscat
    field.button_dec_xy2 = button_dec_xy2  ; app xy fit = dec.appxyfit
    field.button_timefit = button_time2  ; 
    field.button_backfit = button_back2  ; 
    field.button_fwhmfit = button_fwhm2  ; 

    Widget_Control, button_scat1, Set_Button = 0
    Widget_Control, button_scat2, Set_Button = 0
    Widget_Control, button_scat3, Set_Button = 0

    Widget_Control, button_dec_xy1, Set_Button = 0
    Widget_Control, button_dec_xy2, Set_Button = 0

    orb_freq_field = coyote_field(decorbase3, title='Orb Freq', value = d.orb_freq(0), $
     /CR_Only, Event_Pro='wirep_orbital_frequency', decimal=5,xsize=10)
    field.field_orb_freq = orb_freq_field

    zero_phase_field = coyote_field(decorbase3, title='Zero Phase', value = d.add_phase(0), $
     /CR_Only, Event_Pro='wirep_zero_phase', decimal=5,xsize=7)
    field.field_zero_phase = zero_phase_field



    endif
   ; -----------------------------------------------------------

; Choose LC to plot?
   ; starname = ['1','2','3','4','5']

   if update_info eq 1 then lc = wirep_struct.lc

   nstar = n_elements(lc(0).x)
   ;starname = string( indgen(nstar) + 1)
   ;for j=0,nstar-1 do starname(j) = strcompress(starname(j),/remove_all)
   starname = strcompress(obj.hdnam,/remove_all)
   nel = n_elements(starname)

  buttone = $
;    Widget_Base(lc_handle, column=nel, /Exclusive, Event_Pro='wirep_choose_lc',frame=3)
     Widget_Base(lc_handle, column=nel, /NonExclusive, Event_Pro='wirep_choose_lc',frame=3)

   for st=0,nel-1 do begin
    button = Widget_Button(buttone, Value=starname(st))
    if (st eq 0) then Widget_Control, button, Set_Button=1
   endfor

   mainbase = Widget_Base(tlb, row=ny, frame=3)
   allbase  = Widget_Base(tlb, row=1, frame=3)

   ; -----------------------------------------------------------
   ;      FFT Related buttons
   ; -----------------------------------------------------------
     button2_1 = Widget_Button(allbase, Value='FFT', Event_Pro='wirep_button')
     button2_2 = Widget_Button(allbase, Value='FFT All', Event_Pro='wirep_button')
     button2_3 = Widget_Button(allbase, Value='FFT Erase', Event_Pro='wirep_button')

    weightbase = Widget_Base(allbase, column=2, /NonExclusive, frame=3)
     button_weights1   = $
       Widget_Button(weightbase, Value='Wei_Stetson', Event_Pro='wirep_weights')
     button_weights2   = $
       Widget_Button(weightbase, Value='Wei_PTP', Event_Pro='wirep_weights')
     button_weights2   = $
       Widget_Button(weightbase, Value='Amp/Power', Event_Pro='wirep_amp_power')
     button_weights2   = $
       Widget_Button(weightbase, Value='MarkFreq', Event_Pro='wirep_mark_freq')

    yybase = Widget_Base(allbase, column=2, frame=3) 

     button3_1 = $
       Widget_Button(yybase, Value='Postscript Spec', Event_Pro='wirep_postscript_fft')

     button4_1 = $
       Widget_Button(yybase, Value='YRange', Event_Pro='wirep_recalc_noise')

     button5_1 = $
       Widget_Button(yybase, Value='ColorOn', Event_Pro='wirep_colon')
     colon = 0B

     button5_2 = $
       Widget_Button(yybase, Value='BadPoints', Event_Pro='wirep_badpoints')


   ; -----------------------------------------------------------

   ; -----------------------------------------------------------
   ; Size of screen in pixels
   ; -----------------------------------------------------------
   screenSize = Get_Screen_Size()
   geom = Widget_Info(tlb, /Geometry)

   reserve_y_size = 300.0 ; space for buttons, sliders, etc.
   reserve_x_size = 300.0 ; space for additional widget windows
   ; Define draw widgets
   v_plot_x_siz = ceil( (  (screenSize(0)-reserve_x_size) * 0.9 * (1./nx)  ) / 10. ) * 10 
   v_plot_y_siz = ceil( (  (screenSize(1)-reserve_y_size) * 0.75 * (1./ny)  ) / 10. ) * 10
   ; -----------------------------------------------------------

   ; Realize top-level base and all of its children.
   Widget_Control, tlb, /Realize

   ; -----------------------------------------------------------
   ; Main drawing window(s)
   ; -----------------------------------------------------------

   wid = replicate({win:0L, id:0L},nx * ny)
   for i=0,ny-1 do begin
   for j=0,nx-1 do begin
    drawABUNDID = $
     Widget_Draw(mainbase, XSize=v_plot_x_siz, YSize=v_plot_y_siz, Button_Events=1)
    Widget_Control, drawABUNDID, Get_Value=wid_abund
    wid(j*ny+i).win = wid_abund
    wid(j*ny+i).id = drawABUNDID
   endfor
   endfor
   ; -----------------------------------------------------------

   ; Get the model names:
   ; vwa_wirep_get_model_names, filelist, fund, model

   per1 = -99.9
   if strmatch(fil, '*altair*') eq 1 then per1 = 15.009 ; for wire == orbits pr. day
   if strmatch(fil, '*HD113226*') eq 1 then per1 = 15.04772 ; for wire == orbits pr. day
   if strmatch(fil, '*beta_UMi*') eq 1 then per1 = 15.0045 ; for wire == orbits pr. day

   if strmatch(fil, '*alphaUMa',/fold_case) eq 1 then stop ; offset ?

   if strmatch(fil, '*alphaCen*',/fold_case) eq 1 then begin
    freq_unit = 'milliHz'
   endif

   if strmatch(fil, '*Procyon*',/fold_case) eq 1 then begin
    freq_unit = 'milliHz'
;     print,' %%% TARGET IS PROCYON --- COMPUTING LIGHT CURVE OFFSETS'
;     for star=0,4 do begin ; for each star, remove offset. Observ. done 350 days apart!
;      wt1 = where( (lc.hjd - t0) lt 200 and abs(lc.mag(star)) gt .1,ct1)
;      wt2 = where( (lc.hjd - t0) ge 200 and abs(lc.mag(star)) gt .1,ct2)
;      if ct1 ge 1 and ct2 ge 1 then begin
;        off1 = median( lc(wt1).mag(star) )
;        off2 = median( lc(wt2).mag(star) )
;        medoff = avg([off1,off2]) ; typical magnitude
;        lc(wt1).mag(star) = lc(wt1).mag(star) - off1 + medoff
;        lc(wt2).mag(star) = lc(wt2).mag(star) - off2 + medoff
;        ;;; lc = lc(wt1) & np = n_elements(lc) ; remove data from second epoch!
;        print,'Removed offset for two epochs for Procyon: ',star,off1,off2, medoff
;      endif ; any valid data points?
;     endfor
   endif


WSet, wid(0).win
np = n_elements(lc)

dec = {scatfit:0B,appscatfit:0B,$   ; (A) fit to the phase LC (eg. after subtr. of modes)
       remscat:0B,$                 ; (B) borders marked by user
       xyfit:0B,appxyfit:0B, $      ; (C) fit to x,y positions (decorrelation)
       appbackfit:0B, $             ; (D) background decorrelation fit
       appfwhmfit:0B, $             ; (E) FWHM decor. fit
       apptimefit:0B, $             ; (F) TIME spline fit
         sfit:  fltarr(nstar,np), $   ; [A] the fitted scat. curve
         rscat: fltarr(nstar,2), $    ; [B] borders of rem scat !!
         xfit:  fltarr(nstar,np), $   ; [C] the x,y fit !
         ffit:  fltarr(nstar,np), $   ; [E] FWHM
         tfit:  fltarr(nstar,np), $   ; [F] TIME !!
         bfit:  fltarr(nstar,np)}     ; [D] background decorrelation
       
       dec.rscat(*,0) = 0 & dec.rscat(*,1) = 1.


; print,fac

if n_elements(every) eq 0 then every = 1 ; plot only every "every" data point

if strmatch(fil,'*alphaCen*') eq 1 and $
   strmatch(fil,'*2004*') eq 0 then t0 = 51400.000D

   if freq_unit eq 'microHz' then fac = 1e6/86400D
   if freq_unit eq 'milliHz' then fac = 1e3/86400D

; September 2005:
if strmatch(target,'EtaBoo_*') then begin
 freq_unit = 'microHz' & fac = 1e6/86400D
endif

   ; Create info structure to hold information needed in event handler.
   info = {  wid:wid, $
             obj:obj, $ ; information on each object!
             target:target, $
             field:field, $
             eclipsing:eclipsing, $
             every:every, $ ; plot only every "every" data point
             scale_noise:pp_zoom, $
             v_plot_x_siz:v_plot_x_siz, v_plot_y_siz:v_plot_y_siz, $ 
             nx:nx, ny:ny, $
             d:d, lc:lc, $
             sp:sp, $
             fil:fil, $
             starname:starname, $
             star:star, $
             nstar:nstar, $
             t0:t0, $
             t1:t1, t2:t2, $
             per1:per1, $
             onoff:onoff, $
             fit2d:Ptr_New(fit2d), $
             fit2t:Ptr_New(fit2t), $
             fiton:0B, $ ; artificial light curve!
             fit:Ptr_New(fit), $
             fitsub:0B, $
             freq1:freq1, freq2:freq2, $
             fac:fac, $
             specmax:specmax, $
             wei_ptp:0B, wei_stetson:0B, $
             decor:decor, dec:dec, $ ; decorrelation window on/off?
             colon:colon}

; ==================================================
    if update_info eq 1 then $
       wirep_update_info, info, wirep_struct,field, decor=decor, fac=fac
; ==================================================

    fit2d = *info.fit2d & fit2t = *info.fit2t
    fit = *info.fit ; 24 Feb 2005

    wirep_plot, info.wid, info.d, info.lc, info.t0, info.onoff, info.fac, info.sp, $
                info.t1,info.t2, $
                fit2d, fit2t, info.fiton, $
                fit, info.fitsub, $
                info.freq1,info.freq2, $
                -1,  info.decor, info.dec,$
                info.colon, 0, every=info.every

   ; Store the info structure in the top-level base
    Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Register the program, set up event loop. 
   ; Make this program a non-blocking widget.

   XManager, 'wirep', tlb, Event_Handler='wirep_Events', $
      /No_Block,Cleanup='wirep_Cleanup', Group_Leader=group_leader

END
