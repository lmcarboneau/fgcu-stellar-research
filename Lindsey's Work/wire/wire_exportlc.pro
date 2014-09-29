PRO wire_exportlc, lc, hjd, fwhm, pick, fit, t0, mmag, dec, star, $
                   out, $
                   remscat = remscat, outfile = outfile, debug=debug, $
                   rr=rr, orgrr=orgrr

 
default9, outfile, ''
default9, fitsub, 0D
default9, debug, 0B

; Was the flag for removing scattered light points explicitly set ?
if n_elements(remscat) eq 0 then begin 
 remscat = dec.remscat
endif

; Used by wirep.pro and wire_gather.pro to export final light curves

    wg = where((pick XOR 1) eq (pick-1) and $ ; good data
               (pick XOR 8) ne (pick-8) and $ ; bad points 
               (pick XOR 16 * remscat) ne $
                                   (pick-16) and $ ; scat light
                abs(hjd-t0) lt 1200.0, cg)

     if cg ge 10 then begin
      ttm  = hjd(wg) - t0
      fwhm = fwhm(wg) ; FWHM of stellar profile

     fitsub = double(fitsub)
     if n_elements(fit) ne n_elements(lc) then fitsub = 0D


; For calculating weights: subtract any fitted LC !!
    ddwei  = lc(wg) - mmag - $
         1D * fit(wg) - $                    ; Fitted light curve
         dec.appscatfit * dec.sfit(star,wg) - $  ; Scat light spline
         dec.appbackfit * dec.bfit(star,wg) - $  ; Background spline
         dec.appfwhmfit * dec.ffit(star,wg) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,wg) - $  ; TIME splinefit
         dec.appxyfit   * dec.xfit(star,wg)      ; XY Decorrelation

    dd = lc(wg) - mmag - $
         fitsub * fit(wg) - $                    ; Fitted light curve
         dec.appscatfit * dec.sfit(star,wg) - $  ; Scat light spline
         dec.appbackfit * dec.bfit(star,wg) - $  ; Background spline
         dec.appfwhmfit * dec.ffit(star,wg) - $  ; FWHM poly fit
         dec.apptimefit * dec.tfit(star,wg) - $  ; TIME splinefit
         dec.appxyfit   * dec.xfit(star,wg)      ; XY Decorrelation

if debug then $
 print,' %%% Apply SCAT + BACK + FWHM + TIME + XY-POS fits: ',$
  dec.appscatfit,dec.appbackfit,dec.appfwhmfit,dec.apptimefit,dec.appxyfit



       dd    = dd    - median(  dd)
       ddwei = ddwei - median(ddwei)


     rr = robust_sigma(ddwei)
     wg2 = where(abs(ddwei) lt 10. * rr,cg2)
     wg2 = where(abs(ddwei) lt 100. * rr,cg2)

     wg = wg(wg2) & cg = cg2
     dd = dd(wg2)
     ddwei = ddwei(wg2)
     ttm = ttm(wg2)
     fwhm2 = fwhm(wg2)


;stop
;readcol,'~/wire/wire_lc/wire_lc_AlphaAra_Feb2004_s0_HD158427_B2Vne.dat',t,d,w

;col=getcolor(/load)
;plot,ttm,lc-mmag,psym=3,/nodata
;oplot,ttm,dd,psym=3
;oplot,t,d-.02,psym=1,symsi=.3,col=col.sky

; Sanity:
; plot,ttm,lc(wg)-mmag,psym=3,yr=[-1,1]*0.1,xr=[-1,1]
; oplot,ttm,dd,psym=1,symsi=.3,col=col.sky
; oplot,ttm,fit(wg),psym=1,symsi=.3,col=col.red


if debug then begin
col=getcolor(/load)
 plot,hjd-t0,dec.appbackfit * dec.bfit(star,*),xr=[-1,1]*.8,psym=1,symsi=.3,yr=[-1,1]*.2, $
  tit='Corrections: White: BACK  RED: FWHM  GREEN: PHASE',$
  xtit='YELLOW: RAW -- MAGENTA: MASSAGED ( + SUBTRACTED FIT)'
 oplot,hjd-t0,dec.appfwhmfit * dec.ffit(star,*)+0.01,psym=1,col=col.red,symsi=.3
 oplot,hjd-t0,dec.appscatfit * dec.sfit(star,*)-0.01,psym=1,col=col.green,symsi=.3
 oplot,hjd(wg)-t0,dd-.1,psym=1,col=col.magenta,symsi=.3
 oplot,hjd(wg)-t0,lc(wg)-mmag+.1,psym=1,col=col.yellow,symsi=.3
 oplot,hjd(wg)-t0,ddwei-.15,psym=1,col=col.magenta,symsi=.3

; plot,hjd-t0,dec.appfwhmfit * dec.ffit(star,*),xr=[-1,1]*.5,psym=1,symsi=.3,yr=[-1,1]*.02
; plot,hjd-t0,dec.appscatfit * dec.sfit(star,*),xr=[-1,1]*.5,psym=1,symsi=.3,yr=[-1,1]*.02

; FWHM correction vs. FWHM:
; plot,fwhm,dec.appfwhmfit * dec.ffit(star,wg),psym=1,symsi=.3,yr=[-1,1]*0.02

 hitme, s8
 if s8 eq 'x' then stop
endif


     rr    = robust_sigma(ddwei)
     orgrr = robust_sigma(dd)

     wire_stetson, ddwei, wei, 0, /silent ; Stetson Weights
     wei_stet = wei / total(wei)
     wei_ptp  = fltarr(cg2) & wei_ptp(*) = 1.0 / float(cg2) ; even ptp weights

     ; Remove really bad points === ultra low weights!
      w2  = where(wei gt max(wei_stet) * 0.005,c2)
      w2  = where(wei gt 0.,c2)

      tt2 = ttm(w2) 
      dd2 = dd(w2) & dd2 = dd2 - median(dd2)
      dd2wei = ddwei(w2) & dd2wei = dd2wei - median(dd2wei)
        ; we2 = we(w2) & we2 = we2 / total(we2)
      we2 = wei_stet(w2) * wei_ptp(w2) & we2 = we2 / total(we2)
      dd2 = dd2 - median(  dd2)
      fwhm3 = fwhm2(w2)
    
;      plot,fwhm3,dd2wei,psym=3


if debug then begin
      !P.multi=[0,1,4] & !p.charsize=2.0
      col = getcolor(/load)

      wb=where(we2 lt median(we2 * 0.8),cb) ; Mark data points with low values?
      wb2=where(we2 lt median(we2 * 0.5),cb2) ; Mark data points with low values?

      plot,tt2,dd2,psym=3,yr=[-1,1]*orgrr*4.,tit='MASSAGED LC'
      if cb  ge 2 then oplot,tt2(wb),dd2(wb),psym=6,symsi=.7,col=col.sky
      if cb2 ge 2 then oplot,tt2(wb),dd2(wb),psym=4,symsi=.5,col=col.red

      plot,tt2,dd2,psym=3,yr=[-1,1]*orgrr*4.,tit='MASSAGED LC (zOOm)',xr=[-1,1]
      if cb  ge 2 then oplot,tt2(wb),dd2(wb),psym=6,symsi=.7,col=col.sky
      if cb2 ge 2 then oplot,tt2(wb),dd2(wb),psym=4,symsi=.5,col=col.red

      plot,tt2,dd2wei,psym=3,yr=[-1,1]*rr*6.,tit='LC - FIT'
      if cb  ge 2 then oplot,tt2(wb),dd2wei(wb),psym=6,symsi=.7,col=col.sky
      if cb2 ge 2 then oplot,tt2(wb2),dd2wei(wb2),psym=4,symsi=.5,col=col.red

      plot,dd2wei,we2,psym=1,symsi=.4,tit='WEIGHTS'
      if cb  ge 2 then oplot,dd2wei(wb), we2(wb), psym=6,symsi=.7,col=col.sky
      if cb2 ge 2 then oplot,dd2wei(wb2),we2(wb2),psym=4,symsi=.5,col=col.red

      hitme, s9 & if s9 eq 'x' then stop

      !P.multi = 0
endif

; Create final output array:
      nout = n_elements(tt2)
      out = replicate( {tt2:0., dd2:0., dd2wei:0., we2:0., fwhm3:0., fit:0.}, nout )
      out.tt2 = tt2
      out.dd2 = dd2
      out.dd2wei = dd2wei
      out.we2 = we2 & out.fwhm3 = fwhm3
      out.fit = fit(wg)

endif else begin
 print, ' %%% wire_exportlc.pro: No valid data for star: ',star
 out = -1
endelse


END
