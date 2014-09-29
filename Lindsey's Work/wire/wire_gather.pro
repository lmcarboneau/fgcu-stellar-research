PRO wire_gather, inpfile, wire, debug=debug, r_orb=r_orb, highres=highres, $
 update_fft=update_fft, nofft=nofft, max_freq=max_freq

; The purpose of this program is to gather the information
; from the WIREG/ WIREP.PRO reduction restore .idl files,
; ie. reduced light curve, power spectrum, frequencies etc.
; and combine this with the magnitude/colour/vsini/parallax
; spectral type information in order to build a data base
; with all the necessary information to get an overview
; of the WIRE data. 

; For example: plot all B-type main seq. stars with noise
; levels below 20 ppm in the freq range 4-10 c/day after
; cleaning all frequencies.

; Example:
; wire_gather,'/home/bruntt/wire/wire_process/reduced.txt', wire

; Keywords:
; r_orb: Set range in frequency that will be counted as 'orbital
; harmonics', i.e. if f_orbit = 15.0 c/day, and r_orb = 0.3 then 
; peaks in the range 14.7 .. 15.3 will be counted as orbital
; harmonics. Note that peaks in the range 0... (2/tobs) are also
; counted as harmonics, although actually these peaks correspond to
; periods that are too long to be sampled at all by the time
; series. Often the "beating" of orbital harmomic frequencies can
; generate such artificial low-frequency peaks. [tobs = total observing
; time in days.]

default9, store_org, 0B ; store the original light curve?

default9, highres, 1.0 ; higher resolution in power spectra?
default9, lowfactor, 1.0 ; low freq. peaks: < (2/total_obs_time) * lowfactor
default9, debug, 0B  ; progress plots?
default9, r_orb, 0.2 ; remove orbital freq within "(+/-) r_orb" c/day

default9, lowfreq, 2.0 ; smoothin' of ampl. spec. < lowfreq is done with diff. stiffness
default9, stiff1, 3.0*(highres^1.0) ; stiff factor used below low freq [range: 2...5]
default9, stiff2, 8.0*(highres^1.5) ; stiff factor used above low freq [range: 2..50]

default9, update_fft, 0B ; make new FFT calculation?
default9, nofft, 0B ; skip fft calculation?

default9, max_freq, 50. ; up to 50 cycles pr. day

pi2 = 2D * !DPI

; Subtract special freq. for special stars:
wire_bstars_remfreq, remfreq


 wire_target_info, '~/wire/wire_essential/targets_wire.txt', info
 restore,'~/wire/wire_essential/wire_sec_info.idl' ; 09 NOV 2004: wireobj og info!

; Absolute magnitudes, ignoring B.C. & A_V
 mvp  = info.v + 5. - 5 * alog10(1e3/info.pax) 
 mv   = wireobj.v + 5. - 5 * alog10(1e3/wireobj.par) 

gg = findfile(inpfile,Count=cnt)
if cnt ne 1 then begin
 print, ' %%% Input file not found: ' + inpfile
 RETURN
endif

spawnrob,'cat ' + inpfile,list
nl = n_elements(list)

count = 0L

; =======================================================================================
; Create output structure
; =======================================================================================
nf  =  20001 ; number of elements for ampl. spectrum
npp = 160000 ; number of data points in light curve (aCen 1999: 110717 data points!)
nfr = 50     ; number of detected frequencies to store
stf = 4      ; number of amplitude spectra to store


; If you want to store the original light curve:
;                    orgnp:  0L, $ ; number of data points, $
;                    orglcsub:fltarr(npp), orglc:fltarr(npp), $
;                    orghjd:fltarr(npp), orgwei:fltarr(npp), $


if n_elements(wire) eq 0 then $
 wire = replicate( {hd:0L, nam:'', primnam:'', period:'', comp:'', entry: 0B, $
                    ra:0., dec:0., v:-9., bv:-19., mv:-1., par:-2., $
                    noise10: 0., $ ; noise around 10 milliHz [ppm]
                    calc_fft:0B, $
                    teff:0L, logg:0., feh:0., $    ; atmosphere parameters
                    eteff:0L, elogg:0., efeh:0., $
                    vsini:999., evsini:99., $
                    spec:'', lumclass:0B, class:0B,$
                    lc:fltarr(npp), hjd:fltarr(npp), wei:fltarr(npp), fwhm:fltarr(npp),$
                    fit:fltarr(npp), fitorb: fltarr(npp), $
                    rr:.1, orgrr: .2, $  ; plotting range
                    np:0L, $
                    lcsub:fltarr(npp), lcsub2:fltarr(npp), $
                    fitt:fltarr(npp), fitd: fltarr(npp), fitdorb:fltarr(npp), $
                    tobs:-1., $
                    t0:-99999D, freq:fltarr(stf,nf), ampl:fltarr(stf,nf), smoo:fltarr(stf,nf), $
                    mark:fltarr(5,200), $
                    orbital:-1., adu:-1., mmag:-99., $
                    f:fltarr(nfr),  a: fltarr(nfr), p: fltarr(nfr), n: 0, $
                    sn: fltarr(nfr), noise: fltarr(nfr), $
                    f2:fltarr(nfr), a2:fltarr(nfr), p2:fltarr(nfr), n2:0, $
                    f3:fltarr(nfr), a3:fltarr(nfr), p3:fltarr(nfr), n3:0 }, nl*5) else $
print,' >>> wire structure detected ... will not make a new structure!'

; =======================================================================================

wire.fitt = -9999.

for i=0,nl-1 do begin
 nam = list(i) 

 g2 = strsplit(nam,' ',/extract)
 namres = g2(0) ; restore file

; Get the main target name + approx. period of observation
 g =strsplit(nam,'/',/extract)
 ng = n_elements(g)
 base = g(ng-1)
 s = strsplit(base,'_',/extract)
 w2 = where(strmatch(s,'*wirep*') eq 1,c2)
 if c2 ne 1 then begin
   print,' %%% Could not determine unique star name from filename: ' + base 
   goto, skip_file
 endif
 name   = s(w2(0)+1)

 p = s(w2(0)+2) ; period + .idl + stars to consider
 dp = strsplit(p,'.',/extract)
 period = dp(0)

; p2 = strsplit(s(w2(0)+3),' ',/extract)

 p2 = strsplit(dp(1),' ',/extract)
 w11 = where(p2 eq 'idl',c11)

 np2 = n_elements(p2)
 wr = lonarr(np2-1)
 for k=0,np2-1-1 do wr(k) = fix(p2(k+w11(0)+1))
 npl = n_elements(wr)

 comp = name + '_' + period
 
 print,' %%% Star + period: ' + comp 
 print,' %%% Individual stars to plot: ',wr

; Read the reduction file:
 restore,namres ; wirep_struct
 dat = wirep_struct & wirep_struct = 0B
  dd = dat.lc


 for s=0,npl-1 do begin ; For each star:

        e = wr(s) ; star entry
  allinfo = dat.obj(e)
       hd = allinfo.hd

 if hd eq 0 then begin
  if comp eq 'betaLeo_June2000' and e eq 0 then begin
     allinfo.hd = 102647L
             hd = allinfo.hd 
  endif

  if comp eq 'kappaVel_May1999' and e eq 0 then begin
     allinfo.hd = 81188L
             hd = allinfo.hd
  endif

  
  if comp eq 'alphaCen_Jan2004' and e eq 0 then begin
     allinfo.hd = 128620L
             hd = allinfo.hd

  endif

  print,' %%% No HD number available for star: ', comp, '  --  Entry: ',e, ' -- HD: ',hd
 endif

  wobj = where(wireobj.hd eq hd,cobj)
  if cobj ne 1 then begin
   print, ' *** HD '+strcompress(hd) + ' not found in wireobj structure!'
  endif else begin
   wobj    = wobj(0)
   nam     = wireobj(wobj).nam
   ra      = wireobj(wobj).ra   & dec      = wireobj(wobj).dec
   v       = wireobj(wobj).v    & bv       = wireobj(wobj).bv
   mv      = wireobj(wobj).v + 5. - 5 * alog10(1e3/wireobj(wobj).par) 
   vsini   = wireobj(wobj).vsini & evsini = wireobj(wobj).evsini
   spec    = wireobj(wobj).spec1
   lumclass= wireobj(wobj).lumclass
   class   = wireobj(wobj).class

   wire(count).nam = nam
   wire(count).ra = ra & wire(count).dec = dec
   wire(count).v = v & wire(count).bv = bv
   wire(count).mv = mv & wire(count).par = wireobj(wobj).par ; PARALLAX
   wire(count).vsini = vsini & wire(count).evsini = evsini
   wire(count).spec = spec
   wire(count).lumclass = lumclass & wire(count).class = class
  endelse


wire(count).hd = hd
wire(count).period = period
wire(count).entry  = e
wire(count).comp = comp
wire(count).primnam = name


; Brightness of the observed star:
  mmag             = dat.d(e).mmag
  wire(count).mmag = mmag
  wire(count).adu  = 10.^((25.0 - mmag)/2.5)

  orglc   = dd.mag(e)            ; light curve
  orghjd  = dd.hjd - dat.t0      ; hjd times
  orgwei  = dat.d(e).wei         ; weights
  fit     = dat.fit & fit = reform(fit(e,*))   ; identical HJD times


 ; Zero point in time
  t0   = dat.t0  
  wire(count).t0 = t0

 ; orbital frequency needed to phase data!
  orbital = dat.orb_freq 
  wire(count).orbital = orbital
  

  star = e
  pick = dat.d.w & pick = pick(*,e) ; SCATTERED LIGHT POINTS MARKED?
  orgfwhm = dd.fwhm & orgfwhm = reform(orgfwhm(e,*))
  dec  = dat.dec


; Construct the light curve that was probably exported from wirep/wireg.pro:
  wire_exportlc, orglc, orghjd+t0, orgfwhm, pick, fit, t0, mmag, dec, star, out, $
     rr=rr, orgrr=orgrr, debug=debug

  if n_elements(out) eq 1 then begin
      print,' *** Skipping star '+strcompress(e) + ' -- for struct: ' + comp
      goto,skip_star
  endif

; Look for .per file:
  lookdir = '/home/bruntt/wire/wire_periods/'
  lookfile = lookdir + '*' + period + '*' + strcompress(hd,/remove_all) + '*'
  slack = findfile(lookfile,Count=cc)
  if cc ge 2 then begin
   print,' %%% I found ' +strcompress(cc) + ' matching files for .p04 frequencies: '
   for jj=0,cc-1 do print,strcompress(jj) + ': ' + slack(jj)
   sel = 0B & read, sel
   slack = slack(sel)
  endif
  print,' %%% Importing freq/amp/phase from file: ' + slack

  if slack eq '' then begin
    print, ' *** No period file available: ',period,hd
    p98f = 1e-6
    p98a = 0.
    p98p = 0. ; artificial zero amplitude signal
    goto,noperiodfile
  endif

   readcol,slack, $
           dummy,p98f,p98a,p98p,$
           format='A,D,D,D', /silent

  noperiodfile:

   nfreq  = n_elements(p98f)
   npfit  = n_elements(out)
   fitter = fltarr(npfit)
   fitter_orbital = fltarr(npfit)

   for pp=0, nfreq-1 do $
    fitter = fitter + p98a(pp) * $
      sin( pi2 * ( p98f(pp) * (out.tt2) + p98p(pp) )  )

   if n_elements(orbital) eq 1 then begin

    worb = lonarr(nfreq) & cntorb = 0
    for p=1,25 do begin ; include very low frequency peaks!
     wo = where(abs(p98f - float(p) * orbital) lt r_orb,co)
     if co ge 1 then begin
       worb(cntorb:cntorb+co-1) = wo
       cntorb=cntorb + co
     endif 
   endfor

 ; Total observing time for data set:
   tobs = max(out.tt2) - min(out.tt2)
   wire(count).tobs = tobs

  ; LOW FREQUENCY PEAKS ---> HARMONICS
   wlow = where(p98f le (2./tobs) * lowfactor,clow)
   if clow ge 1 then worb(cntorb:cntorb+clow-1) = wlow
   cntorb = cntorb + clow

    if cntorb ge 1 then begin

     worb = worb(0:cntorb-1) ; remove unused entries
     sp = sort(worb) & worb = worb(sp)

     w_notorb = findgen(nfreq)
     w_notorb(worb) = -1
     w_gg = where(w_notorb ge 0,c_gg)
     if c_gg ge 1 then $
       w_notorb = w_notorb(w_gg) else $
       w_notorb = -1

    for pp=0, cntorb-1 do $
     fitter_orbital = fitter_orbital + p98a(worb(pp)) * $
       sin(pi2 * (p98f(worb(pp)) * (out.tt2) + p98p(worb(pp)) ) )

   endif ; any orbital frequencies?
  endif ; fit orbital frequencies only?


  border = 0.2
  range_t = (max(out.tt2) - min(out.tt2)) + border * 2.
  fitt = dat.t0 + range_t * findgen(npp) / (npp-1.) - border + min(out.tt2)
  ;; print, min(fitt)-t0,min(out.tt2),  max(fitt)-t0, max(out.tt2)
  fitt2 = fitt-t0

; Synthetic light curve incl. all frequencies
  datt = fltarr(npp)
     for pp=0, nfreq-1 do $
      datt = datt + p98a(pp) * sin(pi2 * (p98f(pp) * fitt2 + p98p(pp))  ) 

; Synthetic light curve ... for the orbital harmonics + low freq.
  datorb = fltarr(npp)
     for pp=0, cntorb-1 do $
      datorb = datorb + p98a(worb(pp)) * sin(pi2 * (p98f(worb(pp)) * fitt2 + p98p(worb(pp)))  ) 

  nfr_space = n_elements(wire(count).f)
  if nfr_space lt nfreq then begin
     print,' *** Too many frequencies ... cannot store them all ... '
     nfreq = nfr_space
     if c_gg gt nfr_space then c_gg = nfr_space
  endif

  ; All frequencies:
   wire(count).f(0:nfreq-1) = p98f(0:nfreq-1)
   wire(count).a(0:nfreq-1) = p98a(0:nfreq-1)
   wire(count).p(0:nfreq-1) = p98p(0:nfreq-1)
   wire(count).n = nfreq
  if cntorb ge 1 then begin ; Orbital harmonics
   wire(count).f3(0:cntorb-1) = p98f(worb)
   wire(count).a3(0:cntorb-1) = p98a(worb)
   wire(count).p3(0:cntorb-1) = p98p(worb)
   wire(count).n3              = cntorb
 endif else begin
   c_gg = nfreq & w_notorb = findgen(nfreq)
 endelse
  if c_gg ge 1 then begin ; frequencies that are NOT orbital harmonics
   wire(count).f2(0:c_gg-1) = p98f(w_notorb(0:c_gg-1))
   wire(count).a2(0:c_gg-1) = p98a(w_notorb(0:c_gg-1))
   wire(count).p2(0:c_gg-1) = p98p(w_notorb(0:c_gg-1))
   wire(count).n2           = c_gg
endif

; Sanity check:
; print,wire(count).f(0:wire(count).n-1) 
; print,wire(count).f2(0:wire(count).n2-1)
; print,wire(count).f3(0:wire(count).n3-1)

col=getcolor(/load)

; Sanity checks:
;plot,out.tt2,fitter  ,/nodata
;oplot,fitt-t0,datt
;oplot,out.tt2,out.dd2,psym=1,symsi=.2,col=col.sky
;hitme,ss199 & if ss199 eq 'x' then stop

;plot,out.tt2,fitter,xr=[-1,1],tit='zOOm'  ,/nodata
;oplot,fitt-t0-0.16,datt
;oplot,out.tt2,out.dd2,psym=1,symsi=.2,col=col.sky
;hitme,ss199 & if ss199 eq 'x' then stop


;plot,fitt-dat.t0,fitd,yr=[-1,1]*.05,/nodata
;oplot,orghjd,orglc-mmag,psym=3,/nodata
;oplot,out.tt2,out.dd2,psym=1,symsi=.2
;oplot,out.tt2,out.fit,psym=1,symsi=.2

;plot,out.tt2,out - out.fit.dd2,psym=1,symsi=.2
;oplot,out.tt2,out.fit,psym=1,symsi=.2

 
; ===========================================================================
; Massaged light curve: FWHM fit, scat fit, etc + perhaps scat light removal?
; ===========================================================================
  np_mass = n_elements(out)
  if np_mass gt npp then begin
   print,' >>> TOO FEW POINTS TO STORE COMPLETE LIGHT CURVE: ',np_mass,' > ', npp
   print,' >>> Star: ',comp, '  --  Entry: ',e
   np_mass = npp
  endif

  if np_mass ge 2 then begin ; any valid data?

   wire(count).lc(0:np_mass-1)     = out(0:np_mass-1).dd2
   wire(count).lcsub(0:np_mass-1)  = out(0:np_mass-1).dd2wei
   wire(count).lcsub2(0:np_mass-1) = out(0:np_mass-1).dd2 -  fitter_orbital(0:np_mass-1)

   wire(count).fit(0:np_mass-1)    = fitter(0:np_mass-1) ; out.fit
   wire(count).fitorb(0:np_mass-1) = fitter_orbital(0:np_mass-1) ; subtract orbital harmonics

   wire(count).hjd(0:np_mass-1)    = out(0:np_mass-1).tt2
   wire(count).wei(0:np_mass-1)    = out(0:np_mass-1).we2
   wire(count).fwhm(0:np_mass-1)   = out(0:np_mass-1).fwhm3

   wire(count).rr = rr & wire(count).orgrr = orgrr
   wire(count).np = np_mass

  endif
; ===========================================================================



if nofft or $
 ((wire(count).calc_fft eq 1) and (update_fft eq 0)) then begin
 print,' >>> I will skip FFT calculation '
 goto, skip_fft
endif

print,' %%% Computing amplitude spectra [1..4]:',format='(A42,$)'
; ====================================================================
; Compute amplitude spectrum of massaged LC:
; ====================================================================
   tt2 = out.tt2 & dat2 = out.dd2 & wei2 = out.we2
   minfreq2 = 2. / wire(count).tobs ; c/day
   maxfreq2 = max_freq ; c/day
   ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase, $
     highres=highres, /silent

; Store the raw amplitude spectrum:
   nfft = n_elements(freq)
   wire(count).freq(0,0:nfft-1) = freq
   wire(count).ampl(0,0:nfft-1) = amp

; Calculate a smoothed version of the cleaned amplitude spectrum:
   wire_smooth_spec, freq, amp, lowfreq, ampl2,stiff1=stiff1,stiff2=stiff2
   wire(count).smoo(0,0:nfft-1) = ampl2

if wire(count).a(0) lt 1e-5 then begin
 print,''
 print,' >>> Ampl. < 1e-5 ppm detected: Only one ampl. spec was calculated!'
 goto,skip_fft
endif

print,'1',format='(A3,$)'
; ====================================================================
; Subtracting the *orbital* frequencies:
; ====================================================================
   tt2 = out.tt2  &  dat2 = out.dd2 - fitter_orbital  &  wei2 = out.we2
   minfreq2 = 2. / wire(count).tobs ; c/day
   maxfreq2 = max_freq ; c/day
   ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase, $
     highres=highres, /silent

; Store the cleaned amplitude spectrum:
   nfft = n_elements(freq)
   wire(count).freq(1,0:nfft-1) = freq
   wire(count).ampl(1,0:nfft-1) = amp

; Calculate a smoothed version of the cleaned amplitude spectrum:
   wire_smooth_spec, freq, amp, lowfreq, ampl2,stiff1=stiff1,stiff2=stiff2
   wire(count).smoo(1,0:nfft-1) = ampl2

print,'2',format='(A3,$)'
; ====================================================================
; Subtracting all frequencies:
; ====================================================================
   tt2 = out.tt2  &  dat2 = out.dd2wei  &  wei2 = out.we2
   minfreq2 = 2. / wire(count).tobs ; c/day
   maxfreq2 = max_freq ; c/day
   ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase, $
     highres=highres, /silent

; Store the cleaned amplitude spectrum:
   nfft = n_elements(freq)
   wire(count).freq(2,0:nfft-1) = freq
   wire(count).ampl(2,0:nfft-1) = amp

; Calculate a smoothed version of the cleaned amplitude spectrum:
   wire_smooth_spec, freq, amp, lowfreq, ampl2,stiff1=stiff1,stiff2=stiff2
   wire(count).smoo(2,0:nfft-1) = ampl2

print,'3',format='(A3,$)'

; ====================================================================
; Subtracting all frequencies / noise at high frequency (10 milliHz):
; ====================================================================
   tt2 = out.tt2  &  dat2 = out.dd2wei  &  wei2 = out.we2
   minfreq2 =  9500. / 11.574
   maxfreq2 = 10500. / 11.574
   ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase, $
     highres=highres, /silent

; Store the cleaned amplitude spectrum:
;   nfft = n_elements(freq)
;   wire(count).freq(4,0:nfft-1) = freq
;   wire(count).ampl(4,0:nfft-1) = amp

; Calculate a smoothed version of the cleaned amplitude spectrum:
;   wire_smooth_spec, freq, amp, lowfreq, ampl2,stiff1=stiff1,stiff2=stiff2
;   wire(count).smoo(4,0:nfft-1) = ampl2

   wire(count).noise10 = avg(amp)

print,'4',format='(A3,$)'


; ====================================================================
; Calculate approximate signal-to-noise levels in the ampl. spectrum:
; ====================================================================
   for jj=0,nfreq-1 do begin
    a = wire(count).smoo(2,0:nfft-1) ; smooth'ed version of cleaned spectrum
    f = wire(count).freq(2,0:nfft-1) ; the frequencies

   ; Interpolate noise level at each frequency:
    noise = interpol(a, f, wire(count).f(jj) )
    wire(count).sn(jj)    = wire(count).a(jj) * 1e6 / noise
    wire(count).noise(jj) = noise 
  endfor

; Set calculation flag
wire(count).calc_fft = 1B


; ====================================================================
; Subtracting all frequencies with (S/N above 10) OR (HIGEST FREQ) + harmomics:
; ====================================================================

; Mark frequencies that are known orbital frequencies:
bad = bytarr(nfreq) & if cntorb ge 1 then bad(worb) = 1

override=0B
jump_override:

windiv = where(hd eq remfreq.hd,cindiv)
if (override eq 1) or (cindiv ne 1) then begin

   w_hi_sn = where(wire(count).sn ge 10.0 and bad ne 1,c_hi)
   w_lo_sn = where(wire(count).sn lt 10.0 and bad ne 1,c_lo)
   
   ; In the case of FEW or NO frequencies with high signal:
   if c_hi le 3 then begin
    n_ok = wire(count).n2
    if n_ok ge 2 then begin
     nrem = floor(n_ok * 0.5)
     frem = wire(count).f2(0:nrem-1)
     w_hi_sn = lonarr(nrem) & w_hi_sn(*) = -1
     for j=0,nrem-1 do begin
       wrr = where(wire(count).f eq frem(j),crr)
       if crr eq 1 then w_hi_sn(j) = wrr
     endfor
     w2p = where(w_hi_sn ge 0,c_hi) & w_hi_sn = w_hi_sn(w2p)
     c_lo = n_ok - c_hi
    endif
   endif
endif else begin
; User has manually selected which freq. should be removed!

 rem = remfreq(windiv).freq
 ggg = where(rem gt 0.,nrem) & rem = rem(ggg)
 w_hi_sn = lonarr(100) & c9 = 0

 for kp=0,nrem-1 do begin
  wp = where( abs(rem(kp) - wire(count).f) lt 0.001 and bad ne 1, c_p)
  if c_p eq 1 then begin
     w_hi_sn(c9) = wp(0)
     c9 = c9 + 1
  endif
endfor

     if c9 eq 0 then begin
      print,''
      print,' %%% No freq match found for star ',wire(count).hd, '  in remfreq structure! -- count = ' + string(count,format='(I3)')
      print,''
      override = 1B &  goto,jump_override
    endif

  w_hi_sn = w_hi_sn(0:c9-1)
  c_hi = c9 & c_lo = wire(count).n2 - c_hi

endelse



; help,clean,fitter_orbital,fit_high,out.dd2

if c_lo ge 1 and c_hi ge 1 then begin
    fit_high = fltarr(n_elements(out.tt2))
    for pp=0, c_hi-1 do $
     fit_high = fit_high + p98a(w_hi_sn(pp)) * $
       sin(pi2 * (p98f(w_hi_sn(pp)) * (out.tt2) + p98p(w_hi_sn(pp)) ) )

   clean = out.dd2 -  fitter_orbital - fit_high ; subtr. orbital freq. + high signal modes

   tt2 = out.tt2  &  dat2 = clean  &  wei2 = out.we2
   minfreq2 = 2. / wire(count).tobs ; c/day
   maxfreq2 = max_freq ; c/day
   ampl_spec_calc_wire_rv,tt2,dat2,wei2,minfreq2,maxfreq2,freq,amp,phase, $
     highres=highres, /silent

; Store the cleaned amplitude spectrum:
   nfft = n_elements(freq)
   wire(count).freq(3,0:nfft-1) = freq
   wire(count).ampl(3,0:nfft-1) = amp

; Calculate a smoothed version of the cleaned amplitude spectrum:
   wire_smooth_spec, freq, amp, lowfreq, ampl2,stiff1=stiff1,stiff2=stiff2
   wire(count).smoo(3,0:nfft-1) = ampl2
endif

print,'  4 ... FFT Calc done!'

; ====================================================================
; ALL FFT calculations were aborted:
; ====================================================================
skip_fft:
; ====================================================================


; ===========================================================================
; Original light curve: Remove data points with very low weights:
; ===========================================================================
  wg = where(orgwei / max(orgwei) gt 1e-4,cg)
  orglc = orglc(wg) & orghjd = orghjd(wg) & orgwei = orgwei(wg) / total(orgwei(wg))
  orglc = orglc - median(orglc)

   fitall = fltarr(n_elements(orglc))
   for pp=0L, nfreq-1 do $
    fitall = fitall + p98a(pp) * $
      sin( pi2 * ( p98f(pp) * orghjd + p98p(pp) )  )
 
if store_org then begin
; Original light curve:
  cg_use = cg & cg_max = n_elements(wire(count).orglc)
  if cg gt cg_max then cg_use = cg_max
  wire(count).orglc(0:cg_use-1)    = orglc(0:cg_use-1)
  wire(count).orghjd(0:cg_use-1)  = orghjd(0:cg_use-1)
  wire(count).orgwei(0:cg_use-1)  = orgwei(0:cg_use-1)
  wire(count).orglcsub(0:cg_use-1) = orglc(0:cg_use-1) - fitall(0:cg_use-1)
  wire(count).orgnp = cg_use
endif

; Fitted light curve -- complete time range, but not the RIGHT times!
  cg_fit = n_elements(fitt)
  wire(count).fitt(0:cg_fit-1) = fitt
  wire(count).fitd(0:cg_fit-1) = datt
  wire(count).fitdorb(0:cg_fit-1) = datt - datorb

; Light curve with SCATTERED light points removed (if marked in
; reduction file)
; ===========================================================================

;  plot,orghjd,orglc,yr=mmag + [-1,1]*.1,psym=1,symsi=.1

; Spectra calculated in WIREP / WIREG.PRO:
  power = dat.sp(e)
  wp = where(power.calc eq 1,cp)
  freq = power.freq & ampl = power.amp

  if cp ge 1 then begin
   freq = reform(freq(wp,*))
   ampl = reform(ampl(wp,*))
   mark = power.mark
   wmm = where(mark gt 0.,cmm) & if cmm ge 1 then mark = mark(wmm) else mark = -1
  endif

count = count + 1

; ===========================================================================
skip_star:
endfor ; next star
; ===========================================================================





skip_file:
endfor

wire = wire(0:count-1)

END
