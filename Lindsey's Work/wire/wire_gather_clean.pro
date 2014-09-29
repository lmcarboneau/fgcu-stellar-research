PRO wire_gather_clean,wildcard,res,gat=gat,$
         calc=calc,freq_tolerance=freq_tolerance,finput=finput,cntgat=cntgat

; -------------------------------------------------
;
; Purpose:
; Import results from wire_master_clean.pro 
; of several simulations
;
; Example:
; wire_gather_clean,'/export/brixx1/bruntt/wire/EpsilonCep/simOSNy/epscep_*.clean.idl',res,gat=gat
;
; -------------------------------------------------
default9, freq_tolerance, 0.1 ; for OSN set this to < 0.006 to get realistic PHASE + FREQ. error estimates
; -------------------------------------------------

default9, calc, 1B

; -------------------------------------------------
l = file_search(wildcard)
n = n_elements(l)

if n eq 0 then begin
 print,' *** No data for wildcard: ' + wildcard
 RETURN
endif



; -------------------------------------------------

; -------------------------------------------------
restore,l(0)
nfreq = n_elements(result)
nfreq_mean = nfreq
if n_elements(finput) ge 1 then nfreq_mean = n_elements(finput)


gat = replicate({freq:dblarr(nfreq), amp:dblarr(nfreq), phase:dblarr(nfreq)}, n)
res = replicate({  freq:0D,   amp:0D,   phase:0D, $
                 e_freq:0D, e_amp:0D, e_phase:0D },nfreq_mean)

default9, output, 1B

; -------------------------------------------------

; -------------------------------------------------
for i=0,n-1 do begin
 file = l(i)
 restore,file

 gat(i).freq  = result.freq
 gat(i).amp   = result.amp
 gat(i).phase = result.phase mod 1

; Make sure phases are in the interval: [0...1]
 w = where(gat(i).phase lt 0., c)
 if c ge 1 then gat(i).phase(w) = gat(i).phase(w) + 1D
 
endfor
; -------------------------------------------------


; -------------------------------------------------
if calc then begin

; +++++++++++++++++++++++++++++++++++++++++++++++++
if n_elements(finput) eq 0 then begin
   for i=0,nfreq_mean-1 do begin
   
   if n_elements(finput) ne 0 then $
    wok = where(abs(gat.freq(i) - finput(i)) lt freq_tolerance) else $
    wok = indgen(n_elements(gat))
   
   cok = n_elements(wok)
   
   if cok ge 5 then begin
    res(i).e_freq  = robust_sigma(gat(wok).freq(i))
    res(i).e_amp   = robust_sigma(gat(wok).amp(i))
    res(i).e_phase = robust_sigma(gat(wok).phase(i))
   
    resistant_mean, gat(wok).freq(i),  3, me_freq , sd_freq , nr_freq
    resistant_mean, gat(wok).amp(i),   3, me_amp  , sd_amp  , nr_amp
    resistant_mean, gat(wok).phase(i), 3, me_phase, sd_phase, nr_phase
   
    res(i).freq  = me_freq
    res(i).amp   = me_amp
    res(i).phase = me_phase
   endif
    
   endfor
endif else begin                           ; if not input freq. were given
; +++++++++++++++++++++++++++++++++++++++++++++++++
   nsim = n_elements(gat) ; number of simulations
   gat_sort = replicate({freq:dblarr(nfreq_mean), amp:dblarr(nfreq_mean), phase:dblarr(nfreq_mean), n:0}, nsim)
   cntgat = fltarr(nfreq_mean)
   
   for i=0,nfreq_mean-1 do begin
    for s=0,nsim-1 do begin
    
    df = abs(gat(s).freq - finput(i))
    wok = where(df lt freq_tolerance,cok) & if wok(0) eq -1 then cok = 0B
    if cok ge 2 then begin ; more than one freq. found within tolarance interval?
      w2 = where(df(wok) eq min(df(wok)),c2)
      wok = wok(w2) & wok = wok(0) & cok = 1
    endif
   
    if cok eq 1 then begin
      gat_sort(cntgat(i)).freq(i)  = gat(s).freq(wok)
      gat_sort(cntgat(i)).amp(i)   = gat(s).amp(wok)
      gat_sort(cntgat(i)).phase(i) = gat(s).phase(wok)
      cntgat(i) = cntgat(i) + 1
    endif
   
    endfor
   endfor

for i=0,nfreq_mean-1 do begin

 res(i).e_freq  = robust_sigma(gat_sort(0:cntgat(i)-1).freq(i))
 res(i).e_amp   = robust_sigma(gat_sort(0:cntgat(i)-1).amp(i))
 res(i).e_phase = robust_sigma(gat_sort(0:cntgat(i)-1).phase(i))

 resistant_mean, gat_sort(0:cntgat(i)-1).freq(i),  3, me_freq , sd_freq , nr_freq
 resistant_mean, gat_sort(0:cntgat(i)-1).amp(i),   3, me_amp  , sd_amp  , nr_amp
 resistant_mean, gat_sort(0:cntgat(i)-1).phase(i), 3, me_phase, sd_phase, nr_phase

 res(i).freq  = me_freq
 res(i).amp   = me_amp
 res(i).phase = me_phase
endfor

gat = gat_sort
 
endelse ; finput was given?
; +++++++++++++++++++++++++++++++++++++++++++++++++

fac_freq = 1e6 / 86400D

if output then begin
print,'Freq [muHz]', 'e(f)', 'Amp [mmag]', 'e(Amp)', 'Phase [0..1]', 'e(Phase)',$
 format='(A14, A10, A12, A10, A10, A10)'
for i=0,nfreq_mean-1 do $
 print,res(i).freq*fac_freq, res(i).e_freq*fac_freq,$
       res(i).amp*1e3,       res(i).e_amp*1e3 ,$
       res(i).phase,res(i).e_phase,$
       format='(D14.3,D10.3, D12.2, D10.2, D12.3, D10.3)'

endif
; -------------------------------------------------
endif  ; calc on?


if n_elements(finput) ne 0 then $
  print,'' & print,' *** NOTE: gat array is SORTED by input frequencies!' & print,''
   

END
