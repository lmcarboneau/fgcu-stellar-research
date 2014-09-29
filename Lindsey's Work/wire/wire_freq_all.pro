PRO wire_freq_all, target, nstars, nclean, unit, f2

; Find frequencies in WIRE observations; June 2004, (c) Hans Bruntt

; Example:
; wire_freq_all,'AlphaAra',5,50,'milli',3.5
; wire_freq_all,'Theta2Tau',5,50,'cday',45.0

; target = 'AlphaAra'
; nstars = 5     ; number of stars in reduction file
; nclean = 2.   ; number of frequencies to clean
; unit = 'milli' ; frequency unit... milliHz
; f1    ; lower freq --> calculated by program
; f2 = 0.2       ; upper freq range

; file = '/ai40/bruntt/wire/collection/'+target+'_merged_allslots_31.idl'
; restore,file
; wirep,wireult,file,target,1,f='/ai40/bruntt/wire/wire_process/wirep_'+target+'.idl'
; orb = 15.3432


; for i=0,nstars-1 do begin
for i=4,nstars-1 do begin

lcfile = '/ai40/bruntt/wire/wire_lc/wire_lc_' + $
 target+'_star_'+string(i,format='(I1)')+'.dat'

print,' '
print,' %%% Light curve: ' + lcfile
print,' '

a = findfile(lcfile,Count=cnt)

if cnt eq 1 then begin ; does lc file exist?

 readcol, lcfile, time, dat, wei
 wgood = where(abs(dat) lt 5. and wei gt max(wei) * 5e-2,c)
 tmax = max(time(wgood)) & tmin = min(time(wgood))
 f1temp = 1. / (tmax - tmin)
 case unit of
  'micro': f1 = f1temp * 11.574
  'milli': f1 = f1temp * 11.574 / 1e3
  'cday':  f1 = f1temp
 endcase
 
 print,' %%% Minumum frequency = 1/Tobs = ', f1
 
 cleanfile = '/ai40/bruntt/wire/wire_lc/wire_lc_'+target+$
             '_star_'+string(i,format='(I1)')+'_clean.idl'
 
 wire_clean_all,lcfile, unit, f1, f2, nclean, fc, dc
 
 restore, cleanfile
 
 ; Find freqs. significant at 4-sigma level:
 sigma_limit = 4.0
 wire_clean_significant, fc, unit, sigma_limit, s
 w = where(s(1,*) eq 1,c) & fc2 = fc(w) ; pick the significant peaks
 
 ; Improve the frequencies you've found:
 wire_clean_improve,lcfile,unit,f1,f2,fc2, fc3, dc3

endif else begin
 print,' *** lc file not found: '+lcfile
endelse

endfor

END
