PRO wire_clean_exportp98, fc, nf, file, unit=unit

; Export frequencies from wire_clean_all.pro in a format
; that can be read by period98 and wirep.pro
; (c) June 2004, Hans Bruntt

; Example:
; -------------------
; wire_clean_all,$
; '/ai40/bruntt/wire/wire_lc/wire_lc_LambdaSco_star_0_no_eclipse.dat',$
;  'cday',0.1,25,30,fc,dc ; range 0.1 to 25 c/day, clean 30 frequencies
; 
; wire_clean_plot,fc,0,20,2000 ; plot ...
; 
; Finally, export the frequencies using THIS program: export 17 freqs:
; wire_clean_exportp98,fc,17,$
;  '/ai40/bruntt/wire/wire_lc/wire_lc_LambdaSco_star_0_no_eclipse_clean.per'

if n_elements(unit) eq 0 then unit = 'cday'
if strmatch(unit,'*day*') then freq_fac = 1D
if strmatch(unit,'*milli*') then freq_fac = 1e3/86400D
if strmatch(unit,'*micro*') then freq_fac = 1e6/86400D


get_lun,u
openw,u,file

for i=0,nf-1 do begin
 outf = 'F' + strcompress(string(i+1,format='(I8)'),/remove_all)
 if strlen(outf) eq 2 then outf = ' ' + outf

 printf,u,outf + ' ' + $
  string(fc(i).f/freq_fac,format='(F10.7)') + ' ' + $
  string(fc(i).a/1e6,format='(F10.7)') + ' ' + $
  string( (fc(i).p)/(2D*!DPI),format='(F10.7)')

endfor

close,u
free_lun,u

print,' %%% Exported freq. in a period98 like format: '
print,' %%%  ' + file

END
