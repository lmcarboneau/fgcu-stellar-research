PRO wire_read_templogg, gam, file, calib=calib, debug=debug

; Examples:
; .r  wire_gamma_combine_info
; wire_read_templogg, gam, '/mnt/hda5/data/wire/gammatarg2OUTcastelli.csv',calib='Cas',/debug
; wire_read_templogg, gam, '/mnt/hda5/data/wire/gammatarg2OUTribas.csv',calib='Rib',/debug
; wire_read_templogg, gam, '/mnt/hda5/data/wire/gammatarg2out.csv',calib='Nap',/debug

; Print results:
; for i=0,22 do print,gam(i).hd,gam(i).teff_cas,gam(i).eteff_cas,$
;  gam(i).logg_cas,gam(i).elogg_cas,format='(I6,2I6,2F6.2)'  


default9, calib, 'Nap'
default9, debug, 0B

if debug then begin
 print,' *** Carefully check input files from TEMPLOGG TNG:'
 print,' *** Remember to replace ",," by ", ," in input file'
 print,' *** Perhaps also delete the last 1-2 lines if they start by "Ident"'
 hitme,s9
endif

a = findfile(file,Count=cnt)
init = 0B

try_again:
if cnt ne 1 and init eq 0 then begin
 spawnrob,'mount /mnt/hda5/'
 init = 1B
 goto,try_again
endif

if cnt ne 1 then begin
 print,' %%% File not found: ' + file
 RETURN
endif

if n_elements(gam) le 0 then begin
  print,' %%% Input structure is empty ... '
  RETURN
endif

; Remember to replace ",," by ", ," in input file
case calib of
 'Nap': lim = 35 
 'Cas': lim = 35
 'Rib': lim = 35
endcase


temp = ''
openr,1,file
while not eof(1) do begin
 readf,1,temp
 s = strsplit(temp,',',/extract)
 ns = n_elements(s)
 if debug then print,ns,format='(I5,$)'
 
if ns eq lim then begin ; correct number of input items?
   hd = long(s(0))
    w = where(gam.hd eq hd,c)
   if c eq 1 then begin ; target found

; ======================================
     if calib eq 'Cas' then begin

 print,s(30:33)

       gam(w).teff_cas  = long(s(30))
       gam(w).eteff_cas = long(s(31)) 
       if s(32) ne 'NA' then gam(w).logg_cas  = float(s(32)) else gam(w).logg_cas = 9.98
       if s(33) ne 'NA' then gam(w).elogg_cas = float(s(33)) else gam(w).elogg_cas = 9.98
       gam(w).lcfile3  = gam(w).lcfile3 + ' CasTeff'
     endif
; ======================================

; ======================================
     if calib eq 'Rib' then begin
       if s(30) ne 'NA' then gam(w).teff_rib  = long(s(30))
       if s(31) ne 'NA' then gam(w).eteff_rib = long(s(31)) 
       if s(32) ne 'NA' then gam(w).logg_rib  = float(s(32))
       if s(33) ne 'NA' then gam(w).elogg_rib = float(s(33)) 
       gam(w).lcfile3  = gam(w).lcfile3 + ' RibTeff'
     endif
; ======================================

; ======================================
     if calib eq 'Nap' then begin
       if s(30) ne 'NA' then gam(w).teff_nap  = long(s(30))
       if s(31) ne 'NA' then gam(w).eteff_nap = long(s(31)) 
       if s(32) ne 'NA' then gam(w).logg_nap  = float(s(32))
       if s(33) ne 'NA' then gam(w).elogg_nap = float(s(33)) 
       if s(24) ne 'NA' then gam(w).feh_nap   = float(s(24))
       if s(25) ne 'NA' then gam(w).efeh_nap  = float(s(25)) 
       if s(16) ne 'NA' then gam(w).extby     = float(s(16)) 
       if s(17) ne 'NA' then gam(w).e_extby   = float(s(17)) 
       if s(20) ne 'NA' then gam(w).mv_templogg     = float(s(20)) 
       if s(21) ne 'NA' then gam(w).emv_templogg    = float(s(21)) 
       if s(26) ne 'NA' then gam(w).mass_templogg     = float(s(26)) 
       if s(27) ne 'NA' then gam(w).radius_templogg   = float(s(27)) 
       gam(w).lcfile3  = gam(w).lcfile3 + ' NapTeff'

;                    mass_templogg:-1., radius_templogg:0., $
;                     extby:-1., e_extby:-1., $
;                     mv_templogg:-1., emy_templogg:-1., $
 
     endif
; ======================================


 endif else stop                ; hd number found / not found = STOP

 endif 
endwhile
close,1

if debug then print,''

END
