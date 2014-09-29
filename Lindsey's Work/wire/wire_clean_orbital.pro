PRO wire_clean_orbital, lcfile,nharm,nclean,forb=forb,df=df, incwid=incwid

; Example:
; wire_clean_orbital,$
; '/ai40/bruntt/wire/wire_lc/wire_procyon_sept99_22jun04.dat',3,2,df=3.
; wire_clean_orbital,lc2,90.,30.,df=7.,forb=173.62563, incwid=190.*11.574

; incwid: increase width of window below freq. of "incwid" microHz

; lcfile = '/ai40/bruntt/wire/wire_lc/wire_procyon_sept99_22jun04.dat'

 a = strsplit(lcfile,'/',/extract)
 na = n_elements(a)
 baseall = '/'
 for j=0,na-2 do baseall = baseall + a(j) + '/'
 base  = baseall + 'temp_clean_orbital.dat'
 final = baseall + a(na-1) + '.clean.orbit

 spawnrob,'rm -f '+base
 spawnrob,'cp -f '+lcfile + ' ' + base

 unit = 'micro'

 if n_elements(forb) eq 0 then forb = 174.767 ; microHz - orbital frequency in 1999
 if n_elements(df) eq 0 then df = 5.          ; width in frequency (microHz)
 if n_elements(incwid) eq 0 then incwid = -99 ; increase width of window below this freq. 


 for h=0,nharm-1 do begin

 fcent = forb * (1. + h)
 dfuse = df
 nclean_use = nclean

 if fcent lt incwid then begin
   dfuse = dfuse * 3.0 ; increase width of clean window
   nclean_use = long(nclean_use * 2.)
 endif

 f1 = fcent - dfuse
 f2 = fcent + dfuse

 wire_clean_all, base, unit, f1, f2, nclean_use, fc, dc, mode = 'SaveLast'

 get_lun,u
 openw,u,base
 nd = n_elements(dc)
 for l=0L,nd-1 do $
  printf,u,dc(l).t, dc(l).d, dc(l).w, $
   format='(D15.8, X, D15.8, X, D15.8)'
 close,u
 free_lun,u


 endfor

 spawnrob,'rm -f '+final
 spawnrob,'mv ' + base + ' ' + final
 print,' %%% New light curve: '+final
 
 
END
