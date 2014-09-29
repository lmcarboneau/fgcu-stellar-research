PRO wire_prep_templogg, wireobj2

; Prepare wire data for the .csv type file that is need for input for
; TEMPLOGG (the windows version, yuk!)

; To get the required input structure for this prg. do this:
; m4_get_basedir, base
; restore,base + 'wire/wire_essential/wire_sec_info.idl' 
; wire_ramirez, wireobj, wireobj2
; Then: wire_prep_templogg, wireobj2

; For gamma doradus stars try this:
; .r  wire_gamma_combine_info
; wire_prep_templogg, gam

; For the corot stars in the gammador sample:
; restore,'~/VWA/MEGA/info/corot_atype_info.idl'
; wire_prep_templogg, gam


; (c) Hans Bruntt

; 13th of Apr 2005: I use wireobj instead of the wire structure,
; Thus I get parameters for all ~175 wire targets!

n = n_elements(wireobj2)
a = sort(wireobj2.spec1)
wireuse = wireobj2(a)

 com = ['Ident.','V','e(V)','(b-y)','e(b-y)','m1',$
        'e(m1)','c1','e(c1)','Hb','e(Hb)','Spc.Type',$
        'Parallaxe','e(Parallaxe)','vsini']
 nc = n_elements(com)
 print,com,$
  format='(A10, A6, A7, 2A7, 2A7, 2A7,2A7,A10,A8, A6, A8)'


for i=0,n-1 do begin
 abort = 0B
 err_v = 0.015 ; typical error on V magnitude
 eby = wireuse(i).eby & if eby lt 0 or eby gt 0.1 then eby = 0.003 & by = wireuse(i).by
  if eby lt 0.0005 then eby = 0.001
 em1 = wireuse(i).em1 & if em1 lt 0 or em1 gt 0.1 then em1 = 0.004 & m1 = wireuse(i).m1
  if em1 lt 0.0005 then em1 = 0.001
 ec1 = wireuse(i).ec1 & if ec1 lt 0 or ec1 gt 0.1 then ec1 = 0.005 & c1 = wireuse(i).c1
  if ec1 lt 0.0005 then ec1 = 0.001
 ehbeta = wireuse(i).ehbeta 
  if ehbeta lt 0 or ehbeta gt 0.1 then ehbeta = 0.002 & hbeta = wireuse(i).hbeta
 if ehbeta lt 0.0005 then ehbeta = 0.001



 spectype = wireuse(i).spec1
 spectype2 = spectype ; save the original spec type

 a = strsplit(spectype,'.',/extract) &  na = n_elements(a)
 if na ge 2 then spectype = a(0) + '.' + a(1) else spectype = a(0)

 a = strsplit(spectype,':',/extract) &  na = n_elements(a)
 spectype = a(0)

 a = strsplit(spectype,'+',/extract) &  na = n_elements(a)
 spectype = a(0)

 a = strsplit(spectype,'p',/extract) &  na = n_elements(a)
 spectype = a(0)

 a = strsplit(spectype,'m',/extract) &  na = n_elements(a)
 spectype = a(0)

; AIV-V ---> AIV
 a = strsplit(spectype,'-',/extract) &  na = n_elements(a)
 spectype = a(0)

; B2III/IV ---> B2III
 a = strsplit(spectype,'/',/extract) &  na = n_elements(a)
 spectype = a(0)

 w_ae = where(strmatch(spectype,'*ae') eq 1,c_ae)
 xlen = strlen(spectype)
 if c_ae eq 1 then spectype = strmid(spectype,0,xlen-2)

 w_ab = where(strmatch(spectype,'*ab') eq 1,c_ab)
 xlen = strlen(spectype)
 if c_ab eq 1 then spectype = strmid(spectype,0,xlen-2)

 w_ab = where(strmatch(spectype,'*evar') eq 1,c_ab)
 xlen = strlen(spectype)
 if c_ab eq 1 then spectype = strmid(spectype,0,xlen-4)

 w_ne = where(strmatch(spectype,'*ne') eq 1,c_ne)
 xlen = strlen(spectype)
 if c_ne eq 1 then spectype = strmid(spectype,0,xlen-2)

 xlen = strlen(spectype)
 if strmid(spectype,xlen-1,1) eq 'b' then spectype = strmid(spectype,0,xlen-1)

 xlen = strlen(spectype)
 if strmid(spectype,xlen-1,1) eq 'n' then spectype = strmid(spectype,0,xlen-1)

 xlen = strlen(spectype)
 if strmid(spectype,xlen-1,1) eq 'e' then spectype = strmid(spectype,0,xlen-1)

; Set error for the parallax:
 epar = wireuse(i).epar & par = wireuse(i).par
;;  if epar lt 0 or epar gt 0.1 then epar = max([par*0.2,1.0]) 

 evsini = wireuse(i).evsini 
  if evsini lt 0 or evsini gt 99 then evsini = 30. & vsini = wireuse(i).vsini

 if abs(by) gt 5. then abort=1

 if vsini lt 800. then $
  vsini2 = strcompress(string(vsini,format='(F9.1)'),/remove_all) else $
  vsini2 = ''

 if hbeta lt 9. then begin
  hbeta2 = strcompress(string(hbeta,format='(F9.3)'),/remove_all) 
  ehbeta2 = strcompress(string(ehbeta,format='(F9.3)'),/remove_all) 
 endif else begin
  hbeta2 = ''
  ehbeta2 = ''
 endelse
 
; If Hbeta or b-y is available, print this... otherwise print only
; johnson V + B-V colour

 if abort eq 0 then $
  print,wireuse(i).hd, $
        wireuse(i).v, err_v, $
        by, eby, $
        m1, em1, $
        c1, ec1, $
        hbeta2, ehbeta2, $
        spectype, $
        par, epar, $
        vsini2, $ ;  evsini2,$
format='(I10, F6.2, F7.3, 2F7.3, 2F7.3, 2F7.3,2A7,A10,F8.2, F6.2,A8)' else $
    print,wireuse(i).hd, $
        wireuse(i).v, wireuse(i).bv, ' ', $
        spectype, $
        par, epar, $
        vsini2, $ 
format='(I10, F6.2, F7.3, A56, A10, F8.2, F6.2 , A8)'
 
endfor

print,com

; This was written 13/11/2006:
print,''
print,' INSTRUCTIONS: '
print,' Copy and paste all the text above. Import file in EXCEL'
print,' in windows. It should be albe to identify the columns with'
print,' information. Then export the file in .CSV format. '
print,' Templogg TNG (windows version) can import this format.'
print,' Calculate Teff for Napiwotzski 1997, Castelli & Ribas'
print,' and save to approp. output files. These are input with'
print,' program wire_comp_teff_calibr.pro in IDL.'

END
