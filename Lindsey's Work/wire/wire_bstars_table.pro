; Run this after wire_bstars.pro to create a table for paper on B type stars

snr_limit = 4.0 ; do not print modes with very low S/N

tabletxt = 'table'

decat = replicate( {hd:0L, n: 0B, r: lonarr(15), f:fltarr(15), a:fltarr(15), p:fltarr(15), comment:'', type:99B }, 20)

decat(0).hd = 40494
decat(0).comment = 'Candidate SPB. No modes.'
decat(0).n = 0B
decat(0).type = 0B ; SPB

decat(1).hd = 158404
decat(1).comment = 'Candidate Beta Cep.'
decat(1).type = 1B ; Beta Cep

decat(2).hd = 29248
decat(2).f(0:8) = [5.76330 , 5.65396, 5.62006, 5.63734 , 7.89845, 6.24434, 6.22304, 6.26080 , 7.20012]
decat(2).r(0:8) = 2005
decat(2).n = 9
decat(2).type = 1B

decat(3).hd = 44743
decat(3).f(0:2) = [3.979331, 3.9995, 4.1834]
decat(3).r(0:2) = 2006
decat(3).n = 3
decat(3).type = 1B

decat(4).hd = 111123
decat(4).f(0:2) = [5.2305468,  5.958666 , 5.472165]
decat(4).f(0:2) = 2010
decat(4).n = 3
decat(4).type = 1B

decat(5).hd = 126341
decat(5).f(0) = [5.637953]
decat(5).r(0) = 2014
decat(5).n = 1
decat(5).type = 1B

decat(6).hd = 129056
decat(6).f(0:1) = [3.848424, 4.223]
decat(6).r(0:1) = [2015, 2001]
decat(6).n = 1
decat(6).type = 1B

decat(7).hd = 158926
decat(7).f(0:8) = [4.679424,  4.39, 4.14, 3.86, 4.96, 4.51, 3.95, 4.25, 4.86]
decat(7).r(0:8) = [2024, 2035, 2035, 2035, 2035, 2035, 2035, 2035, 2035] 
decat(7).n = 1
decat(7).type = 1B

decat(8).hd = 160578
decat(8).f(0:1) = [5.0042461, 4.8678381]
decat(8).r(0:1) = [2024, 2024]
decat(8).n = 1
decat(8).type = 1B

decat(9).hd = 205021
decat(9).f(0:3) = [5.380, 4.924, 5.083, 5.417]
decat(9).r(0:3) = 2027
decat(9).n = 1
decat(9).type = 1B

for mode=0,1 do begin

hdcnt = 0L
hdused = lonarr(100)
lncnt = 0L

 if mode eq 0 then begin
   staruse = betacep
   symbol = 4 ; triangle up
   fileout = '/home/bruntt/papers/wire/bstars/betacep.tex'
   com = '$\beta$~Cep'
 endif
 if mode eq 1 then begin
   staruse = spb
   symbol = 5 ; triangle down
   fileout = '/home/bruntt/papers/wire/bstars/spb.tex'
   com = 'SPB'
 endif
 

nout = n_elements(staruse)

for g=0,nout-1 do begin

 sall = where(wire.hd eq staruse(g),cs)
 s=sall

 number_of_epochs = cs

 if cs ge 2 then begin
   noise = wire(sall).noise10
   wnoise = where(noise eq min(noise),cnoise)
   s = sall(wnoise)
   print,' %%% Several TEFF-PLOT for star: ',staruse(g), ' --- I will use: ',s
 endif

 amplitude   = wire(s).a2(0) ; avoid LOW FREQ + orbital peaks!
 frequency   = wire(s).f2(0)
 temperature = wire(s).teff

 good = 0B ; good data? long obs + now noise in the cleaned ampl. spectrum
 if wire(s).tobs ge 10. and wire(s).noise10 le 20 then good = 1
 if good then plotsym, symbol, /fill else plotsym, symbol

; szuse = interpol(sz, ampl, amplitude*1e3/1.086) ; symbol size
; plots,alog10(temperature), frequency, psym=8, symsi=szuse

 ; Known B stars should be clearly identified:
 add_decat = '' ; Star in De Cat's catalogue of known variable B stars?
 if pdc(s) ge 1 then add_decat = '$^K$'
 hdout = 'HD~' + strcompress(string(wire(s).hd,format='(I9)'),/remove_all) + add_decat + addhd
 if number_of_epochs ge 2 then $
  hdout = hdout + ' [' + strcompress(string(number_of_epochs,format='(I4)'),/remove_All) + ']'

 del = ' & '
 endel = ' \\'

  nfreq = wire(s).n2 ; number of freqs. to print out
  flim = 0.0005

  wused = where(wire(s).hd eq hdused,cused)
  if cused ge 1 then goto, already_printed

  hdused(hdcnt)=wire(s).hd
  hdcnt = hdcnt + 1  

; ============================================================
if hdcnt eq 1 then begin
   get_lun,uu
   openw,uu,fileout
endif

if hdcnt eq 1 or lncnt gt 30 then begin

   if lncnt gt 30 then begin
    printf,uu,"\end{tabular}"
    printf,uu,"\end{footnotesize}"
    printf,uu,"\end{" + tabletxt + "}"
   endif

   lncnt = 0L

   printf,uu,"\begin{" + tabletxt + "}"
   printf,uu," \centering"
   printf,uu," \caption{Modes detected for "+com+" stars."
   printf,uu," \label{tab:freq}}"
   printf,uu," \setlength{\tabcolsep}{3pt} % narrow table: default is tabcolsep = 6pt"
   printf,uu," \begin{footnotesize}"
   
;   outtab = "\begin{tabular}{l|rr|rrrr}"
   outtab = "\begin{tabular}{rrrr}"
   printf,uu,outtab
   printf,uu,"\hline"
;   outtab2 = "   & $T_{\rm eff}$ & $\log g$ & $f$ $[$c/d$]$ & $a$ $[$ppm$]$ & $p$ & SNR \\"
   outtab2 = "$f$   & $a$   &     &     \\"
   outtab3 = "$[$c/d$]$ & $[$ppm$]$ & $p$ & SNR \\"
   printf,uu,outtab2
   printf,uu,outtab3
   printf,uu,"\hline"
endif
; ============================================================

; Do you have freq. from peter de cat?
 wpeter_de_cat = where(wire(s).hd eq decat.hd,c_peter_de_cat)
 search_peter_de_cat=0B
 if c_peter_de_cat ge 1 then begin
    n_peter_de_cat = decat(wpeter_de_cat).n
    search_peter_de_cat = 1B
 endif

; ============================================================


  for k=0,nfreq-1 do begin

  w = where( abs(wire(s).f2(k) - wire(s).f) lt flim,c)
  if c ne 1 then stop
  
  snr  = strcompress( string(wire(s).sn(w),format='(F5.1)'),/remove_all) ; S/N
  freq = strcompress( string(wire(s).f2(k),format='(F8.4)'),/remove_all) ; Frequency in c/day
  ampl = strcompress( string(wire(s).a2(k)*1e6/1.086,format='(I8)'),/remove_all) ; ampl. in ppm
  phas = strcompress( string(wire(s).p2(k),format='(F8.4)'),/remove_all) ; phase [0..1]

  add_known1 = '' & add_known2 = ''
  if search_peter_de_cat then begin
   if n_peter_de_cat ge 1 then begin
   f_peter = decat(wpeter_de_cat).f(0:n_peter_de_cat-1)
   f_lim = 0.01 ; c/day
   wf = where(abs(wire(s).f2(k)-f_peter) lt f_lim,cf)
   if cf ge 2 then stop
   if cf eq 1 then begin
     add_known1 = '{\bf '
     add_known2 = '}'
 endif
 endif
 endif


   freq = add_known1 + freq + add_known2


 if float(snr) lt snr_limit then goto, insignificant

  hdout2 = '' & teffout = '' & loggout = ''

  if k eq 0 then begin
    hdout2 = hdout
    teffout = strcompress(string(round(wire(s).teff/100.)*100.,format='(I5)'),/remove_all)
    loggout = strcompress(string(wire(s).logg,format='(F6.1)'),/remove_all)
  endif

;   printf,uu,hdout2, del, teffout, del, loggout, del, freq, del, ampl, del, phas, del, snr, endel, $
;     format='(A16, A3, A6, A5, A5, A3, A7, A3, A7, A3, A7, A3, A6, A3)' 

   if k eq 0 then begin
    printf, uu,'\multicolumn{4}{c}{' + hdout2 + '} \\', format='(A45)'
    printf, uu,'\hline'
    lncnt = lncnt + 1
   endif

   printf,uu,freq, del, ampl, del, phas, del, snr, endel, $
     format='(A14, A5, A5, A3, A7, A3, A7, A3, A7, A3, A6, A3)' 
   lncnt = lncnt + 1

insignificant:  

; print,' %%% Next mode in this star: ' + hdout  
endfor
; ====================================

 printf,uu,"\hline"

 already_printed:
; print,' %%% Next star entry'
endfor 
; ====================================

; ====================================
   printf,uu,"\end{tabular}"
   printf,uu,"\end{footnotesize}"
   printf,uu,"\end{" + tabletxt + "}"
close,uu
free_lun,uu
; ====================================

 print,'' &  print,' %%% Wrote file: ' + fileout &  print,''
endfor                          ; next mode
; ====================================

END

