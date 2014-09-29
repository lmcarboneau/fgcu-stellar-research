PRO wire_dens_txtout, timegran, ampgran, ampmode, lifetime, whitenoise, $
 ranges, colx=colx,addyear=addyear, identify_model=identify_model, $
 yu1=yu1, yu2=yu2, xu1 = xu1, xu2 = xu2, shx=shx

default9, identify_model, 0B

 nl = n_elements(timegran)
 numout = 1 & if nl eq 1 then numout = 0


 default9, colx, [255] ; colours defined?
 ncol = n_elements(colx)

; Placement of txt
 xx = !x.crange & x1 = 10.^xx(0) & x2 = 10.^xx(1)
 yy = !y.crange & y1 = 10.^yy(0) & y2 = 10.^yy(1)

 default9, yu1, .3
 default9, yu2, .15
 default9, xu1, .15
 default9, xu2, .70
 default9, shx, 1.1

 log_y2a = alog10(y2 * yu1) & log_y1a = alog10(y2 * yu2)  
  log_k = findgen(nl+1) * (log_y2a - log_y1a) / (nl-1.+1.) + log_y1a
 k = 10.^(log_k)

 nxx = 6 ; antal data ud paa plot (ID, timeG, ampG, ampP, lifetime, wn)
 log_x1a = alog10(x2 * xu1) & log_x2a = alog10(x2 * xu2) ; range x
  log_kx = findgen(nxx) * (log_x2a - log_x1a) / (nxx-1.) + log_x1a
 kx = 10.^(log_kx)

  txtout2 = strarr(6)
  txtout2(0) =  '' & txtout2(1) = 't!IG!N [s]'
  txtout2(2) = 'a!IG!N [ppm]' & txtout2(3) = 'a!Im!N [ppm]'
  txtout2(4) = '!4g!3 [d]' & txtout2(5) = 'WN [ppm]'

  txtout = strarr(6)





for i=0,nl-1 do begin

 txtout(0) = strcompress(i,/remove_all)
 if numout eq 0 then txtout(0) = ''
 txtout(1) = strcompress(string(timegran(i),format='(I8)'),/remove_all)


 txtout(2) = strcompress(string(ampgran(i),format='(I8)'),/remove_all)
 txtout(3) = strcompress(string(ampmode(i),format='(F9.1)'),/remove_all)
 txtout(4) =strcompress(string(lifetime(i),format='(F9.1)'),/remove_all)
 txtout(5) =strcompress(string(whitenoise(i),format='(I9)'),/remove_all)

if identify_model then begin
 txtout(2) = strcompress(string(ampgran(i),format='(F8.2)'),/remove_all)
 txtout(3) = strcompress(string(ampmode(i),format='(I8)'),/remove_all)
endif
  
 for it = 0, nxx-1 do $
  xyouts,kx(it),k(i),txtout(it),$
   col=colx(i mod ncol), charsi=0.9, charthick=2

; Write explanation of numbers on the plot
 ori = replicate(270,6) & ori(0) = 0.
 if i eq (nl-1) then for j=0,nxx-1 do $
   xyouts, kx(j)*shx, k(i+1), txtout2(j), $
    charsi=1.0, charthick=2, $
    alignment = 1., orientation = ori(j)

endfor

if n_elements(addyear) eq 1 then $
xyouts,x1*1.3,y1*1.5,'Procyon ' + addyear, $
 charsi=1.6,charthick=3.0


END
