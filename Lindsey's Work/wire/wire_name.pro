PRO wire_name, str

 wire_red_setup,a,b
 restore,a ; xyinfo + wireinfo

 w = where(strmatch(wireinfo.object,'*' + str + '*') eq 1, c)

if c ge 1 then begin
 for i=0,c-1 do print,strcompress(w(i))+': ' + wireinfo(w(i)).object 
endif else begin
 print,' %%% Object containing ' + str + ' NOT found ! '
endelse


 if c eq 1 then begin
  print,' %%% WIRE FIELD IDENTIFICATION: '
  w2 = where(xyinfo(w).object ne '' and xyinfo(w).object ne 'Unknown',c2)
  for j=0,c2-1 do begin
    out = xyinfo(w).object(w2(j))
    g = strsplit(out,'&',/extract)
    

   print,strcompress(w2(j)) + ': ', g(0), g(1), $
    '   flux = ' + strcompress( xyinfo(w).flux(0,w2(j)),/remove_all), $
   format='(A4, A20, A10, A15)'

  endfor
 endif


 



END
