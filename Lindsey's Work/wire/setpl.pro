PRO setpl, dx, dy, fname, xo, yo, ENCAP=encap

 on_error,2

if (n_elements(dx) eq 0) then begin
 message,'Call: setpl, xs, ys, name, xoff, yoff, /encap',/info
 return
endif

 if n_elements(fname) eq 0 then begin
   name = 'idl.ps'
 endif else begin
   name = fname
 endelse

 set_plot,'ps'

  device, xs=dx, ys=dy, xoff=xo, yoff=yo, file=name

 if keyword_set(encap) then begin
   device,/encapsulated
   print,'Encapsulated'
 endif

end
