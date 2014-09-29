PRO wire_replace_hdnumber, vwares

spec1 = [80027290, 80065526, 80110379, 80125081, 80126516] & ns1 = n_elements(spec1)
spec2 = [   27290,   065526,   110379,   125081,   126516]
for p=0,ns1-1 do begin
 p1 = where(vwares.hd eq spec1(p),c1)
 if c1 eq 1 then vwares(p1).hd = spec2(p)
endfor

END
