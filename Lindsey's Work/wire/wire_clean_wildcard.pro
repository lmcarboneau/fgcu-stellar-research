PRO wire_clean_wildcard,wildcard, minf=minf, maxf=maxf, nfreq=nfreq, funit=funit

spawnrob,'ls -1 ' + wildcard, list
nl = n_elements(list)
if nl le 1 then RETURN

default9, minf, 0.02
default9, maxf, 50.
default9, nfreq, 40.
default9, funit, 'cday'

print,' >>> Number of light curves: ' + strcompress(nl,/remove_all)
for i=0,nl-1 do begin
 nam = list(i)
 wire_clean_all, nam, funit, minf, maxf, nfreq
 print,' >>> Number in list ',i
endfor

END
