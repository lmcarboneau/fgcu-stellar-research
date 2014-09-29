PRO wire_pername, impfile, pername

pername ='/'
a = strsplit(impfile,'/',/extract)
na = n_elements(a)
w = where(strmatch(a, 'wire_lc') eq 1,c)

for i=0,w(0)-1 do pername = pername + a(i) + '/'

pername = pername + 'wire_per/'

b = strsplit(a(w(0)+1),'_',/extract)

starname = b(2) + '_' + b(3) + '_' + b(4) + '.per'

pername = pername + starname


END
