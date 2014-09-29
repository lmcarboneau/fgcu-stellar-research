PRO wire_bit_dist, p, p3, bit

wg = where(p gt 0.,cg)
p2 = p(wg)

resistant_mean, p2, 3, me, sd, nr
sig = robust_sigma(p2)
wg2 = where(abs(p2-me) lt 2. * sig,cg2)

p3 = p2(wg2)

bit = fltarr(4,16)

for i=1,16 do begin

bit_value = long(2.^float(i))

w = where( (p3 XOR bit_value) eq (p3-bit_value),c)
bit(0,i-1) = float(c) / float(cg2)
bit(1,i-1) = c
bit(2,i-1) = bit_value
bit(3,i-1) = i

endfor

END
