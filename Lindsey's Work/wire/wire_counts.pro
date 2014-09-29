PRO wire_counts, b1, v1, cnt, out=out

bv1 = b1 - v1
m_wire =  11.6607 + 1.01643 * v1 + 0.410385 * (bv1) -0.373560 * (bv1)^2
adu_wire = 10^( (25 - m_wire) / 2.5)

default9, out, 0

if n_elements(out) eq 0 then com = 0B

if out then print,' %%% Wire magnitude: ' + strcompress(m_wire) + $
 '  Count rate [ADU]: ' + strcompress(adu_wire)

cnt = adu_wire

END
