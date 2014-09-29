PRO wire_merge_ww,time3,dat3,wei3,gc3,cnt3,wire3

np  = n_elements(time3)
; np3 = n_elements(wire3.hjd)

wire3(cnt3:cnt3+np-1).hjd = time3
wire3(cnt3:cnt3+np-1).mag = dat3
wire3(cnt3:cnt3+np-1).gc  = gc3

cnt3 = cnt3 + np

END
