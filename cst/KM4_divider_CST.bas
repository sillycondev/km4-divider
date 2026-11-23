' ============================================================================================
' CST Studio Suite macro: broadband equal-split 4-way Knochel-Mayer power divider/combiner
' Implements: J. Furgal, K. Xu, J. H. Choi, J. K. Lee, 'Broadband Equal-Split Planar 4-Way Power
'             Divider/Combiner Suitable for High Power Applications', EuMC 2020 (fc = 2.75 GHz)
'
' HOW TO USE: File > New project (empty, any template), then Home > Macros > Run Macro... > this file.
'   Run it ONCE in an empty project. Everything is written to the History List, so it survives
'   parameter changes / history updates.
'
' Circuit (Fig. 2): Z1=100, Z2=50, Z3=35, Z4=50, Z5=50, Z6=84.6, Z7=50, Z8=100 ohm, R=50 ohm
'   Z1..Z5, Z7 = lambda/4 ; Z6, Z8 = lambda/2 open stubs.
' Stack-up (Fig. 4, top to bottom):
'   L1 microstrip | RO3003 20 mil | RO4450B 4 mil | L2 GND | RO3003 60 mil | RO4450B 4 mil |
'   L3 stripline  | RO3003 60 mil | L4 GND | RO4450B 4 mil | RO3003 20 mil | L5 microstrip
'   L1: Z4, Z5, resistors (L1-L2 vias to ground)   L3: Z1, Z6, Z7, Z8 + all ports   L5: Z2, Z3
'   Vias: L3-L5 at each output node, L1-L5 at each Z2/Z3/Z4 node, L2-L4 ground stitching.
' Line widths: 2-D quasi-static field solution for this exact stack-up (35 um copper).
' Lengths: lambda/4 (or lambda/2) at 2.75 GHz using each line's effective permittivity,
'   measured centre-to-centre between nodes.
' The paper gives no layout dimensions; this layout is an original one that keeps the
'   paper's topology, layer assignment, vias and stack-up.
' Ports: 1 = input (bottom edge, x=-20), 2 = W, 3 = S, 4 = N, 5 = E (all on stripline L3).
' Units: mm, GHz.
' ============================================================================================

Option Explicit

Sub Main ()

    ' ---------------- parameters (edit in the CST Parameter List afterwards) ----------------
    StoreParameter "h_sub", "0.5080"   ' RO3003 20 mil
    StoreParameter "h_bond", "0.1016"   ' RO4450B bond film 4 mil
    StoreParameter "h_core", "1.5240"   ' RO3003 60 mil
    StoreParameter "t_cu", "0.0350"   ' copper thickness
    StoreParameter "W_ms50", "1.4586"
    StoreParameter "W_ms35", "2.4826"
    StoreParameter "W_sl50", "1.9244"
    StoreParameter "W_sl100", "0.3629"
    StoreParameter "W_sl85", "0.6100"
    StoreParameter "Lstub6", "31.1632"   ' Z6 lambda/2 open stub length (tuning)
    StoreParameter "Lstub8", "31.0924"   ' Z8 lambda/2 open stub length (tuning)
    StoreParameter "r_via", "0.4000"   ' via radius
    StoreParameter "r_pad", "0.8000"   ' via pad radius
    StoreParameter "r_ap", "1.4000"   ' antipad radius in L2/L4
    StoreParameter "R_iso", "50.0000"   ' isolation resistors (ohm)
    StoreParameter "z_L4b", "h_sub+h_bond"   ' L4 GND bottom
    StoreParameter "z_L4t", "z_L4b+t_cu"   ' L4 GND top
    StoreParameter "z_L3", "z_L4t+h_core"   ' L3 trace bottom
    StoreParameter "z_L2b", "z_L3+h_bond+h_core"   ' L2 GND bottom
    StoreParameter "z_L2t", "z_L2b+t_cu"   ' L2 GND top
    StoreParameter "z_L1", "z_L2t+h_bond+h_sub"   ' top dielectric surface (L1 trace bottom)

    ' ---------------- units, background, boundaries ----------------
    AddHist "define units", "With Units" & vbLf & ".Geometry ""mm""" & vbLf & ".Frequency ""GHz""" & vbLf & ".Time ""ns""" & vbLf & "End With"
    Dim s As String
    s = "With Background" & vbLf & ".ResetBackground" & vbLf & ".Type ""Normal""" & vbLf
    s = s & ".XminSpace ""0.0""" & vbLf & ".XmaxSpace ""0.0""" & vbLf & ".YminSpace ""0.0""" & vbLf & ".YmaxSpace ""0.0""" & vbLf
    s = s & ".ZminSpace ""15.0""" & vbLf & ".ZmaxSpace ""15.0""" & vbLf & ".ApplyInAllDirections ""False""" & vbLf & "End With"
    AddHist "define background", s
    s = "With Boundary" & vbLf
    s = s & ".Xmin ""open""" & vbLf
    s = s & ".Xmax ""open""" & vbLf
    s = s & ".Ymin ""open""" & vbLf
    s = s & ".Ymax ""open""" & vbLf
    s = s & ".Zmin ""expanded open""" & vbLf
    s = s & ".Zmax ""expanded open""" & vbLf
    s = s & ".Xsymmetry ""none""" & vbLf & ".Ysymmetry ""none""" & vbLf & ".Zsymmetry ""none""" & vbLf & "End With"
    AddHist "define boundary", s

    ' ---------------- materials ----------------
    DefDielectric "RO3003", "3.00", "0.0013", "0.95", "0.80", "0.55"
    DefDielectric "RO4450B", "3.54", "0.004", "0.75", "0.55", "0.85"

    ' ---------------- components ----------------
    AddHist "new component: Substrate", "Component.New ""Substrate"""
    AddHist "new component: Planes", "Component.New ""Planes"""
    AddHist "new component: L1_top", "Component.New ""L1_top"""
    AddHist "new component: L3_strip", "Component.New ""L3_strip"""
    AddHist "new component: L5_bottom", "Component.New ""L5_bottom"""
    AddHist "new component: Vias", "Component.New ""Vias"""

    ' ---------------- dielectric stack (created first; metal created later overrides it) ----------------
    MakeBrick "sub_bottom", "Substrate", "RO3003", "-49.0000", "32.0000", "-32.0000", "32.0000", "0", "h_sub"
    MakeBrick "bond_bottom", "Substrate", "RO4450B", "-49.0000", "32.0000", "-32.0000", "32.0000", "h_sub", "z_L4t"
    MakeBrick "core_bottom", "Substrate", "RO3003", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L4t", "z_L3"
    MakeBrick "bond_mid", "Substrate", "RO4450B", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L3", "z_L3+h_bond"
    MakeBrick "core_top", "Substrate", "RO3003", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L3+h_bond", "z_L2b"
    MakeBrick "bond_top", "Substrate", "RO4450B", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L2b", "z_L2t+h_bond"
    MakeBrick "sub_top", "Substrate", "RO3003", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L2t+h_bond", "z_L1"

    ' ---------------- ground planes L2 / L4 (embedded in bond film, with antipads) ----------------
    MakeBrick "GND_L2", "Planes", "PEC", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L2b", "z_L2t"
    MakeBrick "GND_L4", "Planes", "PEC", "-49.0000", "32.0000", "-32.0000", "32.0000", "z_L4b", "z_L4t"
    MakeCyl "ap_L2_V15_E", "Planes", "PEC", "r_ap", "7.3171", "15.3950", "z_L2b", "z_L2t"
    AddHist "boolean subtract: ap_L2_V15_E", "Solid.Subtract ""Planes:GND_L2"", ""Planes:ap_L2_V15_E"""
    MakeCyl "ap_L4_V15_E", "Planes", "PEC", "r_ap", "7.3171", "15.3950", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V15_E", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V15_E"""
    MakeCyl "ap_L4_V35_E", "Planes", "PEC", "r_ap", "15.5462", "0.0000", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V35_E", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V35_E"""
    MakeCyl "ap_L2_V15_N", "Planes", "PEC", "r_ap", "-15.3950", "7.3171", "z_L2b", "z_L2t"
    AddHist "boolean subtract: ap_L2_V15_N", "Solid.Subtract ""Planes:GND_L2"", ""Planes:ap_L2_V15_N"""
    MakeCyl "ap_L4_V15_N", "Planes", "PEC", "r_ap", "-15.3950", "7.3171", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V15_N", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V15_N"""
    MakeCyl "ap_L4_V35_N", "Planes", "PEC", "r_ap", "0.0000", "15.5462", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V35_N", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V35_N"""
    MakeCyl "ap_L2_V15_W", "Planes", "PEC", "r_ap", "-7.3171", "-15.3950", "z_L2b", "z_L2t"
    AddHist "boolean subtract: ap_L2_V15_W", "Solid.Subtract ""Planes:GND_L2"", ""Planes:ap_L2_V15_W"""
    MakeCyl "ap_L4_V15_W", "Planes", "PEC", "r_ap", "-7.3171", "-15.3950", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V15_W", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V15_W"""
    MakeCyl "ap_L4_V35_W", "Planes", "PEC", "r_ap", "-15.5462", "0.0000", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V35_W", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V35_W"""
    MakeCyl "ap_L2_V15_S", "Planes", "PEC", "r_ap", "15.3950", "-7.3171", "z_L2b", "z_L2t"
    AddHist "boolean subtract: ap_L2_V15_S", "Solid.Subtract ""Planes:GND_L2"", ""Planes:ap_L2_V15_S"""
    MakeCyl "ap_L4_V15_S", "Planes", "PEC", "r_ap", "15.3950", "-7.3171", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V15_S", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V15_S"""
    MakeCyl "ap_L4_V35_S", "Planes", "PEC", "r_ap", "0.0000", "-15.5462", "z_L4b", "z_L4t"
    AddHist "boolean subtract: ap_L4_V35_S", "Solid.Subtract ""Planes:GND_L4"", ""Planes:ap_L4_V35_S"""

    ' ---------------- layer L3 traces ----------------
    MakeBrick "Z1_E", "L3_strip", "PEC", "0.0000", "15.5462", "0.0000-(W_sl100)/2", "0.0000+(W_sl100)/2", "z_L3", "z_L3+t_cu"
    MakeBrick "Z1_N", "L3_strip", "PEC", "0.0000-(W_sl100)/2", "0.0000+(W_sl100)/2", "0.0000", "15.5462", "z_L3", "z_L3+t_cu"
    MakeBrick "Z1_W", "L3_strip", "PEC", "-15.5462", "0.0000", "0.0000-(W_sl100)/2", "0.0000+(W_sl100)/2", "z_L3", "z_L3+t_cu"
    MakeBrick "Z1_S", "L3_strip", "PEC", "0.0000-(W_sl100)/2", "0.0000+(W_sl100)/2", "-15.5462", "0.0000", "z_L3", "z_L3+t_cu"
    MakeBrick "FEED_E", "L3_strip", "PEC", "15.5462", "32.0000", "0.0000-(W_sl50)/2", "0.0000+(W_sl50)/2", "z_L3", "z_L3+t_cu"
    MakeBrick "FEED_N", "L3_strip", "PEC", "0.0000-(W_sl50)/2", "0.0000+(W_sl50)/2", "15.5462", "32.0000", "z_L3", "z_L3+t_cu"
    MakeBrick "FEED_W", "L3_strip", "PEC", "-49.0000", "-15.5462", "0.0000-(W_sl50)/2", "0.0000+(W_sl50)/2", "z_L3", "z_L3+t_cu"
    MakeBrick "FEED_S", "L3_strip", "PEC", "0.0000-(W_sl50)/2", "0.0000+(W_sl50)/2", "-32.0000", "-15.5462", "z_L3", "z_L3+t_cu"
    MakeBrick "Z7", "L3_strip", "PEC", "0.0000", "15.6408", "0.0000-(W_sl50)/2", "0.0000+(W_sl50)/2", "z_L3", "z_L3+t_cu"
    RotZ "L3_strip:Z7", "0.0000", "0.0000", "-135.0000"
    MakeBrick "Z6", "L3_strip", "PEC", "0.0000", "0.0000+Lstub6", "0.0000-(W_sl85)/2", "0.0000+(W_sl85)/2", "z_L3", "z_L3+t_cu"
    RotZ "L3_strip:Z6", "0.0000", "0.0000", "45.0000"
    MakeBrick "Z8", "L3_strip", "PEC", "-11.0597-Lstub8", "-11.0597", "-11.0597-(W_sl100)/2", "-11.0597+(W_sl100)/2", "z_L3", "z_L3+t_cu"
    MakeBrick "FEED_IN_a", "L3_strip", "PEC", "-11.0597", "1.5837", "-11.0597-(W_sl50)/2", "-11.0597+(W_sl50)/2", "z_L3", "z_L3+t_cu"
    RotZ "L3_strip:FEED_IN_a", "-11.0597", "-11.0597", "-135.0000"
    MakeBrick "FEED_IN_b", "L3_strip", "PEC", "-20.0000-(W_sl50)/2", "-20.0000+(W_sl50)/2", "-32.0000", "-20.0000", "z_L3", "z_L3+t_cu"
    MakeCyl "BEND_K1", "L3_strip", "PEC", "(W_sl50)/2", "-20.0000", "-20.0000", "z_L3", "z_L3+t_cu"
    MakeCyl "NODE_A", "L3_strip", "PEC", "(W_sl50)/2", "0.0000", "0.0000", "z_L3", "z_L3+t_cu"
    MakeCyl "PAD3_E", "L3_strip", "PEC", "r_pad", "15.5462", "0.0000", "z_L3", "z_L3+t_cu"
    MakeCyl "PAD3_N", "L3_strip", "PEC", "r_pad", "0.0000", "15.5462", "z_L3", "z_L3+t_cu"
    MakeCyl "PAD3_W", "L3_strip", "PEC", "r_pad", "-15.5462", "0.0000", "z_L3", "z_L3+t_cu"
    MakeCyl "PAD3_S", "L3_strip", "PEC", "r_pad", "0.0000", "-15.5462", "z_L3", "z_L3+t_cu"

    ' ---------------- layer L5 traces ----------------
    MakeBrick "Z3_E", "L5_bottom", "PEC", "0.0000", "17.0454", "0.0000-(W_ms35)/2", "0.0000+(W_ms35)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z3_E", "0.0000", "0.0000", "64.5787"
    MakeBrick "Z2_E", "L5_bottom", "PEC", "7.3171", "24.7735", "15.3950-(W_ms50)/2", "15.3950+(W_ms50)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z2_E", "7.3171", "15.3950", "-61.8743"
    MakeBrick "Z3_N", "L5_bottom", "PEC", "0.0000", "17.0454", "0.0000-(W_ms35)/2", "0.0000+(W_ms35)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z3_N", "0.0000", "0.0000", "154.5787"
    MakeBrick "Z2_N", "L5_bottom", "PEC", "-15.3950", "2.0613", "7.3171-(W_ms50)/2", "7.3171+(W_ms50)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z2_N", "-15.3950", "7.3171", "28.1257"
    MakeBrick "Z3_W", "L5_bottom", "PEC", "0.0000", "17.0454", "0.0000-(W_ms35)/2", "0.0000+(W_ms35)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z3_W", "0.0000", "0.0000", "-115.4213"
    MakeBrick "Z2_W", "L5_bottom", "PEC", "-7.3171", "10.1392", "-15.3950-(W_ms50)/2", "-15.3950+(W_ms50)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z2_W", "-7.3171", "-15.3950", "118.1257"
    MakeBrick "Z3_S", "L5_bottom", "PEC", "0.0000", "17.0454", "0.0000-(W_ms35)/2", "0.0000+(W_ms35)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z3_S", "0.0000", "0.0000", "-25.4213"
    MakeBrick "Z2_S", "L5_bottom", "PEC", "15.3950", "32.8514", "-7.3171-(W_ms50)/2", "-7.3171+(W_ms50)/2", "-t_cu", "0"
    RotZ "L5_bottom:Z2_S", "15.3950", "-7.3171", "-151.8743"
    MakeCyl "NODE_N5", "L5_bottom", "PEC", "(W_ms35)/2", "0.0000", "0.0000", "-t_cu", "0"
    MakeCyl "PAD5B_E", "L5_bottom", "PEC", "r_pad", "7.3171", "15.3950", "-t_cu", "0"
    MakeCyl "PAD5O_E", "L5_bottom", "PEC", "r_pad", "15.5462", "0.0000", "-t_cu", "0"
    MakeCyl "PAD5B_N", "L5_bottom", "PEC", "r_pad", "-15.3950", "7.3171", "-t_cu", "0"
    MakeCyl "PAD5O_N", "L5_bottom", "PEC", "r_pad", "0.0000", "15.5462", "-t_cu", "0"
    MakeCyl "PAD5B_W", "L5_bottom", "PEC", "r_pad", "-7.3171", "-15.3950", "-t_cu", "0"
    MakeCyl "PAD5O_W", "L5_bottom", "PEC", "r_pad", "-15.5462", "0.0000", "-t_cu", "0"
    MakeCyl "PAD5B_S", "L5_bottom", "PEC", "r_pad", "15.3950", "-7.3171", "-t_cu", "0"
    MakeCyl "PAD5O_S", "L5_bottom", "PEC", "r_pad", "0.0000", "-15.5462", "-t_cu", "0"

    ' ---------------- layer L1 traces ----------------
    MakeBrick "Z5_E", "L1_top", "PEC", "0.0000", "17.4564", "0.0000-(W_ms50)/2", "0.0000+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z5_E", "0.0000", "0.0000", "125.3543"
    MakeBrick "Z4_E", "L1_top", "PEC", "-10.1008", "7.3556", "14.2372-(W_ms50)/2", "14.2372+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z4_E", "-10.1008", "14.2372", "3.8030"
    MakeBrick "Z5_N", "L1_top", "PEC", "0.0000", "17.4564", "0.0000-(W_ms50)/2", "0.0000+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z5_N", "0.0000", "0.0000", "-144.6457"
    MakeBrick "Z4_N", "L1_top", "PEC", "-14.2372", "3.2191", "-10.1008-(W_ms50)/2", "-10.1008+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z4_N", "-14.2372", "-10.1008", "93.8030"
    MakeBrick "Z5_W", "L1_top", "PEC", "0.0000", "17.4564", "0.0000-(W_ms50)/2", "0.0000+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z5_W", "0.0000", "0.0000", "-54.6457"
    MakeBrick "Z4_W", "L1_top", "PEC", "10.1008", "27.5572", "-14.2372-(W_ms50)/2", "-14.2372+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z4_W", "10.1008", "-14.2372", "-176.1970"
    MakeBrick "Z5_S", "L1_top", "PEC", "0.0000", "17.4564", "0.0000-(W_ms50)/2", "0.0000+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z5_S", "0.0000", "0.0000", "35.3543"
    MakeBrick "Z4_S", "L1_top", "PEC", "14.2372", "31.6936", "10.1008-(W_ms50)/2", "10.1008+(W_ms50)/2", "z_L1", "z_L1+t_cu"
    RotZ "L1_top:Z4_S", "14.2372", "10.1008", "-86.1970"
    MakeCyl "NODE_N1", "L1_top", "PEC", "(W_ms50)/2", "0.0000", "0.0000", "z_L1", "z_L1+t_cu"
    MakeBrick "RPAD_E", "L1_top", "PEC", "-10.8508", "-9.3508", "13.4872", "14.9872", "z_L1", "z_L1+t_cu"
    MakeBrick "GPAD_E", "L1_top", "PEC", "-13.4508", "-11.8508", "13.4872", "14.9872", "z_L1", "z_L1+t_cu"
    MakeBrick "RPAD_N", "L1_top", "PEC", "-14.9872", "-13.4872", "-10.8508", "-9.3508", "z_L1", "z_L1+t_cu"
    MakeBrick "GPAD_N", "L1_top", "PEC", "-14.9872", "-13.4872", "-13.4508", "-11.8508", "z_L1", "z_L1+t_cu"
    MakeBrick "RPAD_W", "L1_top", "PEC", "9.3508", "10.8508", "-14.9872", "-13.4872", "z_L1", "z_L1+t_cu"
    MakeBrick "GPAD_W", "L1_top", "PEC", "11.8508", "13.4508", "-14.9872", "-13.4872", "z_L1", "z_L1+t_cu"
    MakeBrick "RPAD_S", "L1_top", "PEC", "13.4872", "14.9872", "9.3508", "10.8508", "z_L1", "z_L1+t_cu"
    MakeBrick "GPAD_S", "L1_top", "PEC", "13.4872", "14.9872", "11.8508", "13.4508", "z_L1", "z_L1+t_cu"
    MakeCyl "PAD1_E", "L1_top", "PEC", "r_pad", "7.3171", "15.3950", "z_L1", "z_L1+t_cu"
    MakeCyl "PAD1_N", "L1_top", "PEC", "r_pad", "-15.3950", "7.3171", "z_L1", "z_L1+t_cu"
    MakeCyl "PAD1_W", "L1_top", "PEC", "r_pad", "-7.3171", "-15.3950", "z_L1", "z_L1+t_cu"
    MakeCyl "PAD1_S", "L1_top", "PEC", "r_pad", "15.3950", "-7.3171", "z_L1", "z_L1+t_cu"

    ' ---------------- vias ----------------
    MakeCyl "VG_E", "Vias", "PEC", "r_via", "-12.6508", "14.2372", "z_L2b", "z_L1+t_cu"
    MakeCyl "VG_N", "Vias", "PEC", "r_via", "-14.2372", "-12.6508", "z_L2b", "z_L1+t_cu"
    MakeCyl "VG_W", "Vias", "PEC", "r_via", "12.6508", "-14.2372", "z_L2b", "z_L1+t_cu"
    MakeCyl "VG_S", "Vias", "PEC", "r_via", "14.2372", "12.6508", "z_L2b", "z_L1+t_cu"
    MakeCyl "V15_E", "Vias", "PEC", "r_via", "7.3171", "15.3950", "-t_cu", "z_L1+t_cu"
    MakeCyl "V35_E", "Vias", "PEC", "r_via", "15.5462", "0.0000", "-t_cu", "z_L3+t_cu"
    MakeCyl "V15_N", "Vias", "PEC", "r_via", "-15.3950", "7.3171", "-t_cu", "z_L1+t_cu"
    MakeCyl "V35_N", "Vias", "PEC", "r_via", "0.0000", "15.5462", "-t_cu", "z_L3+t_cu"
    MakeCyl "V15_W", "Vias", "PEC", "r_via", "-7.3171", "-15.3950", "-t_cu", "z_L1+t_cu"
    MakeCyl "V35_W", "Vias", "PEC", "r_via", "-15.5462", "0.0000", "-t_cu", "z_L3+t_cu"
    MakeCyl "V15_S", "Vias", "PEC", "r_via", "15.3950", "-7.3171", "-t_cu", "z_L1+t_cu"
    MakeCyl "V35_S", "Vias", "PEC", "r_via", "0.0000", "-15.5462", "-t_cu", "z_L3+t_cu"
    MakeCyl "GV_BE_0", "Vias", "PEC", "r_via", "10.0888", "16.5431", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_1", "Vias", "PEC", "r_via", "8.4652", "18.1667", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_2", "Vias", "PEC", "r_via", "6.1691", "18.1667", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_3", "Vias", "PEC", "r_via", "4.5455", "16.5431", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_4", "Vias", "PEC", "r_via", "4.5455", "14.2470", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_5", "Vias", "PEC", "r_via", "6.1691", "12.6234", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_6", "Vias", "PEC", "r_via", "8.4652", "12.6234", "z_L4b", "z_L2t"
    MakeCyl "GV_BE_7", "Vias", "PEC", "r_via", "10.0888", "14.2470", "z_L4b", "z_L2t"
    MakeCyl "GV_OE_p", "Vias", "PEC", "r_via", "15.5462", "4.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_OE_m", "Vias", "PEC", "r_via", "15.5462", "-4.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_0", "Vias", "PEC", "r_via", "-12.6234", "8.4652", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_1", "Vias", "PEC", "r_via", "-14.2470", "10.0888", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_2", "Vias", "PEC", "r_via", "-16.5431", "10.0888", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_3", "Vias", "PEC", "r_via", "-18.1667", "8.4652", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_4", "Vias", "PEC", "r_via", "-18.1667", "6.1691", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_5", "Vias", "PEC", "r_via", "-16.5431", "4.5455", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_6", "Vias", "PEC", "r_via", "-14.2470", "4.5455", "z_L4b", "z_L2t"
    MakeCyl "GV_BN_7", "Vias", "PEC", "r_via", "-12.6234", "6.1691", "z_L4b", "z_L2t"
    MakeCyl "GV_ON_p", "Vias", "PEC", "r_via", "-4.0000", "15.5462", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_0", "Vias", "PEC", "r_via", "-4.5455", "-14.2470", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_1", "Vias", "PEC", "r_via", "-6.1691", "-12.6234", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_2", "Vias", "PEC", "r_via", "-8.4652", "-12.6234", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_3", "Vias", "PEC", "r_via", "-10.0888", "-14.2470", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_4", "Vias", "PEC", "r_via", "-10.0888", "-16.5431", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_5", "Vias", "PEC", "r_via", "-8.4652", "-18.1667", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_6", "Vias", "PEC", "r_via", "-6.1691", "-18.1667", "z_L4b", "z_L2t"
    MakeCyl "GV_BW_7", "Vias", "PEC", "r_via", "-4.5455", "-16.5431", "z_L4b", "z_L2t"
    MakeCyl "GV_OW_p", "Vias", "PEC", "r_via", "-15.5462", "-4.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_BS_0", "Vias", "PEC", "r_via", "18.1667", "-6.1691", "z_L4b", "z_L2t"
    MakeCyl "GV_BS_3", "Vias", "PEC", "r_via", "12.6234", "-6.1691", "z_L4b", "z_L2t"
    MakeCyl "GV_BS_4", "Vias", "PEC", "r_via", "12.6234", "-8.4652", "z_L4b", "z_L2t"
    MakeCyl "GV_BS_5", "Vias", "PEC", "r_via", "14.2470", "-10.0888", "z_L4b", "z_L2t"
    MakeCyl "GV_BS_6", "Vias", "PEC", "r_via", "16.5431", "-10.0888", "z_L4b", "z_L2t"
    MakeCyl "GV_BS_7", "Vias", "PEC", "r_via", "18.1667", "-8.4652", "z_L4b", "z_L2t"
    MakeCyl "GV_OS_p", "Vias", "PEC", "r_via", "4.0000", "-15.5462", "z_L4b", "z_L2t"
    MakeCyl "GV_F47", "Vias", "PEC", "r_via", "-47.0000", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F48", "Vias", "PEC", "r_via", "-42.1875", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F49", "Vias", "PEC", "r_via", "-37.3750", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F50", "Vias", "PEC", "r_via", "-32.5625", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F51", "Vias", "PEC", "r_via", "-27.7500", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F52", "Vias", "PEC", "r_via", "-13.3125", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F53", "Vias", "PEC", "r_via", "-8.5000", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F54", "Vias", "PEC", "r_via", "5.9375", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F55", "Vias", "PEC", "r_via", "10.7500", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F56", "Vias", "PEC", "r_via", "15.5625", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F57", "Vias", "PEC", "r_via", "20.3750", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F58", "Vias", "PEC", "r_via", "25.1875", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F59", "Vias", "PEC", "r_via", "30.0000", "-30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F60", "Vias", "PEC", "r_via", "30.0000", "-25.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F61", "Vias", "PEC", "r_via", "30.0000", "-20.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F62", "Vias", "PEC", "r_via", "30.0000", "-15.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F63", "Vias", "PEC", "r_via", "30.0000", "-10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F64", "Vias", "PEC", "r_via", "30.0000", "-5.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F65", "Vias", "PEC", "r_via", "30.0000", "5.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F66", "Vias", "PEC", "r_via", "30.0000", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F67", "Vias", "PEC", "r_via", "30.0000", "15.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F68", "Vias", "PEC", "r_via", "30.0000", "20.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F69", "Vias", "PEC", "r_via", "30.0000", "25.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F70", "Vias", "PEC", "r_via", "30.0000", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F71", "Vias", "PEC", "r_via", "25.1875", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F72", "Vias", "PEC", "r_via", "20.3750", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F73", "Vias", "PEC", "r_via", "15.5625", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F74", "Vias", "PEC", "r_via", "10.7500", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F75", "Vias", "PEC", "r_via", "5.9375", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F76", "Vias", "PEC", "r_via", "-8.5000", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F77", "Vias", "PEC", "r_via", "-13.3125", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F78", "Vias", "PEC", "r_via", "-18.1250", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F79", "Vias", "PEC", "r_via", "-22.9375", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F80", "Vias", "PEC", "r_via", "-27.7500", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F81", "Vias", "PEC", "r_via", "-32.5625", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F82", "Vias", "PEC", "r_via", "-37.3750", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F83", "Vias", "PEC", "r_via", "-42.1875", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F84", "Vias", "PEC", "r_via", "-47.0000", "30.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F85", "Vias", "PEC", "r_via", "-47.0000", "25.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F86", "Vias", "PEC", "r_via", "-47.0000", "20.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F87", "Vias", "PEC", "r_via", "-47.0000", "15.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F88", "Vias", "PEC", "r_via", "-47.0000", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F89", "Vias", "PEC", "r_via", "-47.0000", "5.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F90", "Vias", "PEC", "r_via", "-47.0000", "-5.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F91", "Vias", "PEC", "r_via", "-47.0000", "-10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F92", "Vias", "PEC", "r_via", "-47.0000", "-15.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F93", "Vias", "PEC", "r_via", "-47.0000", "-20.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_F94", "Vias", "PEC", "r_via", "-47.0000", "-25.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G1_1", "Vias", "PEC", "r_via", "-40.5833", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G1_2", "Vias", "PEC", "r_via", "-40.5833", "-16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G1_6", "Vias", "PEC", "r_via", "-40.5833", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G1_7", "Vias", "PEC", "r_via", "-40.5833", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G1_8", "Vias", "PEC", "r_via", "-40.5833", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G2_1", "Vias", "PEC", "r_via", "-34.1667", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G2_2", "Vias", "PEC", "r_via", "-34.1667", "-16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G2_6", "Vias", "PEC", "r_via", "-34.1667", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G2_7", "Vias", "PEC", "r_via", "-34.1667", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G2_8", "Vias", "PEC", "r_via", "-34.1667", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G3_1", "Vias", "PEC", "r_via", "-27.7500", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G3_2", "Vias", "PEC", "r_via", "-27.7500", "-16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G3_6", "Vias", "PEC", "r_via", "-27.7500", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G3_7", "Vias", "PEC", "r_via", "-27.7500", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G3_8", "Vias", "PEC", "r_via", "-27.7500", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G4_6", "Vias", "PEC", "r_via", "-21.3333", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G4_7", "Vias", "PEC", "r_via", "-21.3333", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G4_8", "Vias", "PEC", "r_via", "-21.3333", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G5_1", "Vias", "PEC", "r_via", "-14.9167", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G5_7", "Vias", "PEC", "r_via", "-14.9167", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G5_8", "Vias", "PEC", "r_via", "-14.9167", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G6_1", "Vias", "PEC", "r_via", "-8.5000", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G6_5", "Vias", "PEC", "r_via", "-8.5000", "3.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G6_6", "Vias", "PEC", "r_via", "-8.5000", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G6_7", "Vias", "PEC", "r_via", "-8.5000", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G6_8", "Vias", "PEC", "r_via", "-8.5000", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G8_1", "Vias", "PEC", "r_via", "4.3333", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G8_3", "Vias", "PEC", "r_via", "4.3333", "-10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G8_4", "Vias", "PEC", "r_via", "4.3333", "-3.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G8_6", "Vias", "PEC", "r_via", "4.3333", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G8_8", "Vias", "PEC", "r_via", "4.3333", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G9_1", "Vias", "PEC", "r_via", "10.7500", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G9_2", "Vias", "PEC", "r_via", "10.7500", "-16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G9_3", "Vias", "PEC", "r_via", "10.7500", "-10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G9_4", "Vias", "PEC", "r_via", "10.7500", "-3.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G9_5", "Vias", "PEC", "r_via", "10.7500", "3.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G9_8", "Vias", "PEC", "r_via", "10.7500", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G10_1", "Vias", "PEC", "r_via", "17.1667", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G10_2", "Vias", "PEC", "r_via", "17.1667", "-16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G10_6", "Vias", "PEC", "r_via", "17.1667", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G10_8", "Vias", "PEC", "r_via", "17.1667", "23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G11_1", "Vias", "PEC", "r_via", "23.5833", "-23.3333", "z_L4b", "z_L2t"
    MakeCyl "GV_G11_2", "Vias", "PEC", "r_via", "23.5833", "-16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_G11_3", "Vias", "PEC", "r_via", "23.5833", "-10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G11_6", "Vias", "PEC", "r_via", "23.5833", "10.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_G11_7", "Vias", "PEC", "r_via", "23.5833", "16.6667", "z_L4b", "z_L2t"
    MakeCyl "GV_X0", "Vias", "PEC", "r_via", "-31.0000", "-4.0000", "z_L4b", "z_L2t"
    MakeCyl "GV_X1", "Vias", "PEC", "r_via", "-39.0000", "-7.5000", "z_L4b", "z_L2t"
    MakeCyl "GV_X2", "Vias", "PEC", "r_via", "-23.5000", "-7.5000", "z_L4b", "z_L2t"

    ' ---------------- isolation resistors (lumped R between node pad and grounded pad, on L1) ----------------
    MakeResistor "R_E", "-10.8508", "14.2372", "-11.8508", "14.2372"
    MakeResistor "R_N", "-14.2372", "-10.8508", "-14.2372", "-11.8508"
    MakeResistor "R_W", "10.8508", "-14.2372", "11.8508", "-14.2372"
    MakeResistor "R_S", "14.2372", "10.8508", "14.2372", "11.8508"

    ' ---------------- waveguide ports on the L3 stripline (span L4..L2) ----------------
    MakePort 1, "ymin", "-27.0000", "-13.0000", "-32.0000", "-32.0000", "0"
    MakePort 2, "xmin", "-49.0000", "-49.0000", "-7.0000", "7.0000", "-17.0000"
    MakePort 3, "ymin", "-7.0000", "7.0000", "-32.0000", "-32.0000", "0.0000"
    MakePort 4, "ymax", "-7.0000", "7.0000", "32.0000", "32.0000", "0.0000"
    MakePort 5, "xmax", "32.0000", "32.0000", "-7.0000", "7.0000", "0.0000"

    ' ---------------- frequency range ----------------
    AddHist "define frequency range", "Solver.FrequencyRange ""2"", ""4"""

    Plot.ZoomToStructure
    MsgBox "4-way Knochel-Mayer divider built. Check: Simulation > Setup Solver (S-parameter normalisation 50 ohm).", vbInformation
End Sub

' ============================== helper routines ==============================
Function Qt(ByVal txt As String) As String
    Qt = Chr(34) & txt & Chr(34)
End Function

Sub AddHist(ByVal caption As String, ByVal cmd As String)
    AddToHistory caption, cmd
End Sub

Sub DefDielectric(ByVal nm As String, ByVal eps As String, ByVal tand As String, ByVal r As String, ByVal g As String, ByVal b As String)
    Dim s As String
    s = "With Material" & vbLf & ".Reset" & vbLf & ".Name " & Qt(nm) & vbLf
    s = s & ".FrqType " & Qt("all") & vbLf & ".Type " & Qt("Normal") & vbLf
    s = s & ".SetMaterialUnit " & Qt("GHz") & ", " & Qt("mm") & vbLf
    s = s & ".Epsilon " & Qt(eps) & vbLf & ".Mu " & Qt("1.0") & vbLf
    s = s & ".TanD " & Qt(tand) & vbLf & ".TanDFreq " & Qt("2.75") & vbLf
    s = s & ".TanDGiven " & Qt("True") & vbLf & ".TanDModel " & Qt("ConstTanD") & vbLf
    s = s & ".Colour " & Qt(r) & ", " & Qt(g) & ", " & Qt(b) & vbLf
    s = s & ".Create" & vbLf & "End With"
    AddHist "define material: " & nm, s
End Sub

Sub MakeBrick(ByVal nm As String, ByVal comp As String, ByVal mat As String, ByVal x1 As String, ByVal x2 As String, _
              ByVal y1 As String, ByVal y2 As String, ByVal z1 As String, ByVal z2 As String)
    Dim s As String
    s = "With Brick" & vbLf & ".Reset" & vbLf & ".Name " & Qt(nm) & vbLf & ".Component " & Qt(comp) & vbLf
    s = s & ".Material " & Qt(mat) & vbLf
    s = s & ".Xrange " & Qt(x1) & ", " & Qt(x2) & vbLf
    s = s & ".Yrange " & Qt(y1) & ", " & Qt(y2) & vbLf
    s = s & ".Zrange " & Qt(z1) & ", " & Qt(z2) & vbLf
    s = s & ".Create" & vbLf & "End With"
    AddHist "define brick: " & comp & ":" & nm, s
End Sub

Sub MakeCyl(ByVal nm As String, ByVal comp As String, ByVal mat As String, ByVal r As String, ByVal xc As String, _
            ByVal yc As String, ByVal z1 As String, ByVal z2 As String)
    Dim s As String
    s = "With Cylinder" & vbLf & ".Reset" & vbLf & ".Name " & Qt(nm) & vbLf & ".Component " & Qt(comp) & vbLf
    s = s & ".Material " & Qt(mat) & vbLf & ".OuterRadius " & Qt(r) & vbLf & ".InnerRadius " & Qt("0") & vbLf
    s = s & ".Axis " & Qt("z") & vbLf & ".Zrange " & Qt(z1) & ", " & Qt(z2) & vbLf
    s = s & ".Xcenter " & Qt(xc) & vbLf & ".Ycenter " & Qt(yc) & vbLf & ".Segments " & Qt("0") & vbLf
    s = s & ".Create" & vbLf & "End With"
    AddHist "define cylinder: " & comp & ":" & nm, s
End Sub

Sub RotZ(ByVal fullName As String, ByVal xc As String, ByVal yc As String, ByVal ang As String)
    ' rotate a shape about the vertical axis through (xc, yc) by ang degrees (counter-clockwise)
    Dim s As String
    s = "With Transform" & vbLf & ".Reset" & vbLf & ".Name " & Qt(fullName) & vbLf
    s = s & ".Origin " & Qt("Free") & vbLf & ".Center " & Qt(xc) & ", " & Qt(yc) & ", " & Qt("0") & vbLf
    s = s & ".Angle " & Qt("0") & ", " & Qt("0") & ", " & Qt(ang) & vbLf
    s = s & ".MultipleObjects " & Qt("False") & vbLf & ".GroupObjects " & Qt("False") & vbLf
    s = s & ".Repetitions " & Qt("1") & vbLf & ".MultipleSelection " & Qt("False") & vbLf
    s = s & ".Transform " & Qt("Shape") & ", " & Qt("Rotate") & vbLf & "End With"
    AddHist "transform: rotate " & fullName, s
End Sub

Sub MakeResistor(ByVal nm As String, ByVal x1 As String, ByVal y1 As String, ByVal x2 As String, ByVal y2 As String)
    Dim s As String
    s = "With LumpedElement" & vbLf & ".Reset" & vbLf & ".SetName " & Qt(nm) & vbLf
    s = s & ".SetType " & Qt("RLCSerial") & vbLf & ".SetR " & Qt("R_iso") & vbLf
    s = s & ".SetL " & Qt("0") & vbLf & ".SetC " & Qt("0") & vbLf
    s = s & ".SetP1 " & Qt("False") & ", " & Qt(x1) & ", " & Qt(y1) & ", " & Qt("z_L1+t_cu/2") & vbLf
    s = s & ".SetP2 " & Qt("False") & ", " & Qt(x2) & ", " & Qt(y2) & ", " & Qt("z_L1+t_cu/2") & vbLf
    s = s & ".Create" & vbLf & "End With"
    AddHist "define lumped element: " & nm, s
End Sub

Sub MakePort(ByVal num As Integer, ByVal orient As String, ByVal x1 As String, ByVal x2 As String, _
             ByVal y1 As String, ByVal y2 As String, ByVal refDist As String)
    Dim s As String
    s = "With Port" & vbLf & ".Reset" & vbLf & ".PortNumber " & Qt(CStr(num)) & vbLf
    s = s & ".NumberOfModes " & Qt("1") & vbLf & ".AdjustPolarization " & Qt("False") & vbLf
    s = s & ".PolarizationAngle " & Qt("0.0") & vbLf & ".ReferencePlaneDistance " & Qt(refDist) & vbLf
    s = s & ".TextSize " & Qt("50") & vbLf & ".Coordinates " & Qt("Free") & vbLf
    s = s & ".Orientation " & Qt(orient) & vbLf & ".PortOnBound " & Qt("False") & vbLf
    s = s & ".ClipPickedPortToBound " & Qt("False") & vbLf
    s = s & ".Xrange " & Qt(x1) & ", " & Qt(x2) & vbLf & ".Yrange " & Qt(y1) & ", " & Qt(y2) & vbLf
    s = s & ".Zrange " & Qt("z_L4b") & ", " & Qt("z_L2t") & vbLf
    s = s & ".XrangeAdd " & Qt("0.0") & ", " & Qt("0.0") & vbLf & ".YrangeAdd " & Qt("0.0") & ", " & Qt("0.0") & vbLf
    s = s & ".ZrangeAdd " & Qt("0.0") & ", " & Qt("0.0") & vbLf
    s = s & ".SingleEnded " & Qt("False") & vbLf & ".Create" & vbLf & "End With"
    AddHist "define port: " & CStr(num), s
End Sub
