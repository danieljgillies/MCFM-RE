      integer :: kcase,kcasebelow
      common/kcase/kcase,kcasebelow
      integer, parameter :: kW_only=   1
      integer, parameter :: kW_1jet=   2
      integer, parameter :: kWbfrmc=   3
      integer, parameter :: kW_cjet=   4
      integer, parameter :: kWcjet0=   5
      integer, parameter :: kWbbmas=   6
      integer, parameter :: kWbbbar=   7
      integer, parameter :: kW_2jet=   8
      integer, parameter :: kW_3jet=   9
      integer, parameter :: kWbbjet=  10
      integer, parameter :: kZ_only=  11
      integer, parameter :: kttZbbl=  12
      integer, parameter :: kZ_1jet=  13
      integer, parameter :: kZ_2jet=  14
      integer, parameter :: kZ_3jet=  15
      integer, parameter :: kZbbmas=  16
      integer, parameter :: kZbbbar=  17
      integer, parameter :: kZbbjet=  18
      integer, parameter :: kWWqqbr=  19
      integer, parameter :: kWWnpol=  20
      integer, parameter :: kWW_jet=  21
      integer, parameter :: kWZbbar=  22
      integer, parameter :: kZZlept=  23
      integer, parameter :: kZZ_jet=  24
      integer, parameter :: kWHbbar=  25
      integer, parameter :: kWHgaga=  26
      integer, parameter :: kWH__WW=  27
      integer, parameter :: kWH__ZZ=  28
      integer, parameter :: kZHbbar=  29
      integer, parameter :: kZHgaga=  30
      integer, parameter :: kZH__WW=  31
      integer, parameter :: kZH__ZZ=  32
      integer, parameter :: kggfus0=  33
      integer, parameter :: kHWW_4l=  34
      integer, parameter :: kHWW_tb=  35
      integer, parameter :: kHWWint=  36
      integer, parameter :: kHWWHpi=  37
      integer, parameter :: kggWW4l=  38
      integer, parameter :: kggWWbx=  39
      integer, parameter :: kHWW2lq=  40
      integer, parameter :: kHWWdkW=  41
      integer, parameter :: kHZZ_4l=  42
      integer, parameter :: kHZZ_tb=  43
      integer, parameter :: kHZZint=  44
      integer, parameter :: kHZZHpi=  45
      integer, parameter :: kggZZ4l=  46
      integer, parameter :: kggZZbx=  47
      integer, parameter :: kHZZqgI=  48
      integer, parameter :: kHi_Zga=  49
      integer, parameter :: kHVV_tb=  50
      integer, parameter :: kHVVint=  51
      integer, parameter :: kHVVHpi=  52
      integer, parameter :: kggVV4l=  53
      integer, parameter :: kggVVbx=  54
      integer, parameter :: kH_1jet=  55
      integer, parameter :: ktt_bbl=  56
      integer, parameter :: ktt_ldk=  57
      integer, parameter :: ktt_bbu=  58
      integer, parameter :: ktt_udk=  59
      integer, parameter :: ktt_bbh=  60
      integer, parameter :: ktt_hdk=  61
      integer, parameter :: ktthWdk=  62
      integer, parameter :: kqq_ttg=  63
      integer, parameter :: ktt_tot=  64
      integer, parameter :: kbb_tot=  65
      integer, parameter :: kcc_tot=  66
      integer, parameter :: ktt_glu=  67
      integer, parameter :: kbq_tpq=  68
      integer, parameter :: kttdkay=  69
      integer, parameter :: kt_bbar=  70
      integer, parameter :: ktdecay=  71
      integer, parameter :: kW_tndk=  72
      integer, parameter :: kW_twdk=  73
      integer, parameter :: kWtdkay=  74
      integer, parameter :: kWtbwdk=  75
      integer, parameter :: kWtbndk=  76
      integer, parameter :: khttjet=  77
      integer, parameter :: kggfus1=  78
      integer, parameter :: kattjet=  79
      integer, parameter :: kHWWjet=  80
      integer, parameter :: kHZZjet=  81
      integer, parameter :: kqq_Hqq=  82
      integer, parameter :: kqq_HWW=  83
      integer, parameter :: kqq_HZZ=  84
      integer, parameter :: kqq_Hgg=  85
      integer, parameter :: kqqHqqg=  86
      integer, parameter :: kqqZZqq=  87
      integer, parameter :: ktautau=  88
      integer, parameter :: kqqWWqq=  89
      integer, parameter :: kqqVVqq=  90
      integer, parameter :: kqqWWss=  91
      integer, parameter :: kqqWZqq=  92
      integer, parameter :: kqg_tbq=  93
      integer, parameter :: kqgtbqq=  94
      integer, parameter :: k4ftwdk=  95
      integer, parameter :: kdk_4ft=  96
      integer, parameter :: k4ftjet=  97
      integer, parameter :: kqq_tbg=  98
      integer, parameter :: kqqtbgg=  99
      integer, parameter :: kepem3j= 100
      integer, parameter :: kWpWp2j= 101
      integer, parameter :: kWpWp3j= 102
      integer, parameter :: kWpmZjj= 103
      integer, parameter :: kWpmZbj= 104
      integer, parameter :: kWpmZbb= 105
      integer, parameter :: kgQ__ZQ= 106
      integer, parameter :: kgagajj= 107
      integer, parameter :: kggfus2= 108
      integer, parameter :: kHWW2jt= 109
      integer, parameter :: kHZZ2jt= 110
      integer, parameter :: kggfus3= 111
      integer, parameter :: kHWW3jt= 112
      integer, parameter :: kHZZ3jt= 113
      integer, parameter :: kdirgam= 114
      integer, parameter :: kgamjet= 115
      integer, parameter :: khflgam= 116
      integer, parameter :: kgamgam= 117
      integer, parameter :: kgmgmjt= 118
      integer, parameter :: ktrigam= 119
      integer, parameter :: kgmgmjj= 120
      integer, parameter :: kfourga= 121
      integer, parameter :: ktwojet= 122
      integer, parameter :: kthrjet= 123
      integer, parameter :: kWgamma= 124
      integer, parameter :: kWgajet= 125
      integer, parameter :: kZgamma= 126
      integer, parameter :: kZgajet= 127
      integer, parameter :: kZ_2gam= 128
      integer, parameter :: kZ2gajt= 129
      integer, parameter :: kZga2jt= 130
      integer, parameter :: kW_bjet= 131
      integer, parameter :: kWcjetg= 132
      integer, parameter :: kZ_bjet= 133
      integer, parameter :: kZbjetg= 134
      integer, parameter :: kWcsbar= 135
      integer, parameter :: kWcs_ms= 136
      integer, parameter :: kW_2gam= 137
      integer, parameter :: kWbbjem= 138
      integer, parameter :: kWttmas= 139
      integer, parameter :: kqq_ttw= 140
      integer, parameter :: kttwldk= 141
      integer, parameter :: kqq_ttz= 142
      integer, parameter :: kqqtthz= 143
      integer, parameter :: kH_tjet= 144
      integer, parameter :: kH_tdkj= 145
      integer, parameter :: kZ_tjet= 146
      integer, parameter :: kZt2jet= 147
      integer, parameter :: kZ_tdkj= 148
      integer, parameter :: kZtdk2j= 149
      integer, parameter :: kHHpair= 150
      integer, parameter :: kWH1jet= 151
      integer, parameter :: kZH1jet= 152
      integer, parameter :: ktottth= 153
      integer, parameter :: kqq_tth= 154
      integer, parameter :: ktth_ww= 155
      integer, parameter :: kdm_jet= 156
      integer, parameter :: kdm_gam= 157
      integer, parameter :: kdm2jet= 158
      integer, parameter :: kdm_gaj= 159
      integer, parameter :: kvlchk2= 160
      integer, parameter :: kvlchk3= 161
      integer, parameter :: kvlchk4= 162
      integer, parameter :: kvlchk5= 163
      integer, parameter :: kvlchk6= 164
      integer, parameter :: kvlchk8= 165
      integer, parameter :: kvlchkm= 166
      integer, parameter :: kvlchm3= 167
      integer, parameter :: kvlchwt= 168
      integer, parameter :: kvlchwn= 169
      integer, parameter :: kvlchwg= 170
      integer, parameter :: kvlchwh= 171
      integer, parameter :: kWWqqdk= 172
      integer, parameter :: kHigaga= 173
      integer, parameter :: kdmgamj= 174
      integer, parameter :: kHgagaj= 175
      integer, parameter :: kHZZpjt= 176
      integer, parameter :: kHmZZ4l= 177
      integer, parameter :: kHZZ_jj= 178
      integer, parameter :: kW_cwdk= 179
      integer, parameter :: kZccmas= 180
      integer, parameter :: ktotttz= 181
      integer, parameter :: kW_frag= 182
      integer, parameter :: kWb2jet= 183
      integer, parameter :: kWW2jet= 184
      integer, parameter :: kZ_frag= 185
      integer, parameter :: kqqttbb= 186
      integer, parameter :: kqqttgg= 187
      integer, parameter :: kqg_tbb= 188
      integer, parameter :: kHt2jet= 189
      integer, parameter :: kttbdkh= 190
      integer, parameter :: kttbdkl= 191
      integer, parameter :: khlljet= 192
      integer, parameter :: kZHlept= 193
      integer, parameter :: kWHbbdk= 194
      integer, parameter :: kZHbbdk= 195
      integer, parameter :: kgg2gam= 196
      integer, parameter :: khjetma= 197
      integer, parameter :: kh2jmas= 198
      integer, parameter :: kWga2jt= 199
      integer, parameter :: kHi_Zaj= 200
      integer, parameter :: ktt_mix= 201
      integer, parameter :: ktwo_ew= 202
      integer, parameter :: kgg2lep= 203
