
      double complex function hjetmass_bubble_pppm_s124_dp 
     &     (i1,i2,i3,i4,za,zb,mt,p,flip)
          implicit double complex (t)
          integer i1,i2,i3,i4
          include 'types.f'
          include 'mxpart.f'
          include 'constants.f'
          include 'zprods_decl.f'
          double complex ret
          double precision mt
          double precision p(mxpart,4)
          double complex alpha
          logical flip

      alpha = (za(i1,i2)*zb(i2,i1) + za(i1,i4)*zb(i4,i1) +
     & za(i2,i4)*zb(i4,i2))/(za(i1,i4)*zb(i4,i1) + za(i2,i4)*zb(i4,i2))

      p(5,:) = dreal(alpha)*p(i4,:)
      p(6,:) = (1d0-dreal(alpha))*p(i4,:) + p(i1,:) + p(i2,:)
      if (flip .eqv. .true.) then
          call spinoru_dp(6,p,zb,za)
      else
          call spinoru_dp(6,p,za,zb)
      end if

      t1 = za(5, i3)
      t2 = za(6, i4)
      t3 = za(i2, i4)
      t4 = zb(i2, 5)
      t5 = za(6, i3)
      t6 = zb(i2, 6)
      t7 = 0.1D1 / t1
      t8 = t5 * t7
      t9 = t8 * t6 + t4
      t10 = zb(i2, i1)
      t11 = zb(i3, 5)
      t12 = za(i1, i2)
      t13 = za(i1, i4)
      t14 = za(i3, i4)
      t15 = zb(i3, 6)
      t16 = 0.1D1 / t15
      t17 = t11 * t16
      t18 = t17 + t8
      t19 = zb(i4, i1)
      t20 = t12 * t10
      t21 = t13 * t19
      t22 = t3 * zb(i4, i2)
      t23 = t21 + t22 + t20
      t24 = zb(i4, 6)
      t25 = zb(i3, i1)
      t26 = t6 * t11
      t27 = t26 * t16 - t4
      t28 = zb(i1, 5)
      t29 = za(5, i1)
      t30 = zb(i1, 6)
      t31 = za(6, i1)
      t32 = za(6, i2)
      t33 = t31 * t28
      t34 = t32 * t4
      t35 = t5 * t11
      t36 = t35 + t33 + t34
      t37 = t33 * t36
      t38 = za(5, i2)
      t39 = t29 * t30
      t40 = t38 * t6
      t41 = t1 * t15
      t42 = t41 + t39 + t40
      t43 = t29 * t28
      t44 = t31 * t30
      t45 = t2 * t24
      t46 = -t45 + t43 - t44
      t47 = 4
      t48 = t39 * t33
      t46 = t46 ** 2 + t48 * t47
      t46 = cdsqrt(t46)
      t49 = -t45 + t43 - t44 + t46
      t50 = 0.1D1 / t42
      t51 = t4 * t15
      t52 = t26 + t51
      t53 = t50 ** 2
      t54 = t50 * t53
      t55 = t53 * t52
      t56 = 0.1D1 / t24
      t57 = 0.1D1 / t49
      t58 = t30 ** 2
      t59 = t58 ** 2
      t60 = t30 * t59
      t61 = t30 * t58
      t62 = t29 ** 2
      t63 = t62 ** 2
      t64 = t29 * t63
      t65 = t29 * t62
      t66 = t57 ** 2
      t67 = t66 ** 2
      t68 = t57 * t67
      t69 = t57 * t66
      t70 = t66 * t56
      t71 = t70 * t58
      t72 = t71 * t62
      t46 = -t45 + t43 - t44 - t46
      t73 = t35 + t33 + t34
      t74 = t38 * t4
      t75 = t32 * t6
      t76 = t1 * t11
      t77 = t5 * t15
      t78 = -t77 - t75 + t76 - t45 + t74 + t43 - t44
      t78 = t78 ** 2
      t79 = t47 * t73 * t42 + t78
      t79 = cdsqrt(t79)
      t80 = -t77 - t75 + t76 - t45 + t74 + t43 - t44 + t79
      t79 = -t77 - t75 + t76 - t45 + t74 + t43 - t44 - t79
      t81 = zb(i3, i2)
      t82 = t31 ** 2
      t83 = t82 * t30
      t84 = t29 * (t35 + t34) + t31 * (t76 - t21 - t45 + t74) - t83
      t85 = t14 * zb(i4, i3)
      t86 = za(i1, i3) * t25
      t87 = za(i2, i3) * t81
      t88 = t87 + t20 + t22 + t75 + t77 + t85 + t86
      t89 = t35 + t34
      t90 = t89 * t30
      t91 = t88 * t28
      t92 = t31 * (t91 + t90)
      t93 = t45 * t89
      t94 = t21 * t89
      t95 = 2
      t96 = t28 ** 2
      t97 = t28 * t96
      t98 = t47 * t29
      t99 = -t28 * t84 * t95 - t98 * t31 * t96 + t92 + t93 + t94
      t100 = 0.1D1 / t79
      t101 = 0.1D1 / t46
      t19 = 0.1D1 / t19
      t102 = 0.1D1 / t80
      t103 = t100 + t102
      t104 = t39 * t101
      t105 = t103 * t42
      t106 = t105 + t104
      t107 = t100 ** 2
      t108 = t107 ** 2
      t109 = t100 * t107
      t110 = t101 ** 2
      t111 = t110 ** 2
      t112 = t101 * t111
      t113 = t101 * t110
      t114 = t42 ** 2
      t115 = t114 ** 2
      t116 = t114 * t115
      t117 = t42 * t115
      t118 = t42 * t114
      t119 = t102 ** 2
      t120 = t119 ** 2
      t121 = t102 * t119
      t122 = t119 * t19
      t123 = t122 * t110 * t107
      t124 = t123 * t115
      t125 = t124 * t37
      t126 = t114 * t119
      t127 = t58 * t62
      t128 = t127 * t110
      t129 = t126 * t62 * t58 * t110
      t130 = t42 * t102
      t131 = t42 * t100
      t132 = t107 * t114
      t133 = t128 * t119 * t107 * t115
      t134 = t76 + t43 + t74
      t135 = t29 * t32
      t136 = t31 * t38
      t137 = t136 + t135
      t138 = t43 - t44
      t139 = t1 * t31
      t140 = t29 * t5
      t141 = t139 + t140
      t142 = t137 * t6
      t143 = t141 * t15
      t137 = t137 * t4
      t144 = t141 * t11
      t145 = t21 * t138
      t146 = t45 * (t76 + t74 - t44 - t21)
      t147 = t2 ** 2
      t24 = t147 * t24 ** 2
      t148 = t21 * (t77 - t74 + t75 - t76)
      t149 = t45 * t88
      t150 = t82 * t58
      t151 = 0.1D1 / t42
      t152 = (0.1D1 / 0.2D1)
      t153 = t80 * t151
      t154 = t153 * t152
      t155 = -t154 * t30 + t28
      t156 = -t154 * t6 + t4
      t78 = t47 * t73 * t42 + t78
      t78 = cdsqrt(t78)
      t157 = -t77 - t75 + t76 - t45 + t74 + t43 - t44 + t78
      t157 = 0.1D1 / t157
      t158 = t95 * t73
      t159 = -t158 * t157 - t154
      t160 = t151 * (t80 - t79)
      t154 = -t154 * t15 + t11
      t161 = t79 * t151
      t162 = t161 * t152
      t163 = -t162 * t30 + t28
      t164 = -t162 * t15 + t11
      t165 = t76 + t43 + t74
      t166 = t165 * t50
      t167 = t41 + t39 + t40
      t168 = (t87 + t20 + t21 + t22 + t44 + t45 + t75 + t77 + t85 + t86)
     # * t151
      t169 = t167 * t53
      t170 = t152 * t79 * (t169 * t79 + t168) - t166 * t79 - t33 - t34 -
     # t35
      t78 = -t77 - t75 + t76 - t45 + t74 + t43 - t44 - t78
      t78 = 0.1D1 / t78
      t158 = -t158 * t78 - t162
      t162 = -t162 * t6 + t4
      t171 = t87 + t20 + t21 + t22 + t44 + t45 + t75 + t77 + t85 + t86
      t172 = t42 * t50
      t173 = t95 * (-t172 * t80 + t43 + t74 + t76) - t171
      t172 = t95 * (-t172 * t79 + t43 + t74 + t76) - t171
      t166 = t152 * t80 * (t169 * t80 + t168) - t166 * t80 - t33 - t34 -
     # t35
      t168 = t8 * t30 + t28
      t169 = t11 * t30
      t174 = t169 * t16 - t28
      t165 = t165 * t95 - t171
      t171 = -t95 * t28 * t141 + (t45 + t44 + t21) * t5
      t14 = 0.1D1 / t14
      t175 = t101 + t57
      t176 = t31 * t5
      t177 = t4 * t14
      t178 = t177 * t176 * t70
      t179 = t33 * t5
      t180 = t96 * t50
      t181 = t95 * t4
      t182 = t101 + t57
      t183 = t39 * t182
      t105 = t183 + t105
      t184 = t127 * t57
      t185 = t184 * t102
      t186 = t185 * t19 * t101
      t185 = t185 * t101
      t187 = t185 * t42
      t188 = t57 * t101
      t189 = t188 * t59 * t63 * (t182 * t57 + t110)
      t190 = t188 * t65 * t61 * t175
      t191 = t131 * t19
      t192 = t181 * t30
      t193 = t28 * t6
      t194 = t193 * t47
      t195 = t30 * (t194 + t192)
      t196 = 0.1D1 / t30
      t197 = t14 * t56
      t198 = t193 * t196 + t181
      t199 = t196 * t28
      t200 = 16 * t63 * t59
      t201 = t62 * t63 * t58 * t59
      t202 = t196 ** 2
      t203 = 0.1D1 / t3
      t204 = t43 * t182
      t205 = t28 * t203
      t206 = t100 + t102
      t51 = t26 + t51
      t207 = t4 * t11
      t208 = t51 * t73
      t209 = t132 * t122
      t210 = 64 * t122 * t73 * t117 * t107 * t206 + 16 * t122 * t115 * t
     #165 * t107
      t116 = (-t73 * (384 * t116 * t119 * t107 * (t107 + t119) + 512 * t
     #116 * t121 * t109) - 128 * t165 * t117 * t119 * t107 * t206 + 64 *
     # t42 * t115 * t107 * t119) * t19
      t117 = t203 * t14
      t211 = t102 * t103 + t107
      t212 = t102 * t19
      t213 = t212 * t56 * t100
      t214 = t169 * t4
      t215 = t30 * t4
      t216 = t193 + t215
      t217 = t28 * t15
      t218 = t169 * t95
      t219 = t218 + t217
      t220 = t28 * t50
      t218 = t30 * (t217 * t47 + t218)
      t221 = t102 * t100
      t222 = t56 * t19
      t223 = 0.1D1 / t29
      t224 = t49 * t223
      t225 = t224 * t152
      t226 = -t225 + t28
      t227 = -t225 * t6 * t196 + t4
      t228 = -t225 * t15 * t196 + t11
      t225 = t225 * t196
      t229 = t33 * t95
      t230 = -t229 * t57 - t225
      t231 = t223 * t196
      t232 = t231 * (-t46 + t49)
      t233 = t224 * t196
      t234 = -t153 + t233
      t235 = -t161 + t233
      t236 = t231 * t46
      t161 = -t161 + t236
      t237 = -t153 + t236
      t238 = t46 * t223
      t239 = -t238 * t152 + t28
      t240 = t236 * t152
      t241 = -t240 * t6 + t4
      t242 = -t240 * t15 + t11
      t243 = -t229 * t101 - t240
      t244 = -t225 - t8
      t225 = -t225 + t17
      t245 = -t240 - t8
      t240 = -t240 + t17
      t246 = 0.1D1 / t13
      t12 = 0.1D1 / t12
      t247 = t197 * t12
      t248 = t7 ** 2
      t249 = t5 * (-t39 * t5 ** 2 * t248 + t33 + t8 * (t45 - t43 + t44))
      t250 = t5 * (t45 + t44)
      t251 = t45 * t196
      t252 = t251 + t31
      t253 = t233 * t1
      t254 = (0.9D1 / 0.4D1)
      t255 = -3 * t28
      t256 = t49 * t196
      t257 = t139 * t223 + t5
      t258 = t21 * t5
      t259 = t256 * t257 + t176
      t260 = (0.3D1 / 0.4D1)
      t261 = -(0.3D1 / 0.8D1)
      t262 = (0.1D1 / 0.4D1)
      t263 = t49 ** 2
      t264 = t21 * t262
      t265 = t260 * t28
      t266 = t223 * t202
      t267 = -t152 * t224 * (t196 * (t257 * t49 + t258) + t176 + t251 * 
     #(t253 + t5)) + t28 * t259 + t266 * t263 * t1 * (t223 * (t261 * t49
     # - t264) + t265)
      t268 = t236 * t1
      t269 = t46 * t196
      t270 = t46 ** 2
      t257 = -t152 * t238 * (t196 * (t257 * t46 + t258) + t176 + t251 * 
     #(t268 + t5)) + t28 * (t269 * t257 + t176) + t266 * t270 * t1 * (t2
     #23 * (t261 * t46 - t264) + t265)
      t59 = t123 * t70 * t114 * t59 * t63
      t123 = t80 ** 2
      t36 = t36 * t31
      t84 = t84 * t50
      t258 = t45 * t29
      t264 = t30 * t80 * t50
      t265 = t33 * t29
      t266 = t43 * t53
      t135 = -t136 - t135
      t271 = t45 + t44
      t139 = -t140 - t139
      t140 = -t35 - t34
      t272 = t140 * t30
      t273 = t39 * t53 ** 2
      t274 = t41 + t40
      t275 = t29 * (t77 + t21 + t75) + t31 * t274
      t276 = t30 * t275
      t277 = t45 * t274
      t278 = t21 * (t77 - t74 + t75 - t76)
      t279 = t123 * t53
      t280 = t87 + t20 + t75 + t77 + t86
      t281 = t87 + t86
      t282 = t169 + t217
      t283 = t193 + t215
      t284 = t1 * t282 + t283 * t38
      t285 = t54 * t80 * t123
      t286 = t87 + t20 + t22 + t85 + t86
      t287 = t21 * t274
      t288 = t39 * t286 + t287
      t289 = (0.1D1 / 0.8D1)
      t290 = (0.3D1 / 0.16D2)
      t123 = t152 * (t153 * (t33 * (-t87 - t22 - t77 - t85 - t86) + t35 
     #* (-t45 - t21) - t34 * (t45 + t21)) + t80 * (t39 * t271 * t54 * t1
     #23 + t151 * t31 * (t28 * (-t75 - t20) + t272) + (t28 * (t135 * t6 
     #+ t139 * t15) + t30 * (t11 * t139 + t135 * t4) + t21 * (t44 - t43)
     # + t45 * (-t76 + t21 + t44 - t74)) * t53 * t80)) - t260 * t266 * t
     #123 * (t29 * (t264 - t28) - t74 - t76) + t261 * t285 * t29 * t284 
     #+ t262 * (t279 * (t45 * t280 - t43 * t281 + t44 * (t87 + t77 + t86
     #)) + t279 * (t44 * (t85 + t20 + t22 + t75) + t278 + t150 + (t277 +
     # t276) * t50 * t80 + t45 * (t85 + t22 + t45) + t43 * (-t85 - t20 -
     # t22))) + t95 * t265 * t80 * t50 * (-t264 + t28) + t290 * t273 * t
     #123 ** 2 * t167 + t289 * t285 * t288 + t28 * (-t258 * t123 * t53 +
     # t84 * t80 + t36)
      t135 = t50 * t79
      t139 = t134 * t28
      t153 = t135 * t30
      t285 = t135 * t29
      t291 = t79 ** 2
      t292 = t53 * t291
      t293 = t28 * (-t142 - t143) + t30 * (-t137 - t144) - t145 - t146
      t294 = t28 * (t29 * t89 + t31 * (t76 - t21 - t45 + t74) - t83)
      t295 = -(0.3D1 / 0.2D1)
      t296 = 8 * t31
      t297 = t220 * t39
      t298 = t85 + t22
      t299 = t54 * t79 * t291
      t300 = -t87 - t20 - t22 - t75 - t77 - t85 - t86
      t301 = t45 * (-t76 + t21 - t74)
      t302 = t21 * t140
      t36 = t152 * (t292 * (t29 * (t28 * (-t77 - t75) + t272 + t135 * t3
     #1 * t58) + t31 * (-t217 * t1 + t45 * t30) - t136 * t283) + t79 * (
     #t151 * (t31 * (t28 * t300 + t272) + t302 + t45 * t140) + t45 * t39
     # * t291 * t54 + (-t43 * t21 + t301 + t44 * (-t76 + t21)) * t53 * t
     #79)) + t260 * t266 * t291 * (t29 * (-t153 + t28) + t74 + t76) + t2
     #61 * t299 * t29 * t284 + t262 * (t292 * (t44 * t280 + t150 + t43 *
     # (-t87 - t20 - t86) + t85 * t45) + t292 * (t153 * t275 - t43 * t29
     #8 + t44 * t298 + t24 + t278 + t45 * (t135 * t274 + t20 + t22 + t75
     # + t77 + t86 + t87))) + t289 * t299 * t288 + t290 * t273 * t291 **
     # 2 * t167 - t95 * t135 * t265 * (t153 - t28) + t28 * (-t292 * t258
     # + t84 * t79 + t36)
      t54 = t196 * (t76 - t21 + t74) - t31
      t84 = t256 + t31
      t140 = t82 * t28
      t151 = t224 * t31
      t258 = t231 * t263
      t266 = t251 + t31
      t272 = t33 * t298
      t273 = t31 * t223
      t275 = t273 - t199
      t291 = t24 * t231
      t298 = t20 * t275
      t299 = t231 * t21
      t303 = t231 * t45
      t304 = t223 * t266 - t199
      t281 = t223 * t263 * (t223 * (t151 * t202 * t274 + t75 * t45 * t20
     #2 + t82) + t85 * t196 * t304 + t22 * t196 * t304) + t258 * (t75 * 
     #t223 * t84 + t77 * t223 * t84 - t199 * t281 + t273 * t281 + t291 +
     # t298 + t299 * (t77 + t49 - t74 + t75 - t76) + t303 * (t233 * t274
     # + t20 + t77 + t86 + t87))
      t287 = t287 * t231 + t20 + t22 + t85 + t86 + t87
      t304 = t223 ** 2
      t305 = t304 * t202
      t306 = t305 * t49 * t263
      t307 = t199 * t274 + t74 + t76
      t308 = t231 * t274 + 1
      t136 = -t152 * (t233 * (t21 * (t49 * (t223 * (-t251 - t31) + t199)
     # + t35) + t272 + t224 * (t41 * t33 * t196 + t74 * t266)) + t224 * 
     #(t11 * t259 + t251 * (t224 * (t76 * t196 - t31) + t34 + t35 - t258
     #) + t34 * (t196 * (t21 + t49) + t31) + t199 * (t280 * t31 + t256 *
     # (t6 * (t136 * t223 + t32) + t77)) - t258 * t31)) + t260 * t202 * 
     #t263 * t28 * (t223 * (t76 - t49 + t74) + t28) + t261 * t306 * t307
     # + t262 * t281 + t289 * t306 * t287 + t290 * t305 * t263 ** 2 * t3
     #08 - t95 * t256 * t33 * (t224 - t28) + t28 * (-t233 * t45 * t84 + 
     #t151 * t54 + t34 * t84 + t35 * t84 + t140)
      t259 = t273 - t199
      t263 = t259 * t46
      t280 = t263 - t33
      t281 = t77 + t75
      t306 = -t45 - t21
      t309 = t231 * t270
      t310 = t269 + t31
      t38 = t38 * t223
      t311 = t1 * t223
      t312 = t277 * t231
      t313 = t199 * t134
      t40 = t273 * (t199 * (-t40 - t41) - t76 - t74) - t199 * t281 - t34
     # - t35
      t41 = t196 * (-t76 + t21 - t74) + t31
      t89 = t43 * t89
      t314 = t33 * (t76 + t74 - t44)
      t315 = t34 * t310
      t316 = t305 * t46 * t270
      t317 = -t269 - t31
      t54 = t152 * (t236 * (t236 * (-t33 * t274 + t301) + t302 - t272) +
     # t238 * (t269 * t21 * t275 - t315 + t251 * (t238 * t310 - t34 - t3
     #5) + t35 * t317 + t199 * (-t31 * (t87 + t20 + t86) + t75 * t317 + 
     #t77 * t317) + t236 * t31 * (-t76 + t46 - t74))) + t260 * t202 * t2
     #70 * t28 * (t223 * (t76 - t46 + t74) + t28) + t261 * t316 * t307 +
     # t262 * (t304 * t270 * (t202 * (t45 * (t236 * t274 + t22 + t85 + t
     #87) + t278) + t82) + t309 * (t223 * (t31 * (t77 + t75) + t269 * (t
     #273 * t274 + t21 + t75 + t77)) + t291 + t298 + t87 * t275 + t86 * 
     #t275 + t85 * t275 + t22 * t275 + t303 * (t86 + t20 + t75 + t77))) 
     #+ t289 * t316 * t287 + t290 * t305 * t270 ** 2 * t308 - t95 * t269
     # * t33 * (t238 - t28) + t28 * (-t236 * t45 * t310 + t238 * t31 * t
     #54 + t35 * t310 + t140 + t315)
      t140 = -t77 + t74 - t75 + t76
      t152 = t50 * t80
      t236 = t152 * t29
      t243 = 0.1D1 / t243
      t235 = 0.1D1 / t235
      t230 = 0.1D1 / t230
      t234 = 0.1D1 / t234
      t159 = 0.1D1 / t159
      t232 = 0.1D1 / t232
      t237 = 0.1D1 / t237
      t158 = 0.1D1 / t158
      t160 = 0.1D1 / t160
      t161 = 0.1D1 / t161
      t261 = t155 ** 2
      t262 = t239 ** 2
      t270 = t163 ** 2
      t272 = t161 ** 2
      t274 = t235 ** 2
      t275 = t237 ** 2
      t278 = t226 ** 2
      t289 = t234 ** 2
      t290 = t234 * t289
      t291 = t232 ** 2
      t298 = t160 ** 2
      t301 = t160 * t298
      t302 = t159 * t275 * t157
      t308 = t302 * t121
      t315 = t164 * t158
      t316 = t315 * t270
      t317 = t316 * t162 * t36
      t318 = t317 * t109 * t274 * t78 * t272
      t319 = t230 * t289
      t320 = t319 * t67
      t321 = t241 * t243
      t322 = t321 * t272
      t323 = t322 * t111 * t275 * t54 * t242
      t324 = t48 * t53 * t291
      t325 = t197 * t19
      t326 = t325 * t246 * t203
      t327 = t326 * t2 * t147
      t328 = t230 ** 2
      t329 = t159 ** 2
      t330 = t154 * t6
      t331 = t15 * t156
      t332 = t154 * t156
      t333 = t332 * t123
      t334 = t164 * t162
      t335 = t78 * t272
      t80 = (-t333 * t275 * t157 * t121 * t329 + t308 * (t123 * (t331 + 
     #t330) + t332 * (t254 * t279 * t29 * t284 - t260 * t279 * t288 + t2
     #95 * t279 * (t152 * t39 * t167 + t276 + t277) - t47 * t43 * (-t152
     # * t45 + t33) - t95 * (t293 * t50 * t80 + t294) + t152 * (t21 * t1
     #40 + t45 * t300 + t44 * (-t87 - t22 - t85 - t86) + t43 * (t87 + t2
     #2 + t85 + t86)) + (-t150 + t20 * t43 + t44 * (-t77 - t75 - t20) - 
     #t24) * t50 * t80 + t92 + t93 + t94 - 3 * t236 * (t264 * t271 + t13
     #9) + t297 * t80 * ((0.9D1 / 0.2D1) * t236 + t296)))) * t289 * t261
      t92 = t228 * t227
      t152 = t92 * t136
      t236 = t241 * t242
      t83 = t272 * t111 * t275 * t243 * t262 * (t54 * (t15 * t241 + t242
     # * (-t321 + t6)) + t236 * (t254 * t309 * t307 - t260 * t309 * t287
     # + t295 * t309 * (t15 * (t311 * t310 + t5) + t6 * (t38 * t310 + t3
     #2) + t21 + t46 + t312) + t47 * t28 * (t251 * t46 - t265) - t95 * (
     #t21 * t280 + t40 * t46 + t314 + t89 + t45 * (t238 * t41 - t33)) - 
     #t31 * (-t28 * t281 - t90) - t87 * t263 - t263 * t85 - t263 * t22 -
     # t20 * t280 - t86 * t280 - t238 * (t24 * t196 + t281 * t31) - t238
     # * (t196 * (t148 + t149) + t83) - t35 * t306 - t33 * (-t87 - t22 -
     # t85) - t34 * t306 - 3 * t46 * (t238 * t266 + t313) + t28 * t46 * 
     #((0.9D1 / 0.2D1) * t269 + t296)))
      t263 = t305 * t298 * t73
      t74 = t263 * t42 * (t80 + t335 * t158 * t274 * t109 * t270 * (t36 
     #* (t162 * (-t315 + t15) + t164 * t6) + t334 * (t254 * t292 * t29 *
     # t284 - t260 * t292 * t288 + t47 * t43 * (t135 * t45 - t33) - t95 
     #* (t135 * t293 + t294) + t295 * t292 * (t135 * t39 * t167 + t276 +
     # t277) - t135 * (t45 * (t87 + t20 + t22 + t75 + t77 + t85 + t86) +
     # t21 * (t75 - t74) + t43 * (-t87 - t20 - t85 - t86) + t44 * (t87 +
     # t20 + t75 + t77 + t85 + t86)) - t31 * (-t91 - t90) + t93 + t94 - 
     #(-t22 * t138 + t150 + t24 + t21 * (t77 - t76)) * t50 * t79 - 3 * t
     #285 * (t153 * t271 + t139) + t297 * t79 * ((0.9D1 / 0.2D1) * t285 
     #+ t296))))
      t32 = t327 * (t74 + t324 * (t83 + (-t152 * t289 * t67 * t328 + t32
     #0 * (t136 * (t15 * t227 + t228 * t6) + t92 * (t254 * t258 * t307 -
     # t260 * t258 * t287 + t295 * t258 * (t15 * (t311 * t84 + t5) + t6 
     #* (t38 * t84 + t32) + t21 + t312 + t49) + t47 * t28 * (t251 * t49 
     #- t265) - t95 * (t40 * t49 + t314 + t89 + t21 * (t259 * t49 - t33)
     # + t45 * (t224 * t41 - t33)) + t49 * (t199 * t286 + t273 * t300 + 
     #t303 * (-t87 - t22 - t85) + t299 * t140) + t31 * (t30 * (-t151 + t
     #34 + t35) + t91) + t94 + t45 * (t233 * (-t77 - t75 - t86 - t20) + 
     #t34 + t35) - t24 * t233 - 3 * t49 * (t224 * t266 + t313) + t28 * t
     #49 * ((0.9D1 / 0.2D1) * t256 + t296)))) * t274 * t278))
      t23 = 0.1D1 / t23
      t34 = 0.1D1 / t244
      t35 = 0.1D1 / t245
      t38 = t162 * t246
      t40 = t38 * t3 + t163
      t41 = t156 * t246
      t74 = t41 * t3
      t75 = t74 + t155
      t79 = t243 ** 2
      t80 = t35 ** 2
      t83 = t158 ** 2
      t84 = t78 ** 2
      t89 = t73 ** 2
      t90 = t157 ** 2
      t91 = t34 ** 2
      t93 = t166 * t159
      t94 = t315 * t170
      t135 = t94 * t109 * t78
      t138 = t315 * t163
      t139 = t138 * t170
      t140 = t139 * t109 * t78
      t151 = t93 * t155
      t153 = t151 * t154 * t157 * t121
      t167 = t56 * t30
      t233 = t167 * t203
      t251 = t23 * t10
      t258 = t93 * t154 * t157
      t259 = t251 * t12
      t260 = t12 * t42
      t264 = t239 * t35
      t266 = t227 * t230
      t271 = t266 * t278
      t272 = t271 * t267
      t273 = t48 * t50
      t276 = t231 * t2
      t277 = t48 * t246 * t232
      t279 = t14 * t19
      t280 = t279 * t147
      t40 = t280 * (t73 * (t260 * (t251 * (-t135 * t40 + t93 * (-t74 - t
     #155) * t121 * t157 * t154) + t233 * (t153 + t140)) * t298 + t259 *
     # (t94 * t40 * t78 * t107 - t258 * t119 * t75) * t301) + t276 * t11
     #4 * t56 * t203 * t246 * (-t334 * t270 * t109 * t235 * t83 * t84 * 
     #t161 + t332 * t261 * t234 * t329 * t237 * t90 * t121) * t160 * t89
     # + t277 * t203 * t56 * t2 * (t273 * (-t236 * t262 * t112 * t79 * t
     #237 * t161 + t92 * t278 * t68 * t328 * t234 * t235) + (t266 * t30 
     #* t226 * t267 * t91 * t67 + t321 * t239 * t257 * t80 * t111 * (-t2
     #64 + t30) - t272 * t34 * t91 * t67) * t248 * t232))
      t74 = 0.1D1 / t11
      t92 = 0.1D1 / t18
      t94 = -0.1D1 / t245
      t236 = -0.1D1 / t244
      t244 = 0.1D1 / t5
      t215 = -t193 - t215
      t245 = t74 ** 2
      t281 = t244 ** 2
      t284 = t244 * t281
      t285 = t92 ** 2
      t286 = t16 ** 2
      t287 = t9 ** 2
      t288 = t4 ** 2
      t18 = 0.1D1 / t18 ** 2
      t292 = t168 ** 2
      t293 = t25 * t28
      t294 = t3 * t10
      t295 = t28 * t81
      t296 = t14 * t74
      t297 = t3 * t246
      t299 = t4 * t25
      t300 = t174 * t27
      t77 = t77 * t14 * t245
      t303 = t81 * t246
      t306 = t10 * t14
      t307 = t246 * t12
      t309 = t81 * t292
      t310 = t10 * (t8 * t15 + t11)
      t312 = t25 * t246
      t313 = t14 * t281
      t314 = t313 * t12
      t336 = t281 * t1
      t337 = t12 * t14
      t294 = t222 * (t11 * (t336 * (t310 * t307 + t307 * (t294 * t14 - t
     #25) * t9 + t12 * (t306 + t303) * t168 - t309 * t276 * t203 * t246 
     #* t94 * t236) * t16 * t92 + t314 * t1 * (t3 * (t303 * t168 * t9 - 
     #t312 * t287) + t309) * t286 * t285) + t12 * (t244 * ((-t295 * t246
     # + t177 * (-t294 * t246 + t293 * t74) + t297 * t296 * t288 * t25) 
     #* t244 * t1 + t296 * (-t299 * t217 * t74 + (-t288 * t15 * t25 * t7
     #4 + t215 * t81) * t246 * t3)) + t77 * (-t81 * t174 ** 2 + t25 * (t
     #297 * t27 ** 2 + t300)) * t18 * t248))
      t338 = -0.1D1 / t225
      t339 = -0.1D1 / t240
      t340 = t297 * t4
      t341 = t340 + t28
      t342 = t4 * t246
      t343 = t9 * t11
      t344 = t343 * t168
      t345 = t6 * t25
      t346 = t30 * t81
      t347 = t297 * t6
      t348 = t96 * t244
      t349 = t25 * t283
      t350 = t81 * t14
      t351 = t276 * t246
      t352 = t222 * t2
      t18 = t352 * (t12 * (t244 * (t246 * (-t346 + t345) + t25 * (-t344 
     #* t285 * t286 * t14 + t342) * t244 * t1) + t296 * t244 * (-t295 * 
     #t341 * t244 * t1 + t349) + t244 * (t14 * (-t347 - t30) - t15 * t24
     #6 + (-t11 * t246 - t14 * t28) * t244 * t1) * t10 + t350 * t15 * (-
     #t300 * t297 * t5 * t248 * t18 + t340 * t28 * t244 + t348) * t245) 
     #+ t351 * (t300 * t77 * t338 * t248 * t18 * t339 * (-t174 * t81 + t
     #25 * t27) + t76 * (t168 * (t313 * t287 * t25 * t286 * t94 * t236 *
     # t285 + (t203 * (t25 * t9 - t310) - t306 * t9) * t281 * t236 * t94
     # * t16 * t92) - t309 * t313 * t9 * t285 * t286 * t94 * t236)))
      t18 = t18 + t2 * (t2 * (-t337 * (t203 * (-16 * t220 * t56 * t219 *
     # t212 * t118 * t100 * t206 + t222 * (32 * t180 * t11 * t221 * t115
     # * t211 + t131 * t47 * t218 * t102)) + (32 * t207 * t28 * t213 * t
     #115 * t211 * t50 - 16 * t50 * (t28 * t51 + t214) * t213 * t118 * t
     #206 + 8 * t213 * t114 * t50 * (t15 * t216 + t26 * t30)) * t246) + 
     #t246 * ((-16 * t205 * t14 * t198 * t190 * t56 * t19 + t117 * (t188
     # * t47 * t62 * t30 * t195 * t56 * t19 + 32 * t96 * t4 * t189 * t19
     # * t56 * t196)) * t244 + t117 * (t56 * (16 * t186 * t131 * (t11 * 
     #t195 + t95 * t28 * (t193 + t192) * t15) + 128 * t180 * t4 * t11 * 
     #t191 * (t131 * (t185 * t114 * t100 + t187 * (t183 + t130)) + t130 
     #* (t130 * (t187 + t190) + t189))) - (64 * t181 * t169 * t28 * t50 
     #* t56 + 64 * t180 * t56 * t52) * t186 * t100 * t114 * t105) * t196
     #) * t223 * t2) + t294)
      t27 = 0.1D1 / t225
      t52 = 0.1D1 / t240
      t77 = -t100 - t102
      t115 = t11 ** 2
      t174 = t25 * t156
      t180 = t25 * t162
      t186 = t154 * t159 * t157 * t119
      t187 = t28 * t103
      t189 = t3 * t81
      t190 = t246 * t56
      t192 = t226 * t230
      t195 = t192 * t34 * t69
      t211 = t104 * t56
      t212 = t19 * t2
      t213 = t10 * t228 - t227 * t25
      t220 = -t10 * t242 + t241 * t25
      t222 = t154 ** 2 * t159 * t157 * t119
      t223 = t39 * t246
      t214 = t352 * (t223 * (t214 * t188 * t131 * t25 * t203 * t102 + t3
     #3 * (t350 * t271 * t27 * t34 * t16 * t69 + t264 * t113 * t243 * (t
     #203 * t220 - t306 * t241) + t195 * (t203 * t213 + t306 * t227)) * 
     #t7 * t232) * t2 + t260 * (t349 * t221 * t14 + (-t164 ** 2 * t107 *
     # t158 * t78 + t222) * t160 * t246 * t73 * t10))
      t26 = t214 + t212 * (t277 * t56 * t2 * (t264 * t25 * t241 ** 2 * t
     #243 * t113 * t52 * t16 * t14 - t81 * (t241 * t52 * t16 * t14 + t20
     #3) * t113 * t35 * t243 * t262 + t195 * (-t25 * t227 ** 2 * t27 * t
     #16 * t14 + t226 * t81 * t203)) * t7 + t260 * (t251 * t221 * t14 * 
     #(t25 * (t13 * t28 + t3 * t4) + (t340 * t77 - t187) * t42 * t11 * t
     #2 + t189 * t341) + t190 * (t350 * t221 * t3 * t283 + (t186 * (t155
     # * t81 - t174) + t315 * (-t163 * t81 + t180) * t78 * t107) * t160 
     #* t73)) + t211 * t2 * (-t313 * t1 * t10 * t96 + t131 * (-t10 * t11
     #5 * t30 + t217 * (t299 - t295) + t293 * t26) * t102 * t246) * t57 
     #* t203)
      t27 = t155 * t203 + t41
      t41 = t272 * t91
      t52 = t321 * t262
      t195 = t52 * t257 * t80
      t214 = t207 * t126
      t225 = t325 * t147
      t140 = t225 * (t260 * (t316 * t170 * t109 * t203 * t78 + t140 * t3
     #8 - t153 * t27) * t301 * t73 + t223 * t203 * t2 * (t214 * t29 * t6
     #1 * t37 * t66 * t110 * t107 + t33 * (t195 * t111 - t41 * t67) * t2
     #48 * t232 * t291))
      t153 = t11 * t203
      t223 = t153 + t177
      t240 = t177 * t74
      t264 = t240 + t203
      t272 = t9 * t92
      t277 = t272 - t6
      t287 = t1 ** 2
      t288 = t288 * t25
      t293 = t6 * t96
      t294 = t168 * t92
      t296 = t223 * t10
      t300 = t299 * t264
      t309 = t303 * t104 * t57
      t169 = -t169 - t217
      t310 = t236 ** 2
      t313 = t94 ** 2
      t315 = t131 * t102
      t321 = t315 * t12
      t338 = t305 * t76
      t8 = t352 * t246 * (t2 * (-t348 * t177 * t104 * t15 * t81 * t245 *
     # t57 + t314 * t76 * t16 * t92 * (t168 * t277 - t30 * t9) + t344 * 
     #t337 * t287 * t284 * t92 * t16) + t321 * (t169 * t81 + t25 * t51) 
     #+ t338 * t117 * t310 * t313 * t16 * t92 * t292 * (t249 * t277 + t9
     # * (t5 * (t98 * t28 + 5 * t39 * t8 - t21) - 3 * t250 - t229 * t1))
     # * t284 * t147)
      t8 = t8 + t147 * (t19 * (t244 * (t17 * t259 * t14 * t92 * (t297 * 
     #t277 + t294 - t30) + t190 * t104 * (t14 * (t10 * t283 + t74 * (-t2
     #88 * t30 + t293 * t81) + t288 * t217 * t245) + t203 * (t10 * t282 
     #+ t215 * t25)) * t57) + t336 * t56 * (t309 * t264 * t96 + t17 * t1
     #17 * t292 * t92 * (-t231 * t10 * t236 * t94 + t12 * t92) + (-t117 
     #* t12 + t188 * (-t300 + t296) * t246 * t29) * t30 * t28) + t17 * t
     #247 * t287 * t292 * t284 * t203 * t92) + t307 * t30 * (64 * t122 *
     # t4 * t11 * t73 * t118 * t56 * t107 * t206 - 16 * t209 * t56 * (-t
     #207 * t165 + t208)) * t14)
      t17 = t295 * t264
      t23 = -t272 * t338 * t326 * t147 * t249 * t313 * t310 * t284 * (t9
     #4 + t236) * t16 * t292 + (t190 * t128 * t2 * t28 * t57 * t244 * t2
     #23 + t321 * (t282 * t14 * t23 * t2 + t190 * t11 * t15)) * t19 * t1
     #0 + t147 * t304 * t246 * t203 * (t199 * (-1024 * t59 * t207 * t37 
     #* t105 + 256 * t59 * (t207 * t99 + t37 * t51)) * t14 - t1 * (-64 *
     # t178 * t64 * t97 * t61 * t110 * t19 * t175 + 16 * t71 * t63 * t96
     # * t14 * t110 * t19 * (t171 * t4 + t179 * t6) + 32 * t178 * t63 * 
     #t96 * t61 * t110 * t19) * t284) + t294 * t325 * t76 * t2 * t203 * 
     #t281 * (t2 * t9 * t249 * t196 * t246 * t304 * t313 * t310 * t244 +
     # t12 * t30) * t16
      t17 = t2 * t23 + t212 * t246 * (t29 * (-t17 * t188 * t2 * t56 * t2
     #44 * t58 + t188 * (-t177 * t287 * t147 * t96 * t56 * t203 * t284 +
     # t240 * t193 * t2 * t25 * t56 * t244) * t30) + t127 * (t70 * t28 *
     # t2 * (t17 + t296 - t300) * t244 * t101 + t56 * t28 * t2 * (t17 - 
     #t300) * t244 * t57 * t110) + t321 * (t251 * t2 * t3 * t14 * t51 + 
     #t11 * (t77 * t11 * t10 - t295 * t103 + t299 * t103) * t56 * t42))
      t13 = t13 * t25
      t23 = t13 * t337
      t51 = t309 * t2 * t11 - t23
      t59 = t315 * t115 * t30 * t246
      t63 = t162 * t158
      t71 = t63 * t78
      t76 = t156 * t157
      t98 = t315 * t197
      t117 = t299 * t30
      t122 = t10 * t15
      t178 = t259 * t14
      t193 = t178 * t160
      t117 = t212 * (t28 * (t315 * t167 * (t337 * t81 + (t188 * (-t122 -
     # t346) * t246 * t203 * t29 + (-t117 * t66 * t101 - t117 * t57 * t1
     #10) * t246 * t203 * t62) * t11 * t2) + t221 * t4 * (-t153 * t312 *
     # t104 * t2 * t57 * t103 + t337 * (t297 * t77 * t81 - t103 * t25)) 
     #* t56 * t114) + t193 * t73 * ((t163 * (t13 + t189) + t180 * t3) * 
     #t78 * t158 * t100 + t157 * t102 * t159 * (t155 * (-t13 - t189) - t
     #174 * t3)) - t247 * t221 * t81 * t103 * t114 * t96)
      t23 = t117 + t212 * (t98 * t104 * t57 * t81 * t2 * t105 * t203 * t
     #97 + t303 * t259 * t3 ** 2 * t73 * t14 * t160 * (-t76 * t102 * t15
     #9 + t71 * t100) + t167 * t205 * (t23 * t315 + (t62 * (t59 * t66 * 
     #t101 + t59 * t57 * t110) + t188 * (t221 * t115 * t114 * t246 * t10
     #3 - t30 * t244 * t14) * t29) * t10 * t2) + (t114 * (t51 * t119 * t
     #100 + t51 * t102 * t107) + t303 * t185 * t131 * t2 * t11 * t182 + 
     #t306 * t184 * t2 * t101 * t244 * t182) * t203 * t56 * t96)
      t29 = t127 * t33
      t41 = t225 * ((t258 * t261 * t203 * t120 + t139 * t108 * t78 * (t1
     #63 * t203 + t38) + t151 * t332 * t246 * t157 * t120) * t298 * t12 
     #* t73 * t114 + t29 * t2 * t248 * t246 * t203 * t291 * (t195 * t112
     # + t41 * t68))
      t51 = t297 * t81 + t25
      t59 = t138 * t78 * t107
      t13 = t13 * t203
      t103 = t155 * t159
      t105 = t163 * t158
      t75 = t75 * t119
      t117 = t56 * t42
      t139 = t81 * t270 * t107 * t235 * t158 * t78 * t161
      t151 = t273 * t232
      t153 = t19 * t147
      t98 = t153 * (t42 * (t297 * t259 * t177 * t11 * t42 * t165 * t119 
     #* t107 + t231 * t203 * t56 * (t350 * t155 * t261 * t234 * t159 * t
     #237 * t157 * t119 - t139 * (t14 * t163 + t164 * t246) - t103 * t31
     #2 * t332 * t234 * t237 * t157 * t119) * t160 * t73 - t193 * t89 * 
     #t163 * t164 * t84 * t107 * t83) + t98 * t309 * t293 + t151 * t203 
     #* t56 * (t242 * t246 * t220 * t161 * t113 * t237 * t243 * t239 - t
     #350 * t239 * t262 * t243 * t237 * t113 * t161 - t303 * t262 * t242
     # * t243 * t237 * t113 * t161 + t350 * t226 * t278 * t235 * t230 * 
     #t234 * t69))
      t13 = t98 + t212 * (t2 * (t48 * (t192 * t228 * t235 * t234 * t246 
     #* t50 * t213 * t69 + (t303 * t228 * t235 * t234 * t50 - t34 * t306
     # * t7) * t69 * t230 * t278 + t306 * t262 * t243 * t35 * t113 * t7)
     # * t232 * t203 * t56 + t178 * t126 * t107 * (t28 * t11 * t165 + t1
     #69 * t73 - t208 * t297)) + t193 * t42 * t2 * (-t297 * t334 * t84 *
     # t107 * t83 + t75 * t90 * t329 * t154) * t89 + t117 * (t351 * t203
     # * (t222 * t10 * t155 * t234 * t237 + t186 * t81 * t261 * t234 * t
     #237 + t59 * t235 * t161 * (-t10 * t164 + t180)) + t337 * (-t103 * 
     #t76 * t51 * t119 + (-t13 - t81) * t119 * t157 * t159 * t261 + t105
     # * t78 * t107 * (t162 * t51 + t163 * (t13 + t81)))) * t160 * t73)
      t48 = t158 * t78
      t51 = t15 * t170 + t164 * t172
      t98 = t332 * t173
      t71 = t71 * t107
      t158 = t15 * t166
      t165 = t103 * t157
      t7 = t280 * (t259 * (-t59 * t172 + t297 * (-t98 * t159 * t157 * t1
     #19 - t71 * t51) - t165 * t119 * (t154 * t173 + t158)) * t298 * t73
     # + t190 * t43 * t58 * (t315 * t188 * t4 * t81 + t43 * (t262 * t241
     # * t79 * t35 * t112 - t278 * t227 * t328 * t34 * t68) * t7 * t232 
     #* t203 * t82 * t2)) + t280 * (t259 * (t166 * (t75 * t154 * t157 * 
     #t329 + (-t154 * t30 + t297 * (-t331 - t330)) * t119 * t157 * t159)
     # + t48 * t107 * t170 * (-t15 * t163 + t164 * (t105 - t30) + t297 *
     # t164 * (t63 - t6))) * t298 * t73 + t315 * t211 * t303 * t57 * t96
     # * t4 * (t42 * t77 - t183) + t56 * t114 * (t334 * t163 * t109 * t2
     #46 * t83 * t84 + t270 * t164 * t109 * t203 * t83 * t84 - t155 * t1
     #54 * t329 * t90 * t121 * t27) * t160 * t12 * t89)
      t34 = -t52 * t237 * t113 * t161 + t271 * t235 * t234 * t69
      t35 = t76 * t261 * t119 * t234 * t237 * t159
      t52 = t231 * t73 * t42
      t59 = t225 * t203
      t63 = t59 * (t342 * t128 * t287 * t31 * t2 * t97 * t284 * t66 + t5
     #2 * t25 * t160 * (-t71 * t270 * t235 * t161 + t35) + t151 * t25 * 
     #t34)
      t71 = t190 * t205
      t34 = t280 * t246 * (t42 * (t214 * t259 * t3 * t73 * t109 + (t35 *
     # t231 * t81 * t160 + t12 * (t165 * t98 * t121 + t135 * (t162 * t30
     # + t163 * t6)) * t298) * t56 * t73) + t265 * (t167 * t81 * t50 * t
     #34 * t232 + t233 * t67 * t248 * t91 * t230 * t278 * t2 * (t227 * (
     #t95 * (t1 * (t224 * t252 - t33) + t5 * (-t43 + t49)) + t21 * (t253
     # + t5) + t250 + t256 * t1 * (t224 * t254 + t255)) + (-t266 + t6) *
     # t267) * t291))
      t5 = t34 + t280 * (t73 * (-t139 * t117 * t38 * t231 * t160 + t260 
     #* t56 * (t157 * t121 * t159 * (t154 * (t166 * (t155 * t6 + t156 * 
     #(-t103 + t30)) * t246 + t261 * (-t93 + t173) * t203) + t158 * t155
     # * t27) + (-t164 * t170 * t203 * t78 * t83 + t48 * t203 * t51) * t
     #109 * t270 + (-t334 * t170 * t246 * t78 * t83 + t48 * t38 * t51) *
     # t109 * t163) * t298) + t39 * (-t71 * t241 * t257 * t291 * t80 * t
     #111 * t248 * t79 + t71 * (t241 * (-t95 * (t1 * (-t238 * t252 + t33
     #) + t5 * (t43 - t46)) + t21 * (t268 + t5) + t250 + t269 * t1 * (t2
     #38 * t254 + t255)) + t6 * t257) * t248 * t111 * t80 * t291 * t243)
     # * t262 * t31 * t2 + t259 * t119 * t107 * t73 * t118 * t11 * (t100
     # * t28 + t102 * t341))
      t27 = t30 * (t204 - 1) + t187 * t42
      t12 = t168 * t12
      t4 = t96 * (t127 * (t70 * t47 * t4 * t11 * (t37 * t132 * (t47 * (3
     #84 * t129 * (t128 + t126) + 512 * t65 * t61 * t118 * t113 * t121) 
     #+ t131 * (2048 * t129 * (t104 + t130) + 1536 * t128 * t118 * t119 
     #* t100)) - 512 * t99 * t133 * t106 + 64 * t133 * (6 * t43 * t134 -
     # t47 * (t28 * (t142 + t143) + t30 * (t137 + t144) + t145 + t146) +
     # t95 * (t44 * t88 + t148 + t149 + t150 + t24 + t43 * (-t87 - t22 -
     # t85 - t86 - t20)) - t43 * (16 * t44 + 8 * t45))) * t19 * t53 + 64
     # * t125 * t72 * (96 * t58 * t62 * t4 * t11 * t53 * t66 + 8 * t6 * 
     #t15 * t53 - 32 * t39 * t55 * t57)) + t95 * (-16 * t65 * t61 * t4 *
     # t11 * t53 * t56 * t69 + t47 * t55 * t72) * (-256 * t125 * t58 * t
     #62 * t106 + 64 * t124 * t62 * t58 * t99)) * t202 * t14
      t14 = t337 * (t203 * (t56 * (t11 * t96 * t116 * t53 - 16 * t209 * 
     #t73 * t218) + t95 * t28 * t53 * t56 * t219 * t210) + t28 * (t56 * 
     #(-32 * t209 * t6 * t15 * t73 + t207 * t116 * t53) + t95 * t55 * t5
     #6 * t210) * t246)
      t15 = t2 * t30
      t9 = t336 * t279 * t2 * ((t1 * (t276 * t56 * t203 * t94 * t236 * t
     #244 * t292 - t12 * t56 * t244) - t259 * t3) * t246 * t9 - t12 * (t
     #1 * t168 * t56 * t203 * t244 + t251)) * t286 * t285 * t115
      t1 = t2 * (t2 * (t2 * (-t311 * (8 * t205 * t184 * t177 * t56 * t10
     #1 * t19 * (-t204 + 1) + t197 * t188 * t47 * t62 * t96 * t30 * t6 *
     # t203 * t19) * t246 * t281 + t246 * t203 * (-(t28 * (t177 * (-128 
     #* t171 * t64 * t60 * t66 * t110 * t175 + t179 * (384 * t201 * t66 
     #* t110 * (t110 + t66) + 512 * t201 * t69 * t113) + t200 * (t1 * (-
     #t21 * t95 + 6 * t43) - t47 * (t45 * t1 + t141 * t30)) * t110 * t66
     #) * t19 * t202 * t56 * t28 + t200 * t176 * t197 * (t194 * t196 + t
     #181) * t110 * t19 * t66) + t95 * t199 * t197 * t198 * (-64 * t179 
     #* t64 * t60 * t66 * t110 * t19 * t175 + t200 * t171 * t66 * t110 *
     # t19)) * t281 + t4) * t304) - t14 + t336 * t247 * t246 * t19 * t21
     #6) + t247 * t244 * (t30 * (-t15 * t203 + t295 * t74) + (-t299 * t3
     # * t74 - t15) * t246 * t6) * t19 + t9 + t279 * t259 * t244 * t2 * 
     #(t341 * t244 * t1 - t30 - t347))
      t3 = t327 * (t42 * t73 * t304 * t196 * t298 * (t138 * t335 * t162 
     #* t36 * t109 * t274 + t333 * t308 * t155 * t289) + t265 * (t152 * 
     #t320 * t226 * t274 + t323 * t239) * t53 * t291 * t58)
      t4 = t327 * (t263 * t114 * (t333 * t302 * t261 * t120 * t289 + t33
     #5 * t317 * t108 * t274) + t29 * (t322 * t262 * t242 * t54 * t275 *
     # t112 + t152 * t319 * t278 * t274 * t68) * t53 * t291)
      t6 = t197 * t191 * t104 * t147 * t96 * t203 * t57 * t102 * (t345 +
     # t346)
      t9 = t59 * t10 * (t52 * t160 * (t316 * t78 * t107 * t235 * t161 - 
     #t186 * t261 * t234 * t237) + t151 * (t262 * t242 * t243 * t237 * t
     #113 * t161 - t278 * t228 * t235 * t230 * t234 * t69))
      ret = 192 * t6 + 768 * t63 + 2048 * t40 + 6144 * t41 - 65536 * t32
     #7 * (t305 * (t298 * ((-t159 * t237 * t275 * t157 * t121 * t289 - t
     #308 * t290) * t123 * t154 * t156 * t261 - t318 * (t235 + t161)) + 
     #t301 * (t308 * t261 * t156 * t154 * t123 * t289 - t318)) * t73 * t
     #42 + t324 * ((t320 * t235 * t274 + (t232 * t67 * t289 + t290 * t67
     #) * t230 * t274) * t136 * t228 * t227 * t278 + t323 * t262 * (-t23
     #2 + t237 + t161))) - 32768 * t3 - 16384 * t32 - 4096 * t140 - 1280
     # * t9 + 1024 * t5 - 640 * t205 * t197 * t191 * t104 * t57 * t102 *
     # t11 * t10 * t147 * t27 + 512 * t7 - 384 * t211 * t205 * t191 * t1
     #77 * t57 * t102 * t25 * t147 * t27 - 98304 * t4 + 256 * t13 - 128 
     #* t23 - 64 * t26 - 32 * t17 - 24 * t153 * t247 * t287 * t28 * t284
     # * (t205 + t342) + t59 * (48 * t343 * t305 * t2 * t287 * t292 * t2
     #49 * t281 ** 2 * t246 * t92 * t16 * t310 * t313 + 320 * t122 * t31
     #5 * t104 * t96 * t57) + 16 * t8 - 8 * t1 + t47 * t18

      hjetmass_bubble_pppm_s124_dp = ret/16d0*(0,1d0)
      return

      end function
