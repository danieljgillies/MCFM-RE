
      complex*32 function hjetmass_triangle_pppm_s134_0_mhsq
     &     (i1,i2,i3,i4,za,zb,mt)
          implicit complex*32 (t)
          integer i1,i2,i3,i4
          include 'types.f'
          include 'mxpart.f'
          include 'constants.f'
          complex*32 za(mxpart,mxpart), zb(mxpart,mxpart)
          
          complex*32 ret
          double precision mt
          real*16 cg

      t1 = za(i1, i3)
      t2 = zb(i3, i1)
      t3 = za(i1, i4)
      t4 = zb(i4, i1)
      t5 = za(i3, i4)
      t6 = zb(i4, i3)
      t7 = t1 * t2
      t8 = t3 * t4
      t9 = t5 * t6
      t10 = t7 + t8 + t9
      t11 = za(i1, i2)
      t12 = zb(i2, i1)
      t13 = za(i2, i3)
      t14 = zb(i3, i2)
      t15 = za(i2, i4)
      t16 = zb(i4, i2)
      t17 = t11 * t12
      t18 = t13 * t14
      t19 = t15 * t16
      t20 = t17 + t18 + t19
      if ( real(t20) > 0q0) then; cg = 1q0; else; cg = -1q0; end if
      t20 = cg * sqrt(t20 ** 2) + t17 + t18 + t19
      t21 = t1 * t14 + t16 * t3
      t22 = 0.1q1 / t20
      t23 = -2 * t17 * t10 * t22 + t7 + t8
      t24 = 2 * t11
      t25 = t24 * t14 * t10 * t22 + t3 * t6
      t24 = t15 * (t24 * t16 * t10 * t22 - t1 * t6)
      t26 = t12 * (t13 * t25 + t24)
      t27 = t11 * t23
      t24 = t12 * (-t27 + t24)
      t28 = t3 * t12
      t29 = -t14 * t5 + t28
      t30 = t3 * t2
      t31 = t14 * t15
      t32 = -2 * t31 * t10 * t22 + t30
      t33 = t5 * t2
      t34 = t15 * t12
      t35 = 2 * t34 * t10 * t22 - t33
      t36 = t16 * t35
      t37 = t12 * t23
      t38 = t11 * (-t37 + t36)
      t39 = 2 * t13
      t40 = t39 * t12 * t10 * t22 + t4 * t5
      t36 = t11 * (t14 * t40 + t36)
      t41 = 0.1q1 / t5
      t26 = 0.1q1 / t26
      t42 = 0.1q1 / t4
      t20 = 0.1q1 / t20
      t43 = 0.1q1 / t3
      t44 = 0.1q1 / t10
      t24 = 0.1q1 / t24
      t45 = 0.1q1 / t13
      t46 = 0.1q1 / t6
      t47 = 0.1q1 / t11
      t48 = mt ** 2
      t49 = t2 ** 2
      t50 = t15 ** 2
      t51 = t15 * t50
      t52 = t12 ** 2
      t53 = t52 ** 2
      t54 = t12 * t52
      t55 = t10 ** 2
      t56 = t26 ** 2
      t57 = t26 * t56
      t58 = t24 ** 2
      t59 = t24 * t58
      t60 = t23 ** 2
      t61 = t19 * t45
      t62 = t14 * t25
      t63 = t2 * t16
      t64 = t62 * t58 * t46
      t65 = t2 * t23
      t66 = t15 * t25
      t67 = t43 * t20 ** 2 * t21
      t68 = t67 * t51 * t55
      t69 = t46 * t41
      t70 = t14 * t43
      t71 = t69 * t48
      t72 = 0.1q1 / t25
      t73 = 0.1q1 / t23
      t74 = t19 + t18
      t75 = -t61 - t14
      t76 = t7 * t43
      t77 = t76 + t4
      t78 = t58 - t56
      t79 = -t58 + t56
      t80 = t14 ** 2
      t81 = t15 * t56
      t82 = t2 * t25
      t83 = t25 * t57
      t84 = t83 * t74
      t85 = t43 * t42
      t86 = t23 * t80
      t87 = t17 * t58
      t88 = t14 * t23
      t89 = t12 * t25
      t90 = t50 * t16 ** 2
      t91 = t2 * t43
      t92 = t47 * t73
      t93 = t92 * t62
      t94 = t12 * t41
      t95 = t45 * t26
      t96 = t20 * t21
      t97 = t96 * t50
      t98 = t25 ** 2
      t99 = t45 ** 2
      t100 = t2 * t14
      t101 = t100 * t46
      t102 = t34 * t45
      t103 = t52 * t11
      t104 = t34 * t98
      t105 = t18 * t2
      t106 = t100 * t42
      t107 = t69 * t4
      t108 = t65 * t12
      t109 = t88 * t56
      t110 = t72 * t99
      t111 = t41 * t56
      t112 = t19 * t47
      t113 = t26 * t23
      t114 = t113 * t15
      t115 = t43 * t41
      t116 = t97 * t52 * t10
      t104 = t116 * (t25 * (t108 * (-t19 * (t69 + t85) + t17 * (-t69 - t
     #85)) * t59 - t31 * t47 * (t107 + t43) * t58) + t114 * (-t111 * t34
     # * t16 * t23 * t45 + t85 * (t12 * (-t109 - t95) - t110) * t6) + t1
     #15 * (-t90 * t83 * t65 * t12 * t42 * t46 - t104 * t46 * (t90 * t47
     # + t103) * t59 - t87 * t65 * t14 * t42 * t46 + t42 * (t102 + t101)
     # * t26 + t105 * t42 * t46 * (-t89 + t88) * t56 - t106 * t24 * t46)
     # * t20 * t10 + t104 * (t43 * (t112 + t12) + t69 * (t112 * t4 + t12
     # * t77)) * t59)
      t117 = t34 * t16 * t25 * t24
      t118 = t25 * t58
      t119 = t118 * t46
      t120 = t14 * t21
      t121 = t37 * t95
      t122 = t48 * t22
      t123 = t122 * t43
      t124 = t94 * t50
      t37 = t124 * (t67 * t46 * t24 * (t2 * (t118 * t11 ** 2 * t23 * t42
     # * t53 + t118 * t90 * t23 * t42 * t52) - t15 * t80 * t47) * t55 + 
     #t96 * t34 * (-t37 * t56 * (t88 * t26 + t45) + t76 * (-t110 * t113 
     #* t42 + t119 * t47 * (t117 - t14))) * t10 + t123 * (t120 * t32 * t
     #47 * t46 * t24 + t121 * t29 * t42))
      t18 = t37 + t104 + t97 * t12 * t10 * (t52 * (t85 * t57 * t15 * (t7
     # * t75 * t41 - t61 * t6) * t60 + t65 * (t84 * t69 + t85 * (t84 + (
     #-t81 * t45 + t82 * (t18 * t57 + t19 * (-t59 + t57)) * t46) * t41 *
     # t1))) - t27 * t85 * t69 * t1 * t54 * t49 * t25 * t59 + t93 * t46 
     #* t24 * (t77 * t47 * t41 * t15 - t91) + t94 * t85 * ((t89 * (-t86 
     #* t13 ** 2 * t57 + t87) + t19 * (t89 * t78 + t88 * t79)) * t46 * t
     #2 + t34 * t60 * t57 * (t13 * t80 + t90 * t45)) * t20 * t10 + t95 *
     # t94 * t65 * t42 * t72)
      t27 = 0.1q1 / t38
      t37 = 0.1q1 / t40
      t36 = 0.1q1 / t36
      t38 = t66 * t47
      t40 = t65 * t42
      t84 = -t40 + t38
      t90 = t89 + t88
      t104 = t35 ** 2
      t125 = t21 ** 2
      t126 = t15 * t41
      t127 = t126 * t125
      t128 = t25 * t21
      t129 = t12 * t98
      t130 = t34 * t41
      t131 = t46 * t2
      t132 = t49 * t15
      t133 = t2 * t21
      t134 = t52 * t50
      t135 = t102 * t65
      t136 = t49 * t46
      t137 = t49 * t42
      t138 = t137 * t46
      t139 = t73 * (t89 - t133) + t14
      t140 = t7 * t42
      t141 = t140 + t3
      t142 = t47 ** 2
      t143 = t47 * t142
      t144 = t72 ** 2
      t145 = t128 * t47
      t146 = t85 * t1
      t147 = t133 * t34
      t148 = t140 * t70 * t50
      t149 = t23 * t56
      t150 = t149 * t42
      t151 = t110 * t114
      t33 = t34 * (t41 * (t46 * (t147 * (t146 * t49 * t21 - t88) * t56 +
     # t12 * (t15 * (t1 * (t85 * t125 * t2 * t49 + t129 * t47 * t2 - t14
     #5 * t49) + t129 * t8 * t47) + t65 * (t1 * (-t89 * t2 * t42 + t137 
     #* t21) - t89 * t3) - t100 * t141 * t60 + t145 * t4 * t139 * t50) *
     # t58 + t66 * t142 * (-t8 * t89 * t73 + t7 * (t73 * (-t31 * t21 * t
     #43 - t89) - t14)) * t24) + t148 * t12 * t21 * t60 * t45 * t72 * t5
     #6 + t148 * t21 * t60 * t99 * t144 * t26) + t151 * t85 * t21 * (t34
     # - t33) * t6 + t147 * t43 * (-t118 * t92 * t15 * t21 - t150 * (t7 
     #* t45 + t14)))
      t100 = t52 * t41
      t145 = t92 * t46
      t148 = t145 * t24
      t152 = t85 * t5
      t153 = t130 * t21
      t154 = t70 * t42
      t155 = t95 * t50
      t156 = t155 * t21
      t157 = t85 * t12
      t158 = t12 * t24
      t159 = t158 * t69
      t160 = t26 * t15
      t28 = t2 * (t15 * (t129 * (-t152 * t12 * t56 + t148) + t46 * (t100
     # * t26 * t42 + t24 * t14 * t47 - t109 * t12) * t25) + t72 * (t70 *
     # t1 * t50 * t60 * t26 * t42 * t99 - t156 * t23 * (t153 * t26 + t15
     #4)) + t89 * (-t157 * t21 * t56 + t69 * (t88 * t1 * t58 * t47 + (-t
     #58 - t56) * t21 * t12)) * t50 - t69 * t17 * t14 * t35 * t27 * t42)
     # + t50 * (t25 * (t28 * t107 * t88 * t47 * t58 + t46 * t14 * (t70 -
     # t94) * t47 * t24) + t95 * t88 * t12 * (t114 * t21 * t41 * t72 + t
     #85) + t92 * t15 * t52 * t21 * t98 * t43 * t58) + t137 * t15 * (-t1
     #51 * t1 * t21 * t43 + t89 * (-t89 * t1 * t46 + t21 * t5) * t43 * t
     #56 - t159 * t21) + t155 * t85 * t23 * (t88 * t5 * t72 * t45 + t160
     # * t52 * t21) * t6
      t109 = t89 + t88
      t114 = t27 - t36
      t137 = t73 ** 2
      t151 = t2 * t42
      t161 = t85 * t31
      t162 = t127 * t2
      t163 = t133 * t12
      t106 = t106 * t46
      t30 = t11 * (t69 * t80 * t27 * t73 * t104 + t106 * (t70 * t114 + t
     #94 * t36) * t35) + t15 * (t146 * t135 * (-t162 * t72 + t88) * t56 
     #+ t133 * t85 * ((-t133 * t99 * t144 + t110 * t12) * t41 * t23 * t5
     #0 * t1 + t101) * t26 + t46 * (t50 * (t21 * (-t94 * t76 * t142 * t1
     #37 * t98 - t62 * t4 * t41 * t142 * t73) + t82 * t41 * t77 * t137 *
     # t142 * t125) + t14 * t40 * (-t70 + t94) + t126 * t82 * (t8 + t7) 
     #* t73 * t142 * t21) * t24 + t163 * (-t161 * t23 + t69 * (t23 * (-t
     #31 + t30) + t15 * (-t151 * t109 + t38 * t139) * t43 * t1)) * t58)
      t40 = -t133 + t88
      t77 = t9 + t7
      t94 = t21 * t6
      t139 = t89 * t21
      t146 = t12 * t43
      t164 = t31 * t43
      t77 = t42 * (t113 * t43 * t50 * (t12 * t77 + t15 * (-t2 * t6 * t14
     #4 * t125 + t88 * t6 * t144 * t21)) * t99 + t82 * t34 * t43 * t26 *
     # (t1 * (t21 * t26 * t49 + (-t153 - t88) * t26 * t2) - t14) * t46 +
     # t146 * (t2 * (-t149 * t31 * t5 * t25 - t139 * t58 * t50) - t11 * 
     #t14 * t104 * t37 * t36) + t95 * t23 * t15 * (t12 * (t94 * t72 * t4
     #0 * t43 * t26 * t50 + t9 * t160 * t40 * t43) + t52 * (t76 * t21 * 
     #t41 * t26 * t50 + t66 * t77 * t43 * t26) - t65 * t14 * t72)) - t10
     #7 * t50 * t25 * t24 * (t128 * t34 * t137 + t14 * t3) * t142 + t89 
     #* t24 * t21 * t50 * (t164 * t24 + t69 * (-t8 * t24 + t73) * t2) * 
     #t47
      t28 = t12 * t77 + t12 * t30 + t12 * t28 + t33 + t12 * (-t138 * t90
     # * t24 + t42 * (-t134 * t23 * t41 * t45 + t136 * t90 - t135) * t26
     # + t34 * (t89 * t84 + t132 * (t69 + t85) * t125 - t133 * t84 + t88
     # * t84) * t58 + t34 * (t15 * (t21 * (t130 - t2) + t89) * t45 * t23
     # + t131 * (t2 * (t127 + t128) - t129) + t31 * t60 * t45 + t132 * t
     #85 * t21 * (-t88 * t69 * t1 + t21)) * t56) + (-t14 * t80 * t27 * t
     #43 * t46 * t73 + t41 * t54 * t42 * t37 * t36) * t104 * t11
      t30 = t61 + t14
      t33 = t14 * t72
      t77 = t33 * t45
      t84 = t69 * t12
      t90 = t145 * t129
      t127 = t15 * t23
      t115 = t122 * t115
      t135 = t96 * t15
      t145 = t59 - t57
      t153 = t42 * t26
      t165 = t12 * t56
      t166 = t98 * t58
      t167 = t124 * t16
      t168 = t118 * t52
      t169 = t91 * t15
      t170 = t14 * (t1 * t12 + t16 * t5)
      t171 = t21 * t12
      t172 = t16 * t32
      t36 = t135 * (-t134 * t107 * t92 * t98 * t58 + t169 * (t1 * (-t167
     # * (t110 * t60 * t56 * t42 + t166 * t46 * t142 * t73) + t126 * (t1
     #48 * t129 * (-t158 + t92) + (-t165 * t42 * t45 * t72 - t153 * t99 
     #* t144) * t60 * t14)) - t168 * t42) + t157 * t136 * (t31 * t78 * t
     #41 * t23 * t1 - t87 * t25)) * t10 + t128 * t7 * t50 * (-t66 * t107
     # * t143 * t137 * t24 + t65 * (t69 * t145 + t85 * t145) * t52) + t1
     #15 * (t72 * (t170 * t51 * t60 * t26 * t42 * t99 - t155 * t14 * t60
     # * t29 * t42) + t52 * t51 * t21 * t98 * t58 * t46 * t47 - t108 * t
     #21 * t56 * t42 * (t14 * (-t39 * t14 * t10 * t22 + t7 + t9) + t172)
     # * t46 * t50 + t160 * t131 * t42 * (-t171 * t32 + t88 * t29) - t17
     # * t29 * t104 * t42 * t37 * t36)
      t37 = t47 * t25
      t39 = t85 * t56
      t78 = t41 * (((t4 * t10 * t20 * t137 - t123 * t73) * t142 * t46 * 
     #t24 + t7 * t4 * t46 * t142 * t73 * t58) * t98 * t21 + t39 * t122 *
     # t60 * t45 * (t16 * t29 + t170)) * t51 + t133 * t69 * t42 * (-t2 *
     # t10 * t26 * t20 + t123 * t32 * t24) * t15 + t133 * ((t88 * (t85 *
     # t58 + t69 * (t58 - t56)) - t43 * t25 * t24 * t46 * (t63 * t42 * t
     #24 + t37 * t137)) * t20 * t10 + t172 * t85 * t71 * t23 * t22 * t58
     #) * t50
      t39 = t82 * t97 * t10 * (t46 * (t118 * t43 * t73 + t111) + t39) + 
     #t7 * t21 * ((t85 * t6 + t41) * t45 * t57 * t60 - t98 * t59 * t47 *
     # (t107 + t43)) * t51
      t27 = t12 * (t153 * t94 * t76 * t51 * t60 * t45 * t99 * t144 + t16
     #1 * t136 * t96 * t10 * t24) + t52 * t78 + t54 * t39 + t12 * t36 + 
     #t12 * (t106 * (t2 * t44 + t115 * t29 * (t11 * t35 * t114 - t127 * 
     #t24)) + t134 * (-t118 * t85 * t65 * t71 * t22 + t23 * (t6 * t15 * 
     #t23 * t45 - t82) * t57 + t25 * (-t38 * t4 + t65) * t59) * t21 + t1
     #46 * t50 * (t140 * t110 * t81 * t6 * t60 + t122 * (-t112 * t46 * t
     #58 - t148) * t41 * t32 * t25) * t21 + t135 * (t42 * (t155 * t43 * 
     #t72 * t60 * t6 * (t75 * t26 * t12 - t77) + t126 * t26 * t72 * t60 
     #* (t30 * t26 * t12 + t77) * t2 + t84 * (t34 * t79 * t43 * t25 * t1
     # - t149 * t74) * t49) + t90 * t63 * t50 * t43 * t58 - t129 * t107 
     #* t51 * t16 * t142 * t73 * t58) * t10) - t71 * t11 * t80 * t29 * t
     #104 * t22 * t43 * t73 * t27
      t32 = t1 ** 2
      t35 = t3 * t4 ** 2
      t36 = t35 * t69
      t38 = t32 * t49
      t39 = t69 * t32 * t49
      t75 = t39 * t43
      t77 = t5 * t6 ** 2
      t78 = t107 * t3
      t79 = t152 * t6
      t7 = t7 * t41
      t1 = t34 * t21 * (t23 * (t84 * t49 * t141 * t56 - t83 * t2 * t52 *
     # t15 * (t78 + t79)) + t60 * (t134 * t41 * t45 * (t38 * t85 + t8) *
     # t57 + t102 * t72 * (t2 * (-t3 * t41 + t42 * (-t7 - t6)) + (t6 * (
     #t79 + 1) + t7) * t45 * t15) * t56 + t160 * t99 * t42 * t144 * (-t1
     # * t49 * t41 + (t38 * t41 + t77) * t45 * t43 * t15)) + t24 * t25 *
     # ((-t9 * t52 * t43 * t58 * t47 + t158 * t73 * (t75 + t4) * t142 - 
     #t36 * t137 * t143) * t25 * t50 + t66 * (t136 * t1 * t43 * t137 * t
     #142 - t91 * t158 * t92 * t5) + t152 * t158 * t49))
      t1 = t1 - t138 * t44 * (t5 * t80 * t43 + t100 * t3) + t132 * t52 *
     # (t150 + t119 * (t76 * t42 + 1)) * t21 + t171 * (t98 * (-t75 * t14
     #3 * t137 * t24 - t69 * t52 * (t38 * t43 + t35) * t47 * t59 + t12 *
     # t73 * (t36 + t76) * t142 * t58) + t121 * t85 * (t23 * (t95 * t32 
     #* t49 * t41 * t72 + t77 * t165) - t33 * t10 * t41 * t20) + t159 * 
     #t93 * t10 * t20 * t43) * t51 + t163 * (t23 * ((t78 * t59 + t85 * (
     #t39 * (t59 - t57) + t9 * t59)) * t25 * t52 - t113 * t6 * t144 * t4
     #2 * t99) + t4 * t98 * t24 * t46 * t142 * t137 - t90 * t58 * (t76 +
     # t4)) * t50
      t3 = t89 * t73 + t14
      t4 = t50 * t16
      t5 = t4 * t21 * t23
      t6 = t69 * t24
      t7 = t34 * t20 * t10
      t8 = t21 * t15 * t43
      t9 = t8 * t73 + 1
      t32 = t19 + t17
      t4 = t20 * t15 * t52 * t10 * (t46 * (t154 * t2 * (-t126 * t13 * t1
     #09 * t21 - t129 * t13 - t5 * t41) * t56 + t41 * (t139 * t50 * t43 
     #* (-t133 * t73 + t14) + t4 * t37 * (-t133 * t9 + t88) + t151 * (t1
     #69 * t32 * t125 + t127 * (-t70 * t17 + t63) * t21 - t14 * t60 * t3
     #2 - t89 * t23 * t32)) * t58) + t95 * t85 * t15 * (t88 * (t113 * t1
     #9 * (t126 * t21 * t72 + 1) + 1) + t89) + t150 * t124 * t70 * t21)
      t8 = t7 * (t42 * (t159 * t2 * (t133 * (t17 * t23 * t24 + 1) - t88 
     #- t89) + (-t133 * t102 + t21 * t41 * (-t120 * t65 * t144 + t52) * 
     #t45 * t50 + t101 * (t89 + t88 - t133)) * t43 * t26 + t34 * (t12 * 
     #(t23 * t30 * t25 - t63 * t46 * t98) + t23 * (-t63 * t50 * t125 * t
     #41 * t72 * t45 + t86)) * t43 * t56) + t166 * t69 * t15 * t54 * t9 
     #+ t6 * t15 * (t14 * (t21 * (-t164 + t2) - t88) - t8 * t52 * t98 * 
     #t137 - t62 * t12) * t47)
      t3 = t8 + t4 + t7 * (t168 * t69 * t31 * t23 + t6 * t50 * (t117 * t
     #3 * t43 * t21 + t91 * t73 * t3 * t125 + t52 * t16 * t98 * t24) * t
     #47 + t85 * (t156 * t41 * t72 * (t80 * t60 * t72 - t163) + t147 * t
     #69 * (-t103 * t25 + t19 * (-t89 - t88)) * t58 + t165 * (t88 * t50 
     #* t21 * t41 * t40 * t72 + t131 * (t162 * t74 + t128 * (t63 * t15 +
     # t105 - t167) - t62 * t74 * t23) + t5 * t45 * (t130 - t2))))
      t4 = 128
      ret = t4 * (t70 * t48 * t49 * t29 * t22 * t42 * t46 * t44 + t68 * 
     #(t23 * (t63 * t62 * t13 * t42 * t46 * t57 + (-t61 - t14) * t42 * t
     #56) - t31 * t16 * t60 * t57 * t42 - t64) * t41 * t54 - t64 * t67 *
     # t55 * t50 ** 2 * t52 * t16 * t47 * t41 + t69 * t68 * t16 * t25 * 
     #t59 * (-t65 * t11 * t42 + t66) * t53 - t71 * t12 * t49 * t29 * t22
     # * t42 * t44) - 64 * t18 - 4 * t28 - 32 * t27 - 16 * t1 + 8 * t3 +
     # 24 * t116 * t2 * (t89 * t69 * t58 + t150 * t70)

      hjetmass_triangle_pppm_s134_0_mhsq = ret/32q0/(0,1q0)
      return

      end function
