// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmp5H2gRn
(function($shade) {
    const $module = $shade["Intrinsics"] || ($shade["Intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmptneoOv
(function($shade) {
    const $module = $shade["Prim"] || ($shade["Prim"] = {})
    const $member_Termination_17 = {
        report: function ($p0) {
            return 0;
        },
    };
    $module.$member_Termination_17 = $member_Termination_17;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpZCh6kg
(function($shade) {
    const $module = $shade["Core.Foldable"] || ($shade["Core.Foldable"] = {})
    function foldMap($r0, $r1, $r2, $p0) {
        return $r0.foldr($p0, $shade["Core"].default(), $shade["Core.Ops"].concat);
    }
    $module.foldMap = foldMap;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpFbc9I1
(function($shade) {
    const $module = $shade["Data.Option"] || ($shade["Data.Option"] = {})
    const $member_Default_18 = {
        default: function () {
            return [0, undefined];
        },
    };
    $module.$member_Default_18 = $member_Default_18;
    const $member_Try_19 = {
        ret: function ($p0) {
            return [1, $p0];
        },
        bind: function ($p0, $p1) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $p1($p0[1]);
                    break $l0
                };
                if ($p0[0] == 0) {
                    $e0 = [0, undefined];
                    break $l0
                };
            } while(0);
            return $e0;
        },
    };
    $module.$member_Try_19 = $member_Try_19;
    const $member_Unwrap_20 = {
        unwrap: function ($p0) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $p0[1];
                    break $l0
                };
                if ($p0[0] == 0) {
                    throw "cannot unwrap a none value"
                };
            } while(0);
            return $e0;
        },
        unwrap_or: function ($p0, $p1) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $p0[1];
                    break $l0
                };
                if ($p0[0] == 0) {
                    $e0 = $p1;
                    break $l0
                };
            } while(0);
            return $e0;
        },
    };
    $module.$member_Unwrap_20 = $member_Unwrap_20;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpkJSCWi
// /tmp/.tmpfRMOnY
(function($shade) {
    const $module = $shade["Core.Cmp"] || ($shade["Core.Cmp"] = {})
    function ne($p0, $p1) {
        return $shade["Data.Bool"].not($shade["Data.Int"].$member_Eq_29.eq($p0, $p1));
    }
    $module.ne = ne;
    function lt($p0, $p1) {
        var $e4;
        $l4: do {
            if ($shade["Data.Int"].$member_Ord_30.cmp($p0, $p1)[0] == 0) {
                $e4 = [1, undefined];
                break $l4
            };
            $e4 = [0, undefined];
        } while(0);
        return $e4;
    }
    $module.lt = lt;
    function le($p0, $p1) {
        var $e4;
        $l4: do {
            if ($shade["Data.Int"].$member_Ord_30.cmp($p0, $p1)[0] == 2) {
                $e4 = [0, undefined];
                break $l4
            };
            $e4 = [1, undefined];
        } while(0);
        return $e4;
    }
    $module.le = le;
    function gt($p0, $p1) {
        var $e4;
        $l4: do {
            if ($shade["Data.Int"].$member_Ord_30.cmp($p0, $p1)[0] == 2) {
                $e4 = [1, undefined];
                break $l4
            };
            $e4 = [0, undefined];
        } while(0);
        return $e4;
    }
    $module.gt = gt;
    function ge($p0, $p1) {
        var $e4;
        $l4: do {
            if ($shade["Data.Int"].$member_Ord_30.cmp($p0, $p1)[0] == 0) {
                $e4 = [0, undefined];
                break $l4
            };
            $e4 = [1, undefined];
        } while(0);
        return $e4;
    }
    $module.ge = ge;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpVbsnkX
(function($shade) {
    const $module = $shade["Core.Ops"] || ($shade["Core.Ops"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpWJtEBJ
(function($shade) {
    const $module = $shade["Data.Result"] || ($shade["Data.Result"] = {})
    function ok($p6) {
        var $e6;
        $l6: do {
            if ($p6[0] == 1) {
                $e6 = [1, $p6[1]];
                break $l6
            };
            if ($p6[0] == 0) {
                $e6 = [0, undefined];
                break $l6
            };
        } while(0);
        return $e6;
    }
    $module.ok = ok;
    function err($p6) {
        var $e6;
        $l6: do {
            if ($p6[0] == 0) {
                $e6 = [1, $p6[1]];
                break $l6
            };
            if ($p6[0] == 1) {
                $e6 = [0, undefined];
                break $l6
            };
        } while(0);
        return $e6;
    }
    $module.err = err;
    function unwrap_err($p6) {
        var $e6;
        $l6: do {
            if ($p6[0] == 0) {
                $e6 = $p6[1];
                break $l6
            };
            if ($p6[0] == 1) {
                throw "cannot unwrap_err an ok value"
            };
        } while(0);
        return $e6;
    }
    $module.unwrap_err = unwrap_err;
    const $member_Try_21 = {
        ret: function ($p0) {
            return [1, $p0];
        },
        bind: function ($p10, $p11) {
            var $e10;
            $l10: do {
                if ([$p10, $p11][0][0] == 1) {
                    $e10 = [$p10, $p11][1]([$p10, $p11][0][1]);
                    break $l10
                };
                if ([$p10, $p11][0][0] == 0) {
                    $e10 = [0, [$p10, $p11][0][1]];
                    break $l10
                };
            } while(0);
            return $e10;
        },
    };
    $module.$member_Try_21 = $member_Try_21;
    const $member_Unwrap_22 = {
        unwrap: function ($p6) {
            var $e6;
            $l6: do {
                if ($p6[0] == 1) {
                    $e6 = $p6[1];
                    break $l6
                };
                if ($p6[0] == 0) {
                    throw "cannot unwrap an err value"
                };
            } while(0);
            return $e6;
        },
        unwrap_or: function ($p10, $p11) {
            var $e6;
            $l6: do {
                if ([$p10, $p11][0][0] == 1) {
                    $e6 = [$p10, $p11][0][1];
                    break $l6
                };
                if ([$p10, $p11][0][0] == 0) {
                    $e6 = [$p10, $p11][1];
                    break $l6
                };
            } while(0);
            return $e6;
        },
    };
    $module.$member_Unwrap_22 = $member_Unwrap_22;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpndFSZD
(function($shade) {
    const $module = $shade["Data.Int"] || ($shade["Data.Int"] = {})
    const $member_Default_23 = {
        default: function () {
            return 0;
        },
    };
    $module.$member_Default_23 = $member_Default_23;
    const $member_Add_24 = {
        add: function ($p0, $p1) {
            return $p0 + $p1;
        },
    };
    $module.$member_Add_24 = $member_Add_24;
    const $member_Sub_25 = {
        sub: function ($p0, $p1) {
            return $p0 - $p1;
        },
    };
    $module.$member_Sub_25 = $member_Sub_25;
    const $member_Mul_26 = {
        mul: function ($p0, $p1) {
            return $p0 * $p1;
        },
    };
    $module.$member_Mul_26 = $member_Mul_26;
    const $member_Div_27 = {
        div: function ($p0, $p1) {
            return $p0 / $p1;
        },
    };
    $module.$member_Div_27 = $member_Div_27;
    const $member_Rem_28 = {
        rem: function ($p0, $p1) {
            return $p0 % $p1;
        },
    };
    $module.$member_Rem_28 = $member_Rem_28;
    const $member_Eq_29 = {
        eq: function ($p0, $p1) {
            return $p0 == $p1;
        },
    };
    $module.$member_Eq_29 = $member_Eq_29;
    const $member_Ord_30 = {
        cmp: function ($p0, $p1) {
            var $e4;
            $l4: do {
                if ($p0 == $p1 ? 1 : $p0 < $p1 ? -1 : 1 == 0) {
                    $e4 = [1, undefined];
                    break $l4
                };
                if ($p0 == $p1 ? 1 : $p0 < $p1 ? -1 : 1 == 1) {
                    $e4 = [2, undefined];
                    break $l4
                };
                $e4 = [0, undefined];
            } while(0);
            return $e4;
        },
    };
    $module.$member_Ord_30 = $member_Ord_30;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp7MSkSv
(function($shade) {
    const $module = $shade["Core"] || ($shade["Core"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpPpwE7k
(function($shade) {
    const $module = $shade["Core.Error"] || ($shade["Core.Error"] = {})
    function unwrap_unsafe($r0, $p0) {
        return $r0.unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmphNfxHT
(function($shade) {
    const $module = $shade["Data.List"] || ($shade["Data.List"] = {})
    const $member_Default_31 = {
        default: function () {
            return [0, undefined];
        },
    };
    $module.$member_Default_31 = $member_Default_31;
    const $member_Foldable_32 = {
        foldl: function ($p0, $p1, $p2) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $member_Foldable_32.foldl($p0[1][1], $p2($p1)($p0[1][0]), $p2);
                    break $l0
                };
                if ($p0[0] == 0) {
                    $e0 = $p1;
                    break $l0
                };
            } while(0);
            return $e0;
        },
        foldr: function ($p0, $p1, $p2) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $p2($member_Foldable_32.foldr($p0[1][1], $p1, $p2))($p0[1][0]);
                    break $l0
                };
                if ($p0[0] == 0) {
                    $e0 = $p1;
                    break $l0
                };
            } while(0);
            return $e0;
        },
    };
    $module.$member_Foldable_32 = $member_Foldable_32;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmprqXaGW
(function($shade) {
    const $module = $shade["Data.Bool"] || ($shade["Data.Bool"] = {})
    function not($p2) {
        var $e4;
        $l4: do {
            if ($p2[0] == 1) {
                $e4 = [0, undefined];
                break $l4
            };
            if ($p2[0] == 0) {
                $e4 = [1, undefined];
                break $l4
            };
        } while(0);
        return $e4;
    }
    $module.not = not;
    const $member_Default_33 = {
        default: function () {
            return [0, undefined];
        },
    };
    $module.$member_Default_33 = $member_Default_33;
})(this.$shade || (this.$shade = {}));
