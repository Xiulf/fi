// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpH34Ttg
(function($shade) {
    const $module = $shade["prim"] || ($shade["prim"] = {})
    const $member_Termination_17 = {
        report: function ($p0) {
            return 0;
        },
    };
    $module.$member_Termination_17 = $member_Termination_17;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpGEBdn0
(function($shade) {
    const $module = $shade["intrinsics"] || ($shade["intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpFTQA5q
(function($shade) {
    const $module = $shade["core/error"] || ($shade["core/error"] = {})
    function unwrap_unsafe($r0, $p0) {
        return $r0.unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp7zlkZd
(function($shade) {
    const $module = $shade["core/data/int"] || ($shade["core/data/int"] = {})
    const $member_Add_18 = {
        add: function ($p0, $p1) {
            return $p0 + $p1;
        },
    };
    $module.$member_Add_18 = $member_Add_18;
    const $member_Sub_19 = {
        sub: function ($p0, $p1) {
            return $p0 - $p1;
        },
    };
    $module.$member_Sub_19 = $member_Sub_19;
    const $member_Mul_20 = {
        mul: function ($p0, $p1) {
            return $p0 * $p1;
        },
    };
    $module.$member_Mul_20 = $member_Mul_20;
    const $member_Div_21 = {
        div: function ($p0, $p1) {
            return $p0 / $p1;
        },
    };
    $module.$member_Div_21 = $member_Div_21;
    const $member_Rem_22 = {
        rem: function ($p0, $p1) {
            return $p0 % $p1;
        },
    };
    $module.$member_Rem_22 = $member_Rem_22;
    const $member_Eq_23 = {
        eq: function ($p0, $p1) {
            return $p0 == $p1;
        },
    };
    $module.$member_Eq_23 = $member_Eq_23;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpQfDVzP
// /tmp/.tmpEua17P
(function($shade) {
    const $module = $shade["core/ops"] || ($shade["core/ops"] = {})
    function ne($p0, $p1) {
        return $shade["core/data/bool"].not($shade["core/data/int"].$member_Eq_23.eq($p0, $p1));
    }
    $module.ne = ne;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpM8J2No
(function($shade) {
    const $module = $shade["core/data/option"] || ($shade["core/data/option"] = {})
    const $member_Try_24 = {
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
                    $e0 = undefined;
                    break $l0
                };
            } while(0);
            return $e0;
        },
    };
    $module.$member_Try_24 = $member_Try_24;
    const $member_Unwrap_25 = {
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
    $module.$member_Unwrap_25 = $member_Unwrap_25;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpbbEIVq
(function($shade) {
    const $module = $shade["core/data/bool"] || ($shade["core/data/bool"] = {})
    function not($p2) {
        var $e4;
        $l4: do {
            if ($p2[0] == 1) {
                $e4 = undefined;
                break $l4
            };
            if ($p2[0] == 0) {
                $e4 = undefined;
                break $l4
            };
        } while(0);
        return $e4;
    }
    $module.not = not;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmppun8yb
(function($shade) {
    const $module = $shade["core/data/result"] || ($shade["core/data/result"] = {})
    function ok($p0) {
        var $e0;
        $l0: do {
            if ($p0[0] == 1) {
                $e0 = [1, $p0[1]];
                break $l0
            };
            if ($p0[0] == 0) {
                $e0 = undefined;
                break $l0
            };
        } while(0);
        return $e0;
    }
    $module.ok = ok;
    function err($p0) {
        var $e0;
        $l0: do {
            if ($p0[0] == 0) {
                $e0 = [1, $p0[1]];
                break $l0
            };
            if ($p0[0] == 1) {
                $e0 = undefined;
                break $l0
            };
        } while(0);
        return $e0;
    }
    $module.err = err;
    function unwrap_err($p0) {
        var $e0;
        $l0: do {
            if ($p0[0] == 0) {
                $e0 = $p0[1];
                break $l0
            };
            if ($p0[0] == 1) {
                throw "cannot unwrap_err an ok value"
            };
        } while(0);
        return $e0;
    }
    $module.unwrap_err = unwrap_err;
    const $member_Try_26 = {
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
                    $e0 = [0, $p0[1]];
                    break $l0
                };
            } while(0);
            return $e0;
        },
    };
    $module.$member_Try_26 = $member_Try_26;
    const $member_Unwrap_27 = {
        unwrap: function ($p0) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $p0[1];
                    break $l0
                };
                if ($p0[0] == 0) {
                    throw "cannot unwrap an err value"
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
    $module.$member_Unwrap_27 = $member_Unwrap_27;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpN2q9O7
// /tmp/.tmpJn6kHU
// /mnt/e/Language/fc/lib/js/include/js.js
function log(str) {
  console.log(str);
}
// /tmp/.tmp4C8s9U
// /tmp/.tmpzMnPs9
(function($shade) {
    const $module = $shade["js/console"] || ($shade["js/console"] = {})
    $module.log = console.log;
})(this.$shade || (this.$shade = {}));
