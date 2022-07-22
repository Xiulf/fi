// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpx7YyXN
(function($shade) {
    const $module = $shade["prim"] || ($shade["prim"] = {})
    const $member_Termination_17 = {
        report: function ($p0) {
            return 0;
        },
    };
    $module.$member_Termination_17 = $member_Termination_17;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpBCJmoE
(function($shade) {
    const $module = $shade["intrinsics"] || ($shade["intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpDuJGGb
(function($shade) {
    const $module = $shade["core/error"] || ($shade["core/error"] = {})
    function unwrap_unsafe($r0, $p0) {
        return $r0.unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmppZSA10
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
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp1HXcH6
// /tmp/.tmpHi3CN3
(function($shade) {
    const $module = $shade["core/ops"] || ($shade["core/ops"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp275mVN
(function($shade) {
    const $module = $shade["core/data/option"] || ($shade["core/data/option"] = {})
    const $member_Try_23 = {
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
    $module.$member_Try_23 = $member_Try_23;
    const $member_Unwrap_24 = {
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
    $module.$member_Unwrap_24 = $member_Unwrap_24;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpSCAyAB
// /tmp/.tmpRhrQxI
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
    const $member_Try_25 = {
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
    $module.$member_Try_25 = $member_Try_25;
    const $member_Unwrap_26 = {
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
    $module.$member_Unwrap_26 = $member_Unwrap_26;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp0uHgoY
// /mnt/e/Language/fc/test/target/js.js
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/lib/js/include/js.js
function log(str) {
  console.log(str);
}
// /tmp/.tmpQH2Qpy
(function($shade) {
    const $module = $shade["js/console"] || ($shade["js/console"] = {})
    $module.log = console.log;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpU7gnWk
// /tmp/.tmpO4KtVU
(function($shade) {
    const $module = $shade["main"] || ($shade["main"] = {})
    function main() {
        var $p0 = "test";
        var $p5 = [0, $shade["core/data/int"].$member_Add_18.add($shade["core/data/int"].$member_Sub_19.sub(21, 4), 12)];
        $shade["js/console"].log($p0);
        return $shade["js/console"].log($p5);
    }
    $module.main = main;
    $shade.main = main
})(this.$shade || (this.$shade = {}));
$shade.main();
