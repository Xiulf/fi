// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpEc41oA
(function($shade) {
    const $module = $shade["prim"] || ($shade["prim"] = {})
    const $member_Termination_17 = {
        report: function ($p0) {
            return 0;
        },
    };
    $module.$member_Termination_17 = $member_Termination_17;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpD2DYbc
(function($shade) {
    const $module = $shade["intrinsics"] || ($shade["intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmp62yOBq
(function($shade) {
    const $module = $shade["core/error"] || ($shade["core/error"] = {})
    function unwrap_unsafe($r0, $p0) {
        return $r0.unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpVkgo4c
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
// /tmp/.tmpCRseVC
// /tmp/.tmpnlrZM0
(function($shade) {
    const $module = $shade["core/ops"] || ($shade["core/ops"] = {})
    function ne($p0, $p1) {
        return $shade["core/data/bool"].not($shade["core/data/int"].$member_Eq_23.eq($p0, $p1));
    }
    $module.ne = ne;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp1IhBt0
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
                    $e0 = [0, undefined];
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
// /tmp/.tmpMJLltO
(function($shade) {
    const $module = $shade["core/data/bool"] || ($shade["core/data/bool"] = {})
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
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpmUvWHO
(function($shade) {
    const $module = $shade["core/data/result"] || ($shade["core/data/result"] = {})
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
    const $member_Try_26 = {
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
    $module.$member_Try_26 = $member_Try_26;
    const $member_Unwrap_27 = {
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
    $module.$member_Unwrap_27 = $member_Unwrap_27;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp1xajlW
// /tmp/.tmpsVbEWx
// /mnt/e/Language/fc/lib/js/include/js.js
function toString(x) {
  return x.toString();
}

function push(a, x) {
  a.push(x);
  return a;
}

function newArray(_) {
  return [];
}
// /tmp/.tmpY1dRyz
(function($shade) {
    const $module = $shade["js"] || ($shade["js"] = {})
    $module.toString = toString;
    $module.push = push;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpUh6Co7
(function($shade) {
    const $module = $shade["js/console"] || ($shade["js/console"] = {})
    $module.log = console.log;
    function print($r0, ) {
        return $r0.print$(new Array());
    }
    $module.print = print;
    const $member_Print_28 = {
        print$: function ($p0) {
            return console.log(...$p0);
        },
    };
    $module.$member_Print_28 = $member_Print_28;
    function $member_Print_29($r0) {
        return {
            print$: function ($p0) {
                function $l9($l9p0) {
                    return $r0.print$($shade["js"].push($p0, $shade["js"].toString($l9p0)));
                };
                return $l9;
            },
        };
    }
    $module.$member_Print_29 = $member_Print_29;
})(this.$shade || (this.$shade = {}));
