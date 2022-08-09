// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpND6Gvl
const $member_Termination_17 = {
    report: function($p0) {
        return 0;
    },
};
// /tmp/.tmp3ZQAio
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpHtkVGB
var Core_Foldable_foldMap;
Core_Foldable_foldMap = function($r0, $r1, $r2, $p0) {
    return $r0.foldr($p0, Core_default(), Core_Ops_concat);
};
// /tmp/.tmpDMAQP1
const $member_Default_18 = {
    default: function() {
        return false;
    },
};
const $member_Try_19 = {
    ret: function($p0) {
        return [true, $p0];
    },
    bind: function($p0, $p1) {
        var $e0;
        $l0: do {
            if ($p0[0]) {
                $e0 = $p1($p0[1]);
                break $l0
            };
            if (!$p0) {
                $e0 = false;
                break $l0
            };
        } while(0);
        return $e0;
    },
};
const $member_Unwrap_20 = {
    unwrap: function($p0) {
        var $e0;
        $l0: do {
            if ($p0[0]) {
                $e0 = $p0[1];
                break $l0
            };
            if (!$p0) {
                throw "cannot unwrap a none value"
            };
        } while(0);
        return $e0;
    },
    unwrap_or: function($p0, $p1) {
        var $e0;
        $l0: do {
            if ($p0[0]) {
                $e0 = $p0[1];
                break $l0
            };
            if (!$p0) {
                $e0 = $p1;
                break $l0
            };
        } while(0);
        return $e0;
    },
};
// /tmp/.tmpGkCCRF
// /tmp/.tmp7SX2uo
var Core_Cmp_ne;
var Core_Cmp_lt;
var Core_Cmp_le;
var Core_Cmp_gt;
var Core_Cmp_ge;
Core_Cmp_ne = function($p0, $p1) {
    return Data_Bool_not($member_Eq_29.eq($p0, $p1));
};
Core_Cmp_lt = function($p0, $p1) {
    var $e4;
    $l4: do {
        if ($member_Ord_30.cmp($p0, $p1) == 0) {
            $e4 = true;
            break $l4
        };
        $e4 = false;
    } while(0);
    return $e4;
};
Core_Cmp_le = function($p0, $p1) {
    var $e4;
    $l4: do {
        if ($member_Ord_30.cmp($p0, $p1) == 2) {
            $e4 = false;
            break $l4
        };
        $e4 = true;
    } while(0);
    return $e4;
};
Core_Cmp_gt = function($p0, $p1) {
    var $e4;
    $l4: do {
        if ($member_Ord_30.cmp($p0, $p1) == 2) {
            $e4 = true;
            break $l4
        };
        $e4 = false;
    } while(0);
    return $e4;
};
Core_Cmp_ge = function($p0, $p1) {
    var $e4;
    $l4: do {
        if ($member_Ord_30.cmp($p0, $p1) == 0) {
            $e4 = false;
            break $l4
        };
        $e4 = true;
    } while(0);
    return $e4;
};
// /tmp/.tmpbF72UQ
// /tmp/.tmpfP8FwJ
var Data_Result_ok;
var Data_Result_err;
var Data_Result_unwrap_err;
Data_Result_ok = function($p6) {
    var $e6;
    $l6: do {
        if ($p6[0]) {
            $e6 = [true, $p6[1]];
            break $l6
        };
        if (!$p6[0]) {
            $e6 = false;
            break $l6
        };
    } while(0);
    return $e6;
};
Data_Result_err = function($p6) {
    var $e6;
    $l6: do {
        if (!$p6[0]) {
            $e6 = [true, $p6[1]];
            break $l6
        };
        if ($p6[0]) {
            $e6 = false;
            break $l6
        };
    } while(0);
    return $e6;
};
Data_Result_unwrap_err = function($p6) {
    var $e6;
    $l6: do {
        if (!$p6[0]) {
            $e6 = $p6[1];
            break $l6
        };
        if ($p6[0]) {
            throw "cannot unwrap_err an ok value"
        };
    } while(0);
    return $e6;
};
const $member_Try_21 = {
    ret: function($p0) {
        return [true, $p0];
    },
    bind: function($p10, $p11) {
        var $e10;
        $l10: do {
            if ([$p10, $p11][0][0]) {
                $e10 = [$p10, $p11][1]([$p10, $p11][0][1]);
                break $l10
            };
            if (![$p10, $p11][0][0]) {
                $e10 = [false, [$p10, $p11][0][1]];
                break $l10
            };
        } while(0);
        return $e10;
    },
};
const $member_Unwrap_22 = {
    unwrap: function($p6) {
        var $e6;
        $l6: do {
            if ($p6[0]) {
                $e6 = $p6[1];
                break $l6
            };
            if (!$p6[0]) {
                throw "cannot unwrap an err value"
            };
        } while(0);
        return $e6;
    },
    unwrap_or: function($p10, $p11) {
        var $e6;
        $l6: do {
            if ([$p10, $p11][0][0]) {
                $e6 = [$p10, $p11][0][1];
                break $l6
            };
            if (![$p10, $p11][0][0]) {
                $e6 = [$p10, $p11][1];
                break $l6
            };
        } while(0);
        return $e6;
    },
};
// /tmp/.tmpW33Hhe
const $member_Default_23 = {
    default: function() {
        return 0;
    },
};
const $member_Add_24 = {
    add: function($p0, $p1) {
        return $p0 + $p1;
    },
};
const $member_Sub_25 = {
    sub: function($p0, $p1) {
        return $p0 - $p1;
    },
};
const $member_Mul_26 = {
    mul: function($p0, $p1) {
        return $p0 * $p1;
    },
};
const $member_Div_27 = {
    div: function($p0, $p1) {
        return $p0 / $p1;
    },
};
const $member_Rem_28 = {
    rem: function($p0, $p1) {
        return $p0 % $p1;
    },
};
const $member_Eq_29 = {
    eq: function($p0, $p1) {
        return $p0 == $p1;
    },
};
const $member_Ord_30 = {
    cmp: function($p0, $p1) {
        var $e4;
        $l4: do {
            if ($p0 == $p1 ? 1 : $p0 < $p1 ? -1 : 1 == 0) {
                $e4 = 1;
                break $l4
            };
            if ($p0 == $p1 ? 1 : $p0 < $p1 ? -1 : 1 == 1) {
                $e4 = 2;
                break $l4
            };
            $e4 = 0;
        } while(0);
        return $e4;
    },
};
// /tmp/.tmpcHj4Vn
// /tmp/.tmp0ghWZP
var Core_Error_unwrap_unsafe;
Core_Error_unwrap_unsafe = function($r0, $p0) {
    return $r0.unwrap($p0);
};
// /tmp/.tmpvAdYgg
const $member_Default_31 = {
    default: function() {
        return false;
    },
};
const $member_Foldable_32 = {
    foldl: function($p0, $p1, $p2) {
        var $e0;
        $l0: do {
            if ($p0[0]) {
                $e0 = $member_Foldable_32.foldl($p0[1][1], $p2($p1)($p0[1][0]), $p2);
                break $l0
            };
            if (!$p0) {
                $e0 = $p1;
                break $l0
            };
        } while(0);
        return $e0;
    },
    foldr: function($p0, $p1, $p2) {
        var $e0;
        $l0: do {
            if ($p0[0]) {
                $e0 = $p2($member_Foldable_32.foldr($p0[1][1], $p1, $p2))($p0[1][0]);
                break $l0
            };
            if (!$p0) {
                $e0 = $p1;
                break $l0
            };
        } while(0);
        return $e0;
    },
};
// /tmp/.tmpxu9sQf
var Data_Bool_not;
Data_Bool_not = function($p2) {
    var $e4;
    $l4: do {
        if ($p2) {
            $e4 = false;
            break $l4
        };
        if (!$p2) {
            $e4 = true;
            break $l4
        };
    } while(0);
    return $e4;
};
const $member_Default_33 = {
    default: function() {
        return false;
    },
};
// /mnt/e/Language/fc/lib/js/include/js.js
function toString(x) {
  return x.toString();
}

function push(a, x) {
  a.push(x);
  return a;
}

function concatString(a, b) {
  return a + b.toString();
}
// /tmp/.tmpfS05Yr
var Js_Console_print;
Js_Console_print = function($r0, ) {
    return $r0.print$(new Array());
};
const $member_Print_34 = {
    print$: function($p0) {
        return console.log(...$p0);
    },
};
function $member_Print_35($r0) {
    return {
        print$: function($p0) {
            function $l7($l7p0) {
                return $r0.print$(push($p0, $l7p0));
            };
            return $l7;
        },
    };
}
// /tmp/.tmpMgIzXH
var Js_DOM_createElement;
Js_DOM_createElement = function($t0, $p0) {
    return document.createElement($t0);
};
// /tmp/.tmpWqXbm4
var Js_toArray;
Js_toArray = function($r0, $p0) {
    function $l12($l12p0) {
        return function($l12p1) {
            return push($l12p0, $l12p1);
        };
    };
    return $r0.foldl($p0, new Array(), $l12);
};
const $member_Concat_37 = {
    concat: function($p0, $p1) {
        return concatString($p0, $p1);
    },
};
