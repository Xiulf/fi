// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmp6avEyi
(function($shade) {
    const $module = $shade["Intrinsics"] || ($shade["Intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp7ftQbJ
(function($shade) {
    const $module = $shade["Prim"] || ($shade["Prim"] = {})
    const $member_Termination_17 = {
        report: function ($p0) {
            return 0;
        },
    };
    $module.$member_Termination_17 = $member_Termination_17;
})(this.$shade || (this.$shade = {}));
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpTEAc46
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
    const $member_Try_18 = {
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
    $module.$member_Try_18 = $member_Try_18;
    const $member_Unwrap_19 = {
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
    $module.$member_Unwrap_19 = $member_Unwrap_19;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpnjYcMS
(function($shade) {
    const $module = $shade["Data.Int"] || ($shade["Data.Int"] = {})
    const $member_Add_20 = {
        add: function ($p0, $p1) {
            return $p0 + $p1;
        },
    };
    $module.$member_Add_20 = $member_Add_20;
    const $member_Sub_21 = {
        sub: function ($p0, $p1) {
            return $p0 - $p1;
        },
    };
    $module.$member_Sub_21 = $member_Sub_21;
    const $member_Mul_22 = {
        mul: function ($p0, $p1) {
            return $p0 * $p1;
        },
    };
    $module.$member_Mul_22 = $member_Mul_22;
    const $member_Div_23 = {
        div: function ($p0, $p1) {
            return $p0 / $p1;
        },
    };
    $module.$member_Div_23 = $member_Div_23;
    const $member_Rem_24 = {
        rem: function ($p0, $p1) {
            return $p0 % $p1;
        },
    };
    $module.$member_Rem_24 = $member_Rem_24;
    const $member_Eq_25 = {
        eq: function ($p0, $p1) {
            return $p0 == $p1;
        },
    };
    $module.$member_Eq_25 = $member_Eq_25;
    const $member_Ord_26 = {
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
    $module.$member_Ord_26 = $member_Ord_26;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpSKjfOO
// /tmp/.tmphYdqqJ
(function($shade) {
    const $module = $shade["Core.Error"] || ($shade["Core.Error"] = {})
    function unwrap_unsafe($r0, $p0) {
        return $r0.unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpsF5VYO
(function($shade) {
    const $module = $shade["Data.List"] || ($shade["Data.List"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpU7Tfrp
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
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpEZ6U0t
(function($shade) {
    const $module = $shade["Core.Ops"] || ($shade["Core.Ops"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpKBzGn0
(function($shade) {
    const $module = $shade["Data.Option"] || ($shade["Data.Option"] = {})
    const $member_Try_27 = {
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
    $module.$member_Try_27 = $member_Try_27;
    const $member_Unwrap_28 = {
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
    $module.$member_Unwrap_28 = $member_Unwrap_28;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpQAY6j5
// /tmp/.tmp9hBrVJ
(function($shade) {
    const $module = $shade["Core.Cmp"] || ($shade["Core.Cmp"] = {})
    function ne($p0, $p1) {
        return $shade["Data.Bool"].not($shade["Data.Int"].$member_Eq_25.eq($p0, $p1));
    }
    $module.ne = ne;
    function lt($p0, $p1) {
        var $e4;
        $l4: do {
            if ($shade["Data.Int"].$member_Ord_26.cmp($p0, $p1)[0] == 0) {
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
            if ($shade["Data.Int"].$member_Ord_26.cmp($p0, $p1)[0] == 2) {
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
            if ($shade["Data.Int"].$member_Ord_26.cmp($p0, $p1)[0] == 2) {
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
            if ($shade["Data.Int"].$member_Ord_26.cmp($p0, $p1)[0] == 0) {
                $e4 = [0, undefined];
                break $l4
            };
            $e4 = [1, undefined];
        } while(0);
        return $e4;
    }
    $module.ge = ge;
})(this.$shade || (this.$shade = {}));
// /mnt/e/Language/fc/test/target/js.js
// /mnt/e/Language/fc/test/target/core.js
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
// /tmp/.tmpDnqHJk
(function($shade) {
    const $module = $shade["Js.Console"] || ($shade["Js.Console"] = {})
    $module.log = console.log;
    function print($r0, ) {
        return $r0.print$(new Array());
    }
    $module.print = print;
    const $member_Print_29 = {
        print$: function ($p0) {
            return console.log(...$p0);
        },
    };
    $module.$member_Print_29 = $member_Print_29;
    function $member_Print_30($r0) {
        return {
            print$: function ($p0) {
                function $l9($l9p0) {
                    return $r0.print$($shade["Js"].push($p0, $shade["Js"].toString($l9p0)));
                };
                return $l9;
            },
        };
    }
    $module.$member_Print_30 = $member_Print_30;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpUsR1Ie
(function($shade) {
    const $module = $shade["Js"] || ($shade["Js"] = {})
    $module.toString = toString;
    $module.concatString = concatString;
    $module.push = push;
    const $member_Concat_31 = {
        concat: function ($p0, $p1) {
            return concatString($p0, $p1);
        },
    };
    $module.$member_Concat_31 = $member_Concat_31;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpW0RGT2
(function($shade) {
    const $module = $shade["Main"] || ($shade["Main"] = {})
    function main() {
        var $p6 = [1, [0, [1, [1, [1, [2, [1, [3, [1, [4, [0, undefined]]]]]]]]]]];
        function $l13($l13p0) {
            return $shade["Data.Int"].$member_Mul_22.mul($l13p0, 2);
        };
        var $p14 = $member_Map_32.map($p6, $l13);
        return $shade["Js.Console"].log(...toArray($member_Foldable_33, $p14));
    }
    $module.main = main;
    $shade.$main = main
    function toArray($r0, $p0) {
        function $l12($l12p0) {
            return function($l12p1) {
                return $shade["Js"].push($l12p0, $l12p1);
            };
        };
        return $r0.foldl($p0, new Array(), $l12);
    }
    $module.toArray = toArray;
    const $member_Map_32 = {
        map: function ($p0, $p1) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = [1, [$p1($p0[1][0]), $member_Map_32.map($p0[1][1], $p1)]];
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
    $module.$member_Map_32 = $member_Map_32;
    const $member_Foldable_33 = {
        foldl: function ($p0, $p1, $p2) {
            var $e0;
            $l0: do {
                if ($p0[0] == 1) {
                    $e0 = $member_Foldable_33.foldl($p0[1][1], $p2($p1)($p0[1][0]), $p2);
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
                    $e0 = $p2($member_Foldable_33.foldr($p0[1][1], $p1, $p2))($p0[1][0]);
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
    $module.$member_Foldable_33 = $member_Foldable_33;
})(this.$shade || (this.$shade = {}));
$shade.$main();
