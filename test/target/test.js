// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpRHiYCB
(function($shade) {
    const $module = $shade["Prim"] || ($shade["Prim"] = {})
    const $member_Termination_17 = {
        report: function ($p0) {
            return 0;
        },
    };
    $module.$member_Termination_17 = $member_Termination_17;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpYsxcPb
(function($shade) {
    const $module = $shade["Intrinsics"] || ($shade["Intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpeaOrWC
(function($shade) {
    const $module = $shade["Core.Error"] || ($shade["Core.Error"] = {})
    function unwrap_unsafe($r0, $p0) {
        return $r0.unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpvEmZ51
(function($shade) {
    const $module = $shade["Core.Data.List"] || ($shade["Core.Data.List"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpNp5XgK
(function($shade) {
    const $module = $shade["Core.Data.Bool"] || ($shade["Core.Data.Bool"] = {})
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
// /tmp/.tmp7kJEoN
// /tmp/.tmp8S0i7N
(function($shade) {
    const $module = $shade["Core.Ops"] || ($shade["Core.Ops"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpd1jMTE
(function($shade) {
    const $module = $shade["Core.Data.Option"] || ($shade["Core.Data.Option"] = {})
    const $member_Try_18 = {
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
    $module.$member_Try_18 = $member_Try_18;
    const $member_Unwrap_19 = {
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
    $module.$member_Unwrap_19 = $member_Unwrap_19;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp6fuA9D
// /tmp/.tmptyPtfw
(function($shade) {
    const $module = $shade["Core.Cmp"] || ($shade["Core.Cmp"] = {})
    function ne($p0, $p1) {
        return $shade["Core.Data.Bool"].not($shade["Core.Data.Int"].$member_Eq_27.eq($p0, $p1));
    }
    $module.ne = ne;
    function lt($p0, $p1) {
        var $e4;
        $l4: do {
            if ($shade["Core.Data.Int"].$member_Ord_28.cmp($p0, $p1)[0] == 0) {
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
            if ($shade["Core.Data.Int"].$member_Ord_28.cmp($p0, $p1)[0] == 2) {
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
            if ($shade["Core.Data.Int"].$member_Ord_28.cmp($p0, $p1)[0] == 2) {
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
            if ($shade["Core.Data.Int"].$member_Ord_28.cmp($p0, $p1)[0] == 0) {
                $e4 = [0, undefined];
                break $l4
            };
            $e4 = [1, undefined];
        } while(0);
        return $e4;
    }
    $module.ge = ge;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmphNZhfc
(function($shade) {
    const $module = $shade["Core.Data.Result"] || ($shade["Core.Data.Result"] = {})
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
    const $member_Try_20 = {
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
    $module.$member_Try_20 = $member_Try_20;
    const $member_Unwrap_21 = {
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
    $module.$member_Unwrap_21 = $member_Unwrap_21;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpS1zbxF
(function($shade) {
    const $module = $shade["Core.Data.Int"] || ($shade["Core.Data.Int"] = {})
    const $member_Add_22 = {
        add: function ($p0, $p1) {
            return $p0 + $p1;
        },
    };
    $module.$member_Add_22 = $member_Add_22;
    const $member_Sub_23 = {
        sub: function ($p0, $p1) {
            return $p0 - $p1;
        },
    };
    $module.$member_Sub_23 = $member_Sub_23;
    const $member_Mul_24 = {
        mul: function ($p0, $p1) {
            return $p0 * $p1;
        },
    };
    $module.$member_Mul_24 = $member_Mul_24;
    const $member_Div_25 = {
        div: function ($p0, $p1) {
            return $p0 / $p1;
        },
    };
    $module.$member_Div_25 = $member_Div_25;
    const $member_Rem_26 = {
        rem: function ($p0, $p1) {
            return $p0 % $p1;
        },
    };
    $module.$member_Rem_26 = $member_Rem_26;
    const $member_Eq_27 = {
        eq: function ($p0, $p1) {
            return $p0 == $p1;
        },
    };
    $module.$member_Eq_27 = $member_Eq_27;
    const $member_Ord_28 = {
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
    $module.$member_Ord_28 = $member_Ord_28;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpUhcoD5
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
// /tmp/.tmpSQXVqo
(function($shade) {
    const $module = $shade["Js"] || ($shade["Js"] = {})
    $module.toString = toString;
    $module.concatString = concatString;
    $module.push = push;
    const $member_Concat_29 = {
        concat: function ($p0, $p1) {
            return concatString($p0, $p1);
        },
    };
    $module.$member_Concat_29 = $member_Concat_29;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpwg7Yok
(function($shade) {
    const $module = $shade["Js.Console"] || ($shade["Js.Console"] = {})
    $module.log = console.log;
    function print($r0, ) {
        return $r0.print$(new Array());
    }
    $module.print = print;
    const $member_Print_30 = {
        print$: function ($p0) {
            return console.log(...$p0);
        },
    };
    $module.$member_Print_30 = $member_Print_30;
    function $member_Print_31($r0) {
        return {
            print$: function ($p0) {
                function $l9($l9p0) {
                    return $r0.print$($shade["Js"].push($p0, $shade["Js"].toString($l9p0)));
                };
                return $l9;
            },
        };
    }
    $module.$member_Print_31 = $member_Print_31;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpGXXctx
(function($shade) {
    const $module = $shade["Main"] || ($shade["Main"] = {})
    function main() {
        var $p6 = [1, [0, [1, [1, [1, [2, [1, [3, [1, [4, [0, undefined]]]]]]]]]]];
        return printList($p6);
    }
    $module.main = main;
    $shade.$main = main
    function printList($p0) {
        function $l16($l16p0) {
            return function($l16p1) {
                var $e0;
                $l0: do {
                    if ($l16p0[0] == 1) {
                        $e0 = $l16($l16p0[1][1])($shade["Js"].push($l16p1, $l16p0[1][0]));
                        break $l0
                    };
                    if ($l16p0[0] == 0) {
                        $e0 = $shade["Js.Console"].log(...$l16p1);
                        break $l0
                    };
                } while(0);
                return $e0;
            };
        };
        return $l16($p0)(new Array());
    }
    $module.printList = printList;
})(this.$shade || (this.$shade = {}));
$shade.$main();
