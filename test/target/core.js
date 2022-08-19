// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmp8Z0WUc
var Prim_Unit;
var Prim_False;
var Prim_True;
var Prim_Proxy;
var Prim_Pair;
Prim_Unit = (function() {
    class Unit {
        constructor() {
        }
    }
    return Unit;
})();
Prim_False = (function() {
    class False {
        constructor() {
        }
    }
    return False;
})();
Prim_True = (function() {
    class True {
        constructor() {
        }
    }
    return True;
})();
Prim_Proxy = (function() {
    class Proxy {
        constructor() {
        }
    }
    return Proxy;
})();
Prim_Pair = (function() {
    class Pair {
        constructor(field0, field1) {
            this.field0 = field0;
            this.field1 = field1;
        }
    }
    return Pair;
})();
const $member_Termination_17 = {
    report: function(param0) {
        return 0;
    },
};
// /tmp/.tmpOzh75R
// /tmp/.tmpRut230
// /tmp/.tmpydJ0DX
const $member_Default_18 = {
    default: function() {
        return "";
    },
};
// /tmp/.tmp03Gi6h
var Data_Option_None;
var Data_Option_Some;
Data_Option_None = (function() {
    class None {
        constructor() {
        }
    }
    return None;
})();
Data_Option_Some = (function() {
    class Some {
        constructor(field0) {
            this.field0 = field0;
        }
    }
    return Some;
})();
const $member_Default_19 = {
    default: function() {
        return new Data_Option_None();
    },
};
const $member_Try_20 = {
    ret: function(param0) {
        return new Data_Option_Some(param0);
    },
    bind: function(param8, param9) {
        var $8 = new Prim_Pair(param8, param9);
        if ($8.field0 instanceof Data_Option_Some) {
            return $8.field1($8.field0.field0);
        } else if ($8.field0 instanceof Data_Option_None) {
            return new Data_Option_None();
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Unwrap_21 = {
    unwrap: function(param4) {
        if (param4 instanceof Data_Option_Some) {
            return param4.field0;
        } else if (param4 instanceof Data_Option_None) {
            throw "cannot unwrap a none value";
        } else {
            throw "failed pattern match";
        };
    },
    unwrap_or: function(param8, param9) {
        var $6 = new Prim_Pair(param8, param9);
        if ($6.field0 instanceof Data_Option_Some) {
            return $6.field0.field0;
        } else if ($6.field0 instanceof Data_Option_None) {
            return $6.field1;
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Mappable_22 = {
    map: function(param8, param9) {
        var $10 = new Prim_Pair(param8, param9);
        if ($10.field0 instanceof Data_Option_Some) {
            return new Data_Option_Some($10.field1($10.field0.field0));
        } else if ($10.field0 instanceof Data_Option_None) {
            return new Data_Option_None();
        } else {
            throw "failed pattern match";
        };
    },
};
// /tmp/.tmp4bsLe2
// /tmp/.tmp3R6dEH
var Core_Cmp_Lt;
var Core_Cmp_Eq;
var Core_Cmp_Gt;
var Core_Cmp_ne;
var Core_Cmp_lt;
var Core_Cmp_le;
var Core_Cmp_gt;
var Core_Cmp_ge;
Core_Cmp_Lt = (function() {
    class Lt {
        constructor() {
        }
    }
    return Lt;
})();
Core_Cmp_Eq = (function() {
    class Eq {
        constructor() {
        }
    }
    return Eq;
})();
Core_Cmp_Gt = (function() {
    class Gt {
        constructor() {
        }
    }
    return Gt;
})();
Core_Cmp_ne = function(param0, param1) {
    return Data_Bool_not($member_Eq_31.eq(param0, param1));
};
Core_Cmp_lt = function(param0, param1) {
    var $4 = $member_Ord_32.cmp(param0, param1);
    if ($4 instanceof Core_Cmp_Lt) {
        return new Prim_True();
    } else {
        return new Prim_False();
    };
};
Core_Cmp_le = function(param0, param1) {
    var $4 = $member_Ord_32.cmp(param0, param1);
    if ($4 instanceof Core_Cmp_Gt) {
        return new Prim_False();
    } else {
        return new Prim_True();
    };
};
Core_Cmp_gt = function(param0, param1) {
    var $4 = $member_Ord_32.cmp(param0, param1);
    if ($4 instanceof Core_Cmp_Gt) {
        return new Prim_True();
    } else {
        return new Prim_False();
    };
};
Core_Cmp_ge = function(param0, param1) {
    var $4 = $member_Ord_32.cmp(param0, param1);
    if ($4 instanceof Core_Cmp_Lt) {
        return new Prim_False();
    } else {
        return new Prim_True();
    };
};
const $member_Eq_23 = {
    eq: function(param12, param13) {
        var $10 = new Prim_Pair(param12, param13);
        if ($10.field0 instanceof Core_Cmp_Lt && $10.field1 instanceof Core_Cmp_Lt) {
            return new Prim_True();
        } else if ($10.field0 instanceof Core_Cmp_Eq && $10.field1 instanceof Core_Cmp_Eq) {
            return new Prim_True();
        } else if ($10.field0 instanceof Core_Cmp_Gt && $10.field1 instanceof Core_Cmp_Gt) {
            return new Prim_True();
        } else {
            return new Prim_False();
        };
    },
};
// /tmp/.tmp2pCeJI
// /tmp/.tmpwQuK8Q
var Core_Error_unwrap_unsafe;
Core_Error_unwrap_unsafe = function(record0, ) {
    return function (param0) {
        return record0.unwrap(param0);
    };
};
// /tmp/.tmpGNbsJn
function $member_Default_24(record0, record1) {
    return {
        default: function() {
            return new Prim_Pair(record0.default, record1.default);
        },
    };
}
// /tmp/.tmpYieoee
const $member_Default_25 = {
    default: function() {
        return 0;
    },
};
const $member_Add_26 = {
    add: function(param0, param1) {
        return param0 + param1;
    },
};
const $member_Sub_27 = {
    sub: function(param0, param1) {
        return param0 - param1;
    },
};
const $member_Mul_28 = {
    mul: function(param0, param1) {
        return param0 * param1;
    },
};
const $member_Div_29 = {
    div: function(param0, param1) {
        return param0 / param1;
    },
};
const $member_Rem_30 = {
    rem: function(param0, param1) {
        return param0 % param1;
    },
};
const $member_Eq_31 = {
    eq: function(param0, param1) {
        return param0 == param1;
    },
};
const $member_Ord_32 = {
    cmp: function(param0, param1) {
        var $4 = param0 == param1 ? 1 : param0 < param1 ? -1 : 1;
        if ($4 == 0) {
            return new Core_Cmp_Eq();
        } else if ($4 == 1) {
            return new Core_Cmp_Gt();
        } else {
            return new Core_Cmp_Lt();
        };
    },
};
// /tmp/.tmpEgAqMS
// /tmp/.tmpS52a3M
var Core_Foldable_foldMap;
Core_Foldable_foldMap = function(record0, record1, record2, ) {
    return function (param0) {
        return record0.foldr(param0, record1.default, (_0 => _1 => record2.concat(_0, _1)));
    };
};
// /tmp/.tmpF1CaJ8
var Data_Result_Error;
var Data_Result_Ok;
var Data_Result_ok;
var Data_Result_err;
var Data_Result_unwrap_err;
Data_Result_Error = (function() {
    class Error {
        constructor(field0) {
            this.field0 = field0;
        }
    }
    return Error;
})();
Data_Result_Ok = (function() {
    class Ok {
        constructor(field0) {
            this.field0 = field0;
        }
    }
    return Ok;
})();
Data_Result_ok = function(param6) {
    if (param6 instanceof Data_Result_Ok) {
        return new Data_Option_Some(param6.field0);
    } else if (param6 instanceof Data_Result_Error) {
        return new Data_Option_None();
    } else {
        throw "failed pattern match";
    };
};
Data_Result_err = function(param6) {
    if (param6 instanceof Data_Result_Error) {
        return new Data_Option_Some(param6.field0);
    } else if (param6 instanceof Data_Result_Ok) {
        return new Data_Option_None();
    } else {
        throw "failed pattern match";
    };
};
Data_Result_unwrap_err = function(param6) {
    if (param6 instanceof Data_Result_Error) {
        return param6.field0;
    } else if (param6 instanceof Data_Result_Ok) {
        throw "cannot unwrap_err an ok value";
    } else {
        throw "failed pattern match";
    };
};
const $member_Try_33 = {
    ret: function(param0) {
        return new Data_Result_Ok(param0);
    },
    bind: function(param10, param11) {
        var $10 = new Prim_Pair(param10, param11);
        if ($10.field0 instanceof Data_Result_Ok) {
            return $10.field1($10.field0.field0);
        } else if ($10.field0 instanceof Data_Result_Error) {
            return new Data_Result_Error($10.field0.field0);
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Unwrap_34 = {
    unwrap: function(param6) {
        if (param6 instanceof Data_Result_Ok) {
            return param6.field0;
        } else if (param6 instanceof Data_Result_Error) {
            throw "cannot unwrap an err value";
        } else {
            throw "failed pattern match";
        };
    },
    unwrap_or: function(param10, param11) {
        var $6 = new Prim_Pair(param10, param11);
        if ($6.field0 instanceof Data_Result_Ok) {
            return $6.field0.field0;
        } else if ($6.field0 instanceof Data_Result_Error) {
            return $6.field1;
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Mappable_35 = {
    map: function(param10, param11) {
        var $12 = new Prim_Pair(param10, param11);
        if ($12.field0 instanceof Data_Result_Ok) {
            return new Data_Result_Ok($12.field1($12.field0.field0));
        } else if ($12.field0 instanceof Data_Result_Error) {
            return new Data_Result_Error($12.field0.field0);
        } else {
            throw "failed pattern match";
        };
    },
};
// /tmp/.tmpG28kuL
var Data_List_Nil;
var Data_List_Cons;
var Data_List_length;
var Data_List_first;
var Data_List_last;
var Data_List_reverse;
var Data_List_sort;
var Data_List_sortByKey;
var Data_List_sortBy;
var Data_List_descending;
var Data_List_ascending;
var Data_List_sequences;
Data_List_Nil = (function() {
    class Nil {
        constructor() {
        }
    }
    return Nil;
})();
Data_List_Cons = (function() {
    class Cons {
        constructor(field0, field1) {
            this.field0 = field0;
            this.field1 = field1;
        }
    }
    return Cons;
})();
Data_List_length = function(param4) {
    if (param4 instanceof Data_List_Cons) {
        return $member_Add_26.add(1, Data_List_length(param4.field1));
    } else if (param4 instanceof Data_List_Nil) {
        return 0;
    } else {
        throw "failed pattern match";
    };
};
Data_List_first = function(param4) {
    if (param4 instanceof Data_List_Cons) {
        return new Data_Option_Some(param4.field0);
    } else if (param4 instanceof Data_List_Nil) {
        return new Data_Option_None();
    } else {
        throw "failed pattern match";
    };
};
Data_List_last = function(param7) {
    if (param7 instanceof Data_List_Cons && param7.field1 instanceof Data_List_Nil) {
        return new Data_Option_Some(param7.field0);
    } else if (param7 instanceof Data_List_Cons) {
        return Data_List_last(param7.field1);
    } else if (param7 instanceof Data_List_Nil) {
        return new Data_Option_None();
    } else {
        throw "failed pattern match";
    };
};
Data_List_reverse = function(record0, ) {
    return function (param0) {
        function lambda8(lambda8_param0) {
            return function(lambda8_param1) {
                return new Data_List_Cons(lambda8_param1, lambda8_param0);
            };
        };
        return record0.foldl(param0, new Data_List_Nil(), lambda8);
    };
};
Data_List_sort = function(record0, ) {
    return function (param0) {
        function lambda8(lambda8_param0) {
            return function(lambda8_param1) {
                return record0.cmp(lambda8_param0, lambda8_param1);
            };
        };
        return Data_List_sortBy(param0, lambda8);
    };
};
Data_List_sortByKey = function(record0, ) {
    return function (param0, param1) {
        function lambda12(lambda12_param0) {
            return function(lambda12_param1) {
                return record0.cmp(param1(lambda12_param0), param1(lambda12_param1));
            };
        };
        return Data_List_sortBy(param0, lambda12);
    };
};
Data_List_sortBy = function(param0, param1) {
    function lambda27(lambda27_param0) {
        return function(lambda27_param1) {
            var $2 = new Prim_Pair(lambda27_param0, lambda27_param1);
            var $26;
            if ($2.field0 instanceof Data_List_Cons && $2.field1 instanceof Data_List_Cons) {
                $26 = $member_Eq_23.eq(param1($2.field0.field0)($2.field1.field0), new Core_Cmp_Gt()) instanceof Prim_True ? new Data_List_Cons($2.field1.field0, lambda27(lambda27_param0)($2.field1.field1)) : new Data_List_Cons($2.field0.field0, lambda27($2.field0.field1)(lambda27_param1));
            } else if ($2.field0 instanceof Data_List_Nil) {
                $26 = $2.field1;
            } else if ($2.field1 instanceof Data_List_Nil) {
                $26 = $2.field0;
            } else {
                throw "failed pattern match";
            };
            return $26;
        };
    };
    function lambda40(lambda40_param0) {
        return lambda40_param0 instanceof Data_List_Cons && lambda40_param0.field1 instanceof Data_List_Cons ? new Data_List_Cons(lambda27(lambda40_param0.field0)(lambda40_param0.field1.field0), lambda40(lambda40_param0.field1.field1)) : lambda40_param0;
    };
    function lambda49(lambda49_param0) {
        return lambda49_param0 instanceof Data_List_Cons && lambda49_param0.field1 instanceof Data_List_Nil ? lambda49_param0.field0 : lambda49(lambda40(lambda49_param0));
    };
    return lambda49(Data_List_sequences(param1)(param0));
};
Data_List_descending = function(param0) {
    function lambda29(lambda29_param0) {
        return function(lambda29_param1) {
            return function(lambda29_param2) {
                return lambda29_param2 instanceof Data_List_Cons && $member_Eq_23.eq(param0(lambda29_param0)(lambda29_param2.field0), new Core_Cmp_Gt()) instanceof Prim_True ? Data_List_descending(param0)(lambda29_param2.field0)(new Data_List_Cons(lambda29_param0, lambda29_param1))(lambda29_param2.field1) : new Data_List_Cons(new Data_List_Cons(lambda29_param0, lambda29_param1), Data_List_sequences(param0)(lambda29_param2));
            };
        };
    };
    return lambda29;
};
Data_List_ascending = function(param0) {
    function lambda34(lambda34_param0) {
        return function(lambda34_param1) {
            return function(lambda34_param2) {
                var $33;
                if (lambda34_param2 instanceof Data_List_Cons && Core_Cmp_ne(param0(lambda34_param0)(lambda34_param2.field0), new Core_Cmp_Gt()) instanceof Prim_True) {
                    function lambda18(lambda18_param0) {
                        return lambda34_param1(new Data_List_Cons(lambda34_param0, lambda18_param0));
                    };
                    $33 = Data_List_ascending(param0)(lambda34_param2.field0)(lambda18)(lambda34_param2.field1);
                } else {
                    $33 = new Data_List_Cons(lambda34_param1(new Data_List_Cons(lambda34_param0, new Data_List_Nil())), Data_List_sequences(param0)(lambda34_param2));
                };
                return $33;
            };
        };
    };
    return lambda34;
};
Data_List_sequences = function(param0) {
    function lambda35(lambda35_param0) {
        var $34;
        if (lambda35_param0 instanceof Data_List_Cons && lambda35_param0.field1 instanceof Data_List_Cons) {
            if ($member_Eq_23.eq(param0(lambda35_param0.field0)(lambda35_param0.field1.field0), new Core_Cmp_Gt()) instanceof Prim_True) {
                $34 = Data_List_descending(param0)(lambda35_param0.field1.field0)(new Data_List_Cons(lambda35_param0.field0, new Data_List_Nil()))(lambda35_param0.field1.field1);
            } else {
                function lambda27(lambda27_param0) {
                    return new Data_List_Cons(lambda35_param0.field0, lambda27_param0);
                };
                $34 = Data_List_ascending(param0)(lambda35_param0.field1.field0)(lambda27)(lambda35_param0.field1.field1);
            };
        } else {
            $34 = new Data_List_Cons(lambda35_param0, new Data_List_Nil());
        };
        return $34;
    };
    return lambda35;
};
const $member_Default_36 = {
    default: function() {
        return new Data_List_Nil();
    },
};
const $member_FromFoldable_37 = {
    fromFoldable: function(param0) {
        function lambda8(lambda8_param0) {
            return function(lambda8_param1) {
                return new Data_List_Cons(lambda8_param1, lambda8_param0);
            };
        };
        return record0.foldr(param0, new Data_List_Nil(), lambda8);
    },
};
const $member_Index_38 = {
    index: function(param13, param14) {
        var $18 = new Prim_Pair(param13, param14);
        if ($18.field0 instanceof Data_List_Cons && $18.field1 == 0) {
            return $18.field0.field0;
        } else if ($18.field0 instanceof Data_List_Cons) {
            return $member_Index_38.index($18.field0.field1, $member_Sub_27.sub($18.field1, 1));
        } else if ($18.field0 instanceof Data_List_Nil) {
            throw "index out of range";
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Foldable_39 = {
    foldl: function(param10, param11, param12) {
        var $17 = new Prim_Pair(param10, new Prim_Pair(param11, param12));
        if ($17.field0 instanceof Data_List_Cons) {
            return $member_Foldable_39.foldl($17.field0.field1, $17.field1.field1($17.field1.field0)($17.field0.field0), $17.field1.field1);
        } else if ($17.field0 instanceof Data_List_Nil) {
            return $17.field1.field0;
        } else {
            throw "failed pattern match";
        };
    },
    foldr: function(param10, param11, param12) {
        var $17 = new Prim_Pair(param10, new Prim_Pair(param11, param12));
        if ($17.field0 instanceof Data_List_Cons) {
            return $17.field1.field1($member_Foldable_39.foldr($17.field0.field1, $17.field1.field0, $17.field1.field1))($17.field0.field0);
        } else if ($17.field0 instanceof Data_List_Nil) {
            return $17.field1.field0;
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Mappable_40 = {
    map: function(param8, param9) {
        var $14 = new Prim_Pair(param8, param9);
        if ($14.field0 instanceof Data_List_Cons) {
            return new Data_List_Cons($14.field1($14.field0.field0), $member_Mappable_40.map($14.field0.field1, $14.field1));
        } else if ($14.field0 instanceof Data_List_Nil) {
            return new Data_List_Nil();
        } else {
            throw "failed pattern match";
        };
    },
};
// /tmp/.tmpo9TXyf
var Data_Bool_not;
Data_Bool_not = function(param2) {
    if (param2 instanceof Prim_True) {
        return new Prim_False();
    } else if (param2 instanceof Prim_False) {
        return new Prim_True();
    } else {
        throw "failed pattern match";
    };
};
const $member_Default_41 = {
    default: function() {
        return new Prim_False();
    },
};
