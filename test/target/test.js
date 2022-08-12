// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpXkBdSB
// /tmp/.tmpdjyEx8
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
// /mnt/e/Language/fc/test/target/core.js
// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpEIRblY
// /tmp/.tmpDJt0Cb
const $member_Default_18 = {
    default: function() {
        return "";
    },
};
// /tmp/.tmpVUyrmR
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
    bind: function(param0, param1) {
        if (param0 instanceof Data_Option_Some) {
            return param1(param0.field0);
        } else if (param0 instanceof Data_Option_None) {
            return new Data_Option_None();
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Unwrap_21 = {
    unwrap: function(param0) {
        if (param0 instanceof Data_Option_Some) {
            return param0.field0;
        } else if (param0 instanceof Data_Option_None) {
            throw "cannot unwrap a none value";
        } else {
            throw "failed pattern match";
        };
    },
    unwrap_or: function(param0, param1) {
        if (param0 instanceof Data_Option_Some) {
            return param0.field0;
        } else if (param0 instanceof Data_Option_None) {
            return param1;
        } else {
            throw "failed pattern match";
        };
    },
};
// /tmp/.tmptlaDzk
// /tmp/.tmpwP9nuh
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
    return Data_Bool_not($member_Eq_30.eq(param0, param1));
};
Core_Cmp_lt = function(param0, param1) {
    if ($member_Ord_31.cmp(param0, param1) instanceof Core_Cmp_Lt) {
        return new Prim_True();
    } else {
        return new Prim_False();
    };
};
Core_Cmp_le = function(param0, param1) {
    if ($member_Ord_31.cmp(param0, param1) instanceof Core_Cmp_Gt) {
        return new Prim_False();
    } else {
        return new Prim_True();
    };
};
Core_Cmp_gt = function(param0, param1) {
    if ($member_Ord_31.cmp(param0, param1) instanceof Core_Cmp_Gt) {
        return new Prim_True();
    } else {
        return new Prim_False();
    };
};
Core_Cmp_ge = function(param0, param1) {
    if ($member_Ord_31.cmp(param0, param1) instanceof Core_Cmp_Lt) {
        return new Prim_False();
    } else {
        return new Prim_True();
    };
};
// /tmp/.tmp7P2iAR
var Core_Error_unwrap_unsafe;
Core_Error_unwrap_unsafe = function(record0, param0) {
    return record0.unwrap(param0);
};
// /tmp/.tmpIQ9rGN
function $member_Default_22(record0, record1) {
    return {
        default: function() {
            return new Prim_Pair(record0.default(), record1.default());
        },
    };
}
// /tmp/.tmp6ChsZ5
const $member_Default_24 = {
    default: function() {
        return 0;
    },
};
const $member_Add_25 = {
    add: function(param0, param1) {
        return param0 + param1;
    },
};
const $member_Sub_26 = {
    sub: function(param0, param1) {
        return param0 - param1;
    },
};
const $member_Mul_27 = {
    mul: function(param0, param1) {
        return param0 * param1;
    },
};
const $member_Div_28 = {
    div: function(param0, param1) {
        return param0 / param1;
    },
};
const $member_Rem_29 = {
    rem: function(param0, param1) {
        return param0 % param1;
    },
};
const $member_Eq_30 = {
    eq: function(param0, param1) {
        return param0 == param1;
    },
};
const $member_Ord_31 = {
    cmp: function(param0, param1) {
        if (param0 == param1 ? 1 : param0 < param1 ? -1 : 1 == 0) {
            return new Core_Cmp_Eq();
        } else if (param0 == param1 ? 1 : param0 < param1 ? -1 : 1 == 1) {
            return new Core_Cmp_Gt();
        } else {
            return new Core_Cmp_Lt();
        };
    },
};
// /tmp/.tmp3UhBIr
// /tmp/.tmpnf0sE7
var Core_Foldable_foldMap;
Core_Foldable_foldMap = function(record0, record1, record2, param0) {
    return record0.foldr(param0, record1.default(), record2.concat);
};
// /tmp/.tmp1ZRKw2
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
const $member_Try_32 = {
    ret: function(param0) {
        return new Data_Result_Ok(param0);
    },
    bind: function(param10, param11) {
        if (new Prim_Pair(param10, param11).field0 instanceof Data_Result_Ok) {
            return new Prim_Pair(param10, param11).field1(new Prim_Pair(param10, param11).field0.field0);
        } else if (new Prim_Pair(param10, param11).field0 instanceof Data_Result_Error) {
            return new Data_Result_Error(new Prim_Pair(param10, param11).field0.field0);
        } else {
            throw "failed pattern match";
        };
    },
};
const $member_Unwrap_33 = {
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
        if (new Prim_Pair(param10, param11).field0 instanceof Data_Result_Ok) {
            return new Prim_Pair(param10, param11).field0.field0;
        } else if (new Prim_Pair(param10, param11).field0 instanceof Data_Result_Error) {
            return new Prim_Pair(param10, param11).field1;
        } else {
            throw "failed pattern match";
        };
    },
};
// /tmp/.tmpbSWoVW
var Data_List_Nil;
var Data_List_Cons;
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
const $member_Default_34 = {
    default: function() {
        return new Data_List_Nil();
    },
};
const $member_Foldable_35 = {
    foldl: function(param0, param1, param2) {
        if (param0 instanceof Data_List_Cons) {
            return $member_Foldable_35.foldl(param0.field1, param2(param1)(param0.field0), param2);
        } else if (param0 instanceof Data_List_Nil) {
            return param1;
        } else {
            throw "failed pattern match";
        };
    },
    foldr: function(param0, param1, param2) {
        if (param0 instanceof Data_List_Cons) {
            return param2($member_Foldable_35.foldr(param0.field1, param1, param2))(param0.field0);
        } else if (param0 instanceof Data_List_Nil) {
            return param1;
        } else {
            throw "failed pattern match";
        };
    },
};
// /tmp/.tmpk2c2zX
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
const $member_Default_36 = {
    default: function() {
        return new Prim_False();
    },
};
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
// /tmp/.tmpobI4JR
var Js_Console_print;
Js_Console_print = function(record0, ) {
    return record0.print$(new Array());
};
const $member_Print_37 = {
    print$: function(param0) {
        return console.log(...param0);
    },
};
function $member_Print_38(record0) {
    return {
        print$: function(param0) {
            function lambda7(lambda7_param0) {
                return record0.print$(push(param0, lambda7_param0));
            };
            return lambda7;
        },
    };
}
// /tmp/.tmpbpJSPs
var Js_DOM_createElement;
var Js_DOM_Sym;
Js_DOM_createElement = function(type0, param0) {
    return document.createElement(type0);
};
Js_DOM_Sym = (function() {
    class Sym {
        constructor() {
        }
    }
    return Sym;
})();
// /tmp/.tmpiGuyum
var Js_toArray;
Js_toArray = function(record0, param0) {
    function lambda12(lambda12_param0) {
        return function(lambda12_param1) {
            return push(lambda12_param0, lambda12_param1);
        };
    };
    return record0.foldl(param0, new Array(), lambda12);
};
const $member_Concat_40 = {
    concat: function(param0, param1) {
        return concatString(param0, param1);
    },
};
// /tmp/.tmpdD8cw6
var Main_main;
var Main_test;
Main_main = function() {
    var $p1 = $member_Default_22($member_Default_24, $member_Default_22($member_Default_19, $member_Default_18)).default();
    var $p4 = $member_Add_25.add(undefined, 1);
    return Js_Console_print($member_Print_38($member_Print_38($member_Print_38($member_Print_38($member_Print_37)))))($p1.field0)($p1.field1.field0)($p1.field1.field1)($p4);
};
const $main = Main_main
Main_test = function(param0) {
    var $p3 = param0 instanceof Prim_True ? 0 : 1;
    return $member_Add_25.add($p3, 1);
};
$main();
