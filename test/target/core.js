(function($shade) {
    const $module = $shade["core/ops"] || ($shade["core/ops"] = {})
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/error"] || ($shade["core/error"] = {})
    function unwrap_unsafe($0) {
        return unwrap($0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/data/int"] || ($shade["core/data/int"] = {})
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core"] || ($shade["core"] = {})
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/fmt"] || ($shade["core/fmt"] = {})
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/data/option"] || ($shade["core/data/option"] = {})
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/data"] || ($shade["core/data"] = {})
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/io"] || ($shade["core/io"] = {})
    function print($1) {
        write(undefined, $1);
        return undefined;
    }
    $module.print = print;
    function println($1) {
        print($1);
        return print("\n");
    }
    $module.println = println;
    function eprint($1) {
        write(undefined, $1);
        return undefined;
    }
    $module.eprint = eprint;
    function eprintln($1) {
        eprint($1);
        return eprint("\n");
    }
    $module.eprintln = eprintln;
    function print_i32($0) {
        if ($0 >= 10) {
            $shade["core/ops"].div(print_i32($0), 10)
        };
        var $13 = $shade["core/ops"].rem($0, 10);
        var $16 = $shade["core/ops"].add($13, 48);
        var $20 = undefined;
        var $28 = undefined($20, 4);
        return print($28);
    }
    $module.print_i32 = print_i32;
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/data/result"] || ($shade["core/data/result"] = {})
    function ok($0) {
        return $0;
    }
    $module.ok = ok;
    function err($0) {
        return $0;
    }
    $module.err = err;
    function unwrap_err($0) {
        return $0;
    }
    $module.unwrap_err = unwrap_err;
})($shade || ($shade = {}));
(function($shade) {
    const $module = $shade["core/data/char"] || ($shade["core/data/char"] = {})
})($shade || ($shade = {}));
