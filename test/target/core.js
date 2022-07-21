// /mnt/e/Language/fc/test/target/prim.js
// /tmp/.tmpggLWuh
(function($shade) {
    const $module = $shade["prim"] || ($shade["prim"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpOelqYI
(function($shade) {
    const $module = $shade["intrinsics"] || ($shade["intrinsics"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpHhdhO5
(function($shade) {
    const $module = $shade["core/error"] || ($shade["core/error"] = {})
    function unwrap_unsafe($p0) {
        return unwrap($p0);
    }
    $module.unwrap_unsafe = unwrap_unsafe;
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpsfsh1G
(function($shade) {
    const $module = $shade["core/data/int"] || ($shade["core/data/int"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp5nviuD
(function($shade) {
    const $module = $shade["core"] || ($shade["core"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpaU5E6V
(function($shade) {
    const $module = $shade["core/ops"] || ($shade["core/ops"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmp8EVh93
(function($shade) {
    const $module = $shade["core/data/option"] || ($shade["core/data/option"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpXjTOfX
(function($shade) {
    const $module = $shade["core/data"] || ($shade["core/data"] = {})
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpYbfCKG
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
})(this.$shade || (this.$shade = {}));
// /tmp/.tmpmUiFje
(function($shade) {
    const $module = $shade["core/data/char"] || ($shade["core/data/char"] = {})
})(this.$shade || (this.$shade = {}));
