(function($shade) {
    const $module = $shade["main"] || ($shade["main"] = {})
    function main() {
        var $1 = "test";
        return $shade["core/io"].println($1);
    }
    $module.main = main;
})($shade || ($shade = {}));
