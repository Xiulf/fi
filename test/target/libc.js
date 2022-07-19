(function($shade) {
    const $module = $shade["libc"] || ($shade["libc"] = {})
    const STDIN_FILENO = 0;
    $module.STDIN_FILENO = STDIN_FILENO;
    const STDOUT_FILENO = 1;
    $module.STDOUT_FILENO = STDOUT_FILENO;
    const STDERR_FILENO = 2;
    $module.STDERR_FILENO = STDERR_FILENO;
    $module.puts = puts;
    $module.write = write;
})($shade || ($shade = {}));
