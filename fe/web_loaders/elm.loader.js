/* globals require, module */


module.exports = function compile() {

    "use strict";

    var childProcess = require("child_process");
    var fs = require("fs");
    var path = require("path");
    var temp = require("temp");

    // Clear out the temp folder on teardown
    temp.track();

    this.cacheable();
    var callback = this.async();

    // Find a tempfile to compile into
    var tempfile = path.join(temp.dir, "out.js");

    // path to required file; Elm will compile this
    var theTarget = this.resourcePath;

    // options are listed in elmLoader
    var options = this.options.elmLoader || {};

    // the entire elm directory is a dependency
    var dep = path.resolve(options.dir || ".");
    this.addContextDependency(dep);

    // actually run the compilation
    var result = childProcess.spawnSync(
        "elm-make",
        [theTarget, "--yes", "--output", tempfile.replace(/ /g, "\\ ")],
        {
            env: process.env,
            stdio: "inherit"
        });

    if (result.status == 0) {
        callback(null, fs.readFileSync(tempfile, "utf8"))
    } else {
        callback(result.error);
    }

};
