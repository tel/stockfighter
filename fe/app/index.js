
var Elm = require("../elm/App.elm");
require("./styles/main.scss");

var App = {
    init: function init() {
        Elm.fullscreen(Elm.App);
    }
};

App.init();
