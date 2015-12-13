/* globals require, module, __dirname */

var path = require("path");
var HtmlWebpackPlugin = require("html-webpack-plugin");
var ExtractTextPlugin = require("extract-text-webpack-plugin");

module.exports = {
    entry: "./app/index.js",
    output: {
        path: path.join(__dirname, "public"),
        filename: "app.js"
    },
    devtool: "source-map",
    module: {
        loaders: [
            {
                test: /\.elm$/,
                loaders: ["exports?Elm", "elm"]
            },
            {
                test: /\.scss$/,
                loader:
                    ExtractTextPlugin.extract(["css?sourceMap", "sass?sourceMap"])
            }
        ]
    },
    elmLoader: {
        dir: "./elm"
    },
    plugins: [
        new ExtractTextPlugin("styles.css"),
        new HtmlWebpackPlugin({
            title: "StudyTeam",
            hash: true,
            template: "./app/index.html",
            inject: "body"
        })
    ],
    devServer: {
        contentBase: "./public",
        historyApiFallback: true
    }
};
