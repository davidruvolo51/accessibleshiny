////////////////////////////////////////////////////////////////////////////////
// FILE: webpack.common.js
// AUTHOR: David Ruvolo
// CREATED: 2020-09-28
// MODIFIED: 2020-09-28
// PURPOSE: configuration to be used in prod and dev
// DEPENDENCIES: see below
// STATUS: working
// COMMENTS: NA
////////////////////////////////////////////////////////////////////////////////

// load
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const webpack = require("webpack");
const path = require("path");

// configuration
module.exports = {
    entry: "./inst/accessibleshiny/src/index.js",
    output: {
        filename: "accessibleshiny.min.js",
        path: path.join(__dirname, "..", "inst/accessibleshiny/public"),
    }  ,
    plugins: [
        new webpack.ProgressPlugin(),
        new CleanWebpackPlugin(),
        new MiniCssExtractPlugin({
            filename: "accessibleshiny.min.css",
            ignoreOrder: false,
        })
    ],
    module: {
        rules: [
            {
                test: /\.js$/,
                use: "babel-loader",
                exclude: /node_modules/,
            },
            {
                test: /\.s[ac]ss$/i,
                use: [
                    // write to file
                    {
                        loader: MiniCssExtractPlugin.loader,
                        options: {
                            publicPath: "./inst/accessibleshiny/public/"
                        }
                    },
                    "css-loader",
                    "postcss-loader",
                    "sass-loader",
                ]
            }
        ]
    }
}
