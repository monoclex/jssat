const path = require("path");
const HTMLWebpackPlugin = require("html-webpack-plugin");
const { DefinePlugin } = require("webpack");

const isDev = "WEBPACK_DEV_SERVER" in process.env;

module.exports = {
  mode: isDev ? "development" : "production",
  entry: "./src/index.tsx",
  output: {
    filename: "index.js",
    path: path.resolve(__dirname, "dist"),
  },
  resolve: { extensions: [".js", ".jsx", ".ts", ".tsx"] },
  plugins: [
    new HTMLWebpackPlugin({ title: "Domino the Debugger" }),
    new DefinePlugin({ "process.env.NODE_ENV": JSON.stringify(isDev) }),
  ],
  performance: {
    // this is going to exclusively be used on localhost
    hints: false,
  },
  devServer: {
    hot: true,
  },
  module: {
    rules: [
      {
        test: /\.(le|c)ss$/,
        use: ["style-loader", "css-loader", "less-loader"],
      },
      {
        test: /\.tsx?$/,
        exclude: /node_modules/,
        use: "babel-loader",
      },
    ],
  },
};
