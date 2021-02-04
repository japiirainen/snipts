"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.PORT = void 0;
var dotenv_1 = __importDefault(require("dotenv"));
var path_1 = __importDefault(require("path"));
var io_ts_1 = require("io-ts");
dotenv_1.default.config({
    path: process.env.NODE_ENV === "production"
        ? path_1.default.join(__dirname, "..", "..", ".env")
        : path_1.default.join(__dirname, "..", "..", ".env.development"),
});
console.log(process.env.NODE_ENV);
exports.PORT = io_ts_1.string.decode(process.env.PORT);
