"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.PORT = void 0;
var dotenv_1 = __importDefault(require("dotenv"));
var path_1 = __importDefault(require("path"));
var t = __importStar(require("io-ts"));
var pipeable_1 = require("fp-ts/lib/pipeable");
var Either_1 = require("fp-ts/lib/Either");
var function_1 = require("fp-ts/lib/function");
dotenv_1.default.config({
    path: process.env.NODE_ENV === "production"
        ? path_1.default.join(__dirname, "..", "..", ".env")
        : path_1.default.join(__dirname, "..", "..", ".env.development"),
});
exports.PORT = pipeable_1.pipe(t.string.decode(process.env.PORT), Either_1.fold(function () {
    throw new Error("no port found!!");
}, function_1.identity));
