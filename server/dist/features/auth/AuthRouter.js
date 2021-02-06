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
Object.defineProperty(exports, "__esModule", { value: true });
exports.authRoutes = void 0;
var E = __importStar(require("fp-ts/Either"));
var function_1 = require("fp-ts/function");
var error_1 = require("../../infrastructure/error");
var registerService_1 = require("./registerService");
var user_1 = require("./user");
exports.authRoutes = {
    me: function (req, res) {
        res.json(user_1.toPublicUser(req.env.user));
    },
    register: function (req, res) {
        registerService_1.register(req.env, req.body)().then(function (e) {
            return function_1.pipe(e, E.fold(error_1.processError(res), function () { return res.status(200).json({ foo: 'bar' }); }));
        });
    },
};
