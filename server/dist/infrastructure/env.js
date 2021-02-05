"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.initializeEnv = void 0;
var initializeEnv = function (pool) { return function (req, _, next) {
    req.env = { pool: pool };
    next();
}; };
exports.initializeEnv = initializeEnv;
