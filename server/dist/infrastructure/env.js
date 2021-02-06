"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.initializeEnv = void 0;
const initializeEnv = (pool) => (req, _, next) => {
    req.env = { pool };
    next();
};
exports.initializeEnv = initializeEnv;
